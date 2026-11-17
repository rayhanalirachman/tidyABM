//! Spike S6 core (no R dependency): the tax model's whole year loop, as in
//! `oracle/bench/tax.R`, with the uniform generator passed in so the same code runs
//! on R's C RNG (main thread) or on the pure port (`rrng`, any thread).

/// Neighbours in CSR form: agent i's neighbours are `idx[off[i]..off[i+1]]` (0-based),
/// in the order the engine's lattice lists them.
pub struct Csr<'a> {
    pub off: &'a [i32],
    pub idx: &'a [i32],
}

/// type: 1 honest, 2 dishonest, 3 imitating. Returns `mean_gross_tax_rate` for ticks 1..=T.
pub fn tax_run(
    typ: &[i32],
    w: &[f64],
    k: &[f64],
    nb: &Csr,
    rate: f64,
    ticks: usize,
    mut unif: impl FnMut() -> f64,
) -> Vec<f64> {
    let n = w.len();
    let (tax, pen) = (0.30, 0.45);
    let mut q = vec![0.02; n];
    let mut share: Vec<f64> = typ
        .iter()
        .map(|&t| if t == 1 { 1.0 } else { 0.0 })
        .collect();
    let mut x = vec![0.0; n];
    let mut owed = vec![0.0; n];
    let mut ns = vec![0.0; n];
    let mut out = Vec::with_capacity(ticks);
    for _ in 0..ticks {
        // abm_neighbours(neighbours_share ~ mean(declared_share)): R's two-pass mean()
        for (i, nsi) in ns.iter_mut().enumerate() {
            let b = &nb.idx[nb.off[i] as usize..nb.off[i + 1] as usize];
            let d = b.len() as f64;
            let mut s = 0.0;
            for &j in b {
                s += share[j as usize];
            }
            s /= d;
            let mut r = 0.0;
            for &j in b {
                r += share[j as usize] - s;
            }
            *nsi = s + r / d;
        }
        // the declared ~ case_when(...) rule
        for i in 0..n {
            x[i] = if owed[i] > 0.0 {
                owed[i]
            } else if typ[i] == 1 {
                w[i]
            } else if typ[i] == 2 {
                if q[i] > tax / pen {
                    w[i]
                } else if q[i] < tax / (tax + (pen - tax) * (k[i] * pen * w[i]).exp()) {
                    0.0
                } else {
                    w[i].min(
                        (w[i] - ((1.0 - q[i]) * tax / (q[i] * (pen - tax))).ln() / (k[i] * pen))
                            .max(0.0),
                    )
                }
            } else {
                ns[i] * w[i]
            };
        }
        // audited ~ declared < income - 1e-9 & runif(n()) < audit_rate, then the
        // three rules that read it (start-of-step values of q are used per agent)
        for i in 0..n {
            let u = unif(); // runif(n()) draws for every agent, in order
            let aud = x[i] < w[i] - 1e-9 && u < rate;
            owed[i] = if aud {
                tax * (w[i] - x[i]) * (1.0 + pen * w[i])
            } else {
                0.0
            };
            if aud {
                q[i] = 1.0;
            } else if q[i] > rate {
                q[i] = if q[i] - 0.2 < 0.0 { rate } else { q[i] - 0.2 };
            }
            share[i] = x[i] / w[i];
        }
        // two accumulators, each left to right, as the engine's sum()s
        let sx: f64 = x.iter().fold(0.0, |a, v| a + v);
        let sw: f64 = w.iter().fold(0.0, |a, v| a + v);
        out.push(tax * sx / sw);
    }
    out
}
