//! usage: dump <gen> <seed> <count> <n> <outfile>
//! Writes the draws (f64 for unif/norm/exp, i32 otherwise, little endian) followed by
//! the 626-integer `.Random.seed`, and prints ns per draw. See tests/compare.R for the
//! R expression each <gen> mirrors.
use rrng::RRng;
use std::io::Write;
use std::time::Instant;

fn weights(n: usize) -> Vec<f64> {
    // heavy ties on purpose: 13 distinct values
    (1..=n).map(|i| ((i * 7919) % 13 + 1) as f64).collect()
}

fn main() {
    let a: Vec<String> = std::env::args().collect();
    assert!(
        a.len() == 6,
        "usage: dump <gen> <seed> <count> <n> <outfile>"
    );
    let (gen, seed, count, n) = (
        a[1].as_str(),
        a[2].parse::<i32>().unwrap(),
        a[3].parse::<usize>().unwrap(),
        a[4].parse::<usize>().unwrap(),
    );
    let mut g = RRng::set_seed(seed);
    let t = Instant::now();
    let mut f: Vec<f64> = Vec::new();
    let mut i: Vec<i32> = Vec::new();
    let k = n.min(10);
    match gen {
        "unif" => f = (0..count).map(|_| g.unif_rand()).collect(),
        "norm" => f = (0..count).map(|_| g.norm_rand()).collect(),
        "exp" => f = (0..count).map(|_| g.exp_rand()).collect(),
        "qnorm" => f = (0..count).map(|_| rrng::qnorm_std(g.unif_rand())).collect(),
        "idx" => i = g.sample_replace(n, count),
        "fy" => (0..count).for_each(|_| i.extend(g.sample_noreplace(n, k))),
        "sample2" => i = g.sample2(2_147_483_647.0, count),
        "probr" => i = g.sample_prob(&weights(n), count, true).unwrap(),
        "probnr" => {
            let w = weights(n);
            (0..count).for_each(|_| i.extend(g.sample_prob(&w, k, false).unwrap()))
        }
        _ => panic!("unknown generator {gen}"),
    }
    let ns = t.elapsed().as_nanos() as f64 / count.max(1) as f64;
    let mut out = std::fs::File::create(&a[5]).unwrap();
    let mut buf: Vec<u8> = Vec::new();
    f.iter()
        .for_each(|x| buf.extend_from_slice(&x.to_le_bytes()));
    i.iter()
        .for_each(|x| buf.extend_from_slice(&x.to_le_bytes()));
    g.random_seed()
        .iter()
        .for_each(|x| buf.extend_from_slice(&x.to_le_bytes()));
    out.write_all(&buf).unwrap();
    println!("{gen} seed={seed} count={count} n={n}: {ns:.2} ns per call");
}
