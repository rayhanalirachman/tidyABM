//! usage: apply <op> <in.bin> <out.bin>
//! in.bin: f64 n, then n f64 (x), then for `pow` n more f64 (y). out.bin: f64 results.
use rreduce::*;
use std::time::Instant;

fn read(p: &str) -> Vec<f64> {
    std::fs::read(p)
        .unwrap()
        .chunks_exact(8)
        .map(|c| f64::from_le_bytes(c.try_into().unwrap()))
        .collect()
}

fn main() {
    let a: Vec<String> = std::env::args().collect();
    assert!(a.len() == 4, "usage: apply <op> <in.bin> <out.bin>");
    let v = read(&a[2]);
    let n = v[0] as usize;
    let x = &v[1..1 + n];
    let t = Instant::now();
    let out: Vec<f64> = match a[1].as_str() {
        "sum" => vec![sum(x, false)],
        "sum_narm" => vec![sum(x, true)],
        "mean" => vec![mean(x)],
        "cumsum" => cumsum(x),
        "exp" => x.iter().map(|v| r_exp(*v)).collect(),
        "log" => x.iter().map(|v| r_log(*v)).collect(),
        "log2" => x.iter().map(|v| v.log2()).collect(),
        "log10" => x.iter().map(|v| v.log10()).collect(),
        "log1p" => x.iter().map(|v| r_log1p(*v)).collect(),
        "expm1" => x.iter().map(|v| r_expm1(*v)).collect(),
        "sqrt" => x.iter().map(|v| r_sqrt(*v)).collect(),
        "exp_libm" => x.iter().map(|v| v.exp()).collect(),
        "log1p_libm" => x.iter().map(|v| v.ln_1p()).collect(),
        "pow" => {
            let y = &v[1 + n..1 + 2 * n];
            x.iter().zip(y).map(|(a, b)| r_pow(*a, *b)).collect()
        }
        op => panic!("unknown op {op}"),
    };
    let ns = t.elapsed().as_nanos() as f64 / n.max(1) as f64;
    let bytes: Vec<u8> = out.iter().flat_map(|o| o.to_le_bytes()).collect();
    std::fs::write(&a[3], bytes).unwrap();
    println!("{:.2}", ns);
}
