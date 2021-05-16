extern crate cfr_rs;
use cfr_rs::*;
use indicatif::ProgressIterator;
use std::time::Instant;

macro_rules! time {
    ($x:expr) => {{
        let start = Instant::now();
        $x;
        let end = start.elapsed();
        end.as_nanos() as f64 / 1_000_000_000 as f64
    }};
}

fn main() {
    let rule = rule::from_name(&std::env::args().nth(1).expect("no rule given"));
    let step = std::env::args()
        .nth(2)
        .expect("no step given")
        .parse::<usize>()
        .expect("step must be usize");
    let run = std::env::args()
        .nth(3)
        .expect("no run times given")
        .parse::<usize>()
        .expect("run times must be usize");

    let mut nanos: Vec<f64> = Vec::new();
    for _ in (0..run).progress() {
        nanos.push(time!({
            cfr::calc_nash_strt(&rule, strategy::uniform(&rule), step)
        }));
    }

    let avg = nanos.iter().sum::<f64>() / nanos.len() as f64;
    let std = nanos
        .iter()
        .map(|t| t - avg)
        .map(|t| t * t)
        .sum::<f64>()
        .sqrt();

    dbg!(avg);
    dbg!(std);
}
