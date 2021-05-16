extern crate cfr_rs;
use cfr_rs::{player::Player, *};
use std::time::Instant;

#[macro_use]
extern crate log;

macro_rules! measure {
    ( $x:expr) => {{
        let start = Instant::now();
        let result = $x;
        let end = start.elapsed();
        println!(
            "{}.{:03}[sec] elapsed.",
            end.as_secs(),
            end.subsec_nanos() / 1_000_000
        );
        result
    }};
}

fn main() {
    env_logger::init();

    trace!("start: main");

    let rule = rule::from_name(&std::env::args().nth(1).expect("no rule given"));
    let step = std::env::args()
        .nth(2)
        .expect("no step given")
        .parse::<usize>()
        .expect("step must be usize");
    let uniform_strt = strategy::uniform(&rule);

    info!(
        "uniform exploitability: {}",
        solver::calc_exploitability(&rule, &uniform_strt)
    );

    let strt = measure!({ cfr::calc_nash_strt(&rule, uniform_strt, step) });

    visualizer::print_strt(&rule, &strt);

    println!("expected value: {:.6}", solver::calc_ev(&rule, &strt));

    println!(
        "P1 can achieve at worst: {:.8}",
        solver::calc_best_resp(&rule, &Player::P1, &strt)
    );
    println!(
        "P2 can achieve at worst: {:.8}",
        solver::calc_best_resp(&rule, &Player::P2, &strt)
    );

    info!(
        "exploitability: {}",
        solver::calc_exploitability(&rule, &strt)
    );

    trace!("finish: main");
}
