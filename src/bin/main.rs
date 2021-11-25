use cfr_rs::*;
use std::time::Instant;
use argh::FromArgs;

#[derive(FromArgs)]
/// Calculate ε-Nash Strategy
struct Args {
    /// the JSON file path of Game rule
    #[argh(option)]
    rule: String,
    /// the number of iterations
    #[argh(option)]
    step: usize,
}

#[macro_use]
extern crate log;

fn main() {
    env_logger::init();
    let args: Args = argh::from_env();

    info!("start: main");

    let rule = rule::from_path(&args.rule);
    let uniform_strt = strategy::uniform(&rule);

    let start = Instant::now();
    let strt = cfr::calc_nash_strt(&rule, uniform_strt, args.step);
    info!(
        "elapsed time: {} [sec]",
        start.elapsed().as_nanos() as f64 / 1_000_000_000 as f64
    );

    info!("calculated strategy: {}", strategy::to_string(&strt, &rule));

    info!("expected value: {:.8}", solver::calc_ev(&rule, &strt));

    info!(
        "P1 can achieve at worst: {:.8}",
        solver::calc_best_resp(&rule, &player::Player::P2, &strt)
    );
    info!(
        "P2 can achieve at worst: {:.8}",
        solver::calc_best_resp(&rule, &player::Player::P1, &strt)
    );

    info!("finish: main");
}
