use cfr_rs::{player::Player, *};

#[macro_use]
extern crate log;

fn main() {
    env_logger::init();

    trace!("start: main");

    let rule = rule::from_path(&std::env::args().nth(1).expect("no rule given"));
    let step = std::env::args()
        .nth(2)
        .expect("no step given")
        .parse::<usize>()
        .expect("step must be usize");
    let uniform_strt = strategy::uniform(&rule);

    info!(
        "exploitability of uniform strategy: {}",
        solver::calc_exploitability(&rule, &uniform_strt)
    );

    let strt = cfr::calc_nash_strt(&rule, uniform_strt, step);

    println!("calculated strategy:");
    visualizer::print_strt(&rule, &strt);

    println!("expected value: {:.8}", solver::calc_ev(&rule, &strt));

    println!(
        "P1 can achieve at worst: {:.8}",
        solver::calc_best_resp(&rule, &Player::P2, &strt)
    );
    println!(
        "P2 can achieve at worst: {:.8}",
        solver::calc_best_resp(&rule, &Player::P1, &strt)
    );

    info!(
        "exploitability: {}",
        solver::calc_exploitability(&rule, &strt)
    );

    trace!("finish: main");
}
