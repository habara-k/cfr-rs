extern crate cfr_rs;
use cfr_rs::{rule, player::Player, profile, solver, visualizer, cfr};
use std::time::Instant;

#[macro_use]
extern crate log;

macro_rules! measure {
    ( $x:expr) => {
        {
            let start = Instant::now();
            let result = $x;
            let end = start.elapsed();
            println!("{}.{:03}[sec] elapsed.", end.as_secs(), end.subsec_nanos() / 1_000_000);
            result
        }
    };
}

fn main() {
    env_logger::init();

    trace!("start: main");

    let rule = rule::from_name(&std::env::args().nth(1).expect("no rule given"));
    let step = std::env::args().nth(2).expect("no step given").parse::<usize>().expect("step must be usize");
    let uniform_prof = profile::uniform(&rule);

    let prof = measure!({
        cfr::calc_nash_strt(&rule, uniform_prof, step)
    });

    visualizer::print_prof(&rule, &prof);

    println!("expected value: {:.6}", solver::calc_ev(&rule, &prof));

    println!("P1 can achieve at worst: {:.6}", solver::calc_best_resp_against_to(&rule, &Player::P1, prof[&Player::P1].clone()).1);
    println!("P2 can achieve at worst: {:.6}", solver::calc_best_resp_against_to(&rule, &Player::P2, prof[&Player::P2].clone()).1);

    info!("exploitability: {}", solver::calc_exploitability(&rule, &prof));

    trace!("finish: main");
}
