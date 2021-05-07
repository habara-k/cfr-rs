extern crate cfr_rs;
use cfr_rs::*;
use std::time::Instant;

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
    measure!({
        let rule = Rule::from_name("kuhn").unwrap();
        let prof = profile::uniform(&rule);
        let step = 10000;
        let nash_prof = cfr::calc_nash_strt(&rule, prof, step);
        println!("best response to P1: {}", solver::calc_best_resp_against_to(&rule, &Player::P1, nash_prof[&Player::P1].clone()).1);
        println!("best response to P2: {}", solver::calc_best_resp_against_to(&rule, &Player::P2, nash_prof[&Player::P2].clone()).1);
        println!("result:");
        visualizer::print(&rule, &nash_prof);
    });
}
