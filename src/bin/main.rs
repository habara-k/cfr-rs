extern crate cfr_rs;
use cfr_rs::*;

fn main() {
    let rule = Rule::from_name("kuhn").unwrap();
    let prof = profile::uniform(&rule);
    let step = 100000;
    let nash_prof = cfr::calc_nash_strt(&rule, prof, step);
    println!("nash_prof: {:?}", nash_prof);
}
