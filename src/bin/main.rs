extern crate cfr_rs;
use cfr_rs::*;

fn main() {
    let rule = Rule::from_name("kuhn").unwrap();
    let nash_prof = cfr::calc_nash_strt(&rule);
    println!("nash_prof: {:?}", nash_prof);
}
