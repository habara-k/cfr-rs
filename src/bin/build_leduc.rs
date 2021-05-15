extern crate cfr_rs;
use cfr_rs::*;

fn main() {
    let leduc = rule::leduc::rule();
    let leduc = serde_json::to_string_pretty(&leduc).unwrap();

    println!("{}", leduc);
}
