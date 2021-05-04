#![feature(test)]
extern crate test;
extern crate cfr_rs;

fn main() {
}

#[cfg(test)]
mod tests {
    use test::Bencher;
    use cfr_rs::*;
    #[bench]
    fn bench_cfr(b: &mut Bencher) {
        b.iter(|| {
            let rule = Rule::from_name("kuhn").unwrap();
            let prof = profile::uniform(&rule);
            let step = 1000;
            let nash_prof = cfr::calc_nash_strt(&rule, prof, step);
            println!("nash_prof: {:?}", nash_prof);
        });
    }
}