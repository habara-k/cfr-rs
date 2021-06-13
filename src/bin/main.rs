use cfr_rs::*;
use std::time::Instant;

#[macro_use]
extern crate log;
extern crate log4rs;

fn init_logger() {
    use log::LevelFilter;
    use log4rs::append::file::FileAppender;
    use log4rs::encode::pattern::PatternEncoder;
    use log4rs::config::{Appender, Config, Root};

    let logfile = FileAppender::builder()
        .encoder(Box::new(PatternEncoder::new("{d} {l} {t} - {m}{n}")))
        .build("log/output.log").unwrap();

    let config = Config::builder()
        .appender(Appender::builder().build("logfile", Box::new(logfile)))
        .build(Root::builder()
                   .appender("logfile")
                   .build(LevelFilter::Debug)).unwrap();

    log4rs::init_config(config).unwrap();
}

fn main() {
    init_logger();

    trace!("start: main");

    let rule = rule::from_path(&std::env::args().nth(1).expect("no rule given"));
    let step = std::env::args()
        .nth(2)
        .expect("no step given")
        .parse::<usize>()
        .expect("step must be usize");
    let uniform_strt = strategy::uniform(&rule);

    let start = Instant::now();
    let strt = cfr::calc_nash_strt(&rule, uniform_strt, step);
    info!("elapsed time: {} [sec]", start.elapsed().as_nanos() as f64 / 1_000_000_000 as f64);

    info!("calculated strategy: {:#?}", &strt);

    info!("expected value: {:.8}", solver::calc_ev(&rule, &strt));

    info!(
        "P1 can achieve at worst: {:.8}",
        solver::calc_best_resp(&rule, &player::Player::P2, &strt)
    );
    info!(
        "P2 can achieve at worst: {:.8}",
        solver::calc_best_resp(&rule, &player::Player::P1, &strt)
    );

    trace!("finish: main");
}
