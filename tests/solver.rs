extern crate cfr_rs;

#[cfg(test)]
mod tests {
    use approx_eq::assert_approx_eq;
    use cfr_rs::*;
    #[test]
    fn kuhn() {
        let rule = rule::from_name("kuhn");

        let nash_strt = strategy::from_name("kuhn_nash");
        assert_approx_eq!(solver::calc_ev(&rule, &nash_strt), -1.0 / 18.0);
        assert_approx_eq!(solver::calc_exploitability(&rule, &nash_strt), 0.0);

        let uniform_strt = strategy::uniform(&rule);
        let calculated_strt = cfr::calc_nash_strt(&rule, uniform_strt.clone(), 1000);
        assert!(
            solver::calc_exploitability(&rule, &uniform_strt)
                > solver::calc_exploitability(&rule, &calculated_strt)
        );
    }
    #[test]
    fn glico() {
        let rule = rule::from_name("glico");

        let nash_strt = strategy::from_name("glico_nash");
        assert_approx_eq!(solver::calc_ev(&rule, &nash_strt), 0.0);
        assert_approx_eq!(solver::calc_exploitability(&rule, &nash_strt), 0.0);

        let uniform_strt = strategy::uniform(&rule);
        let calculated_strt = cfr::calc_nash_strt(&rule, uniform_strt.clone(), 1000);
        assert!(
            solver::calc_exploitability(&rule, &uniform_strt)
                > solver::calc_exploitability(&rule, &calculated_strt)
        );
    }
}
