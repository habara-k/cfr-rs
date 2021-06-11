extern crate cfr_rs;

#[cfg(test)]
mod tests {
    use approx_eq::assert_approx_eq;
    use cfr_rs::*;
    #[test]
    fn kuhn() {
        let rule = rule::from_path("src/rule/kuhn.json");

        let nash_strt = strategy::from_path("src/strategy/kuhn_nash.json");
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
        let rule = rule::from_path("src/rule/glico.json");

        let nash_strt = strategy::from_path("src/strategy/glico_nash.json");
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
