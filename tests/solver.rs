extern crate cfr_rs;

#[cfg(test)]
mod tests {
    use cfr_rs::*;
    use approx_eq::assert_approx_eq;
    #[test]
    fn kuhn() {
        let rule = Rule::from_name("kuhn").unwrap();

        let nash_prof = profile::from_name("kuhn_nash").unwrap();
        assert_approx_eq!(solver::calc_ev(&rule, &nash_prof), -1.0 / 18.0);
        assert_approx_eq!(solver::calc_exploitability(&rule, &nash_prof), 0.0);

        let uniform_prof = profile::uniform(&rule);
        let calculated_prof = cfr::calc_nash_strt(&rule, uniform_prof.clone(), 1000);
        assert!(solver::calc_exploitability(&rule, &uniform_prof) > solver::calc_exploitability(&rule, &calculated_prof));
    }
    #[test]
    fn glico() {
        let rule = Rule::from_name("glico").unwrap();

        let nash_prof = profile::from_name("glico_nash").unwrap();
        assert_approx_eq!(solver::calc_ev(&rule, &nash_prof), 0.0);
        assert_approx_eq!(solver::calc_exploitability(&rule, &nash_prof), 0.0);

        let uniform_prof = profile::uniform(&rule);
        let calculated_prof = cfr::calc_nash_strt(&rule, uniform_prof.clone(), 1000);
        assert!(solver::calc_exploitability(&rule, &uniform_prof) > solver::calc_exploitability(&rule, &calculated_prof));
    }
}
