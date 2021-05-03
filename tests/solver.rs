extern crate cfr_rs;

#[cfg(test)]
mod tests {
    use cfr_rs::*;
    use approx_eq::assert_approx_eq;
    #[test]
    fn it_works() {
        let rule = Rule::from_name("kuhn").unwrap();
        let prof = profile::from_name("kuhn_nash").unwrap();
        assert_approx_eq!(solver::calc_ev(&rule, &prof), -1.0 / 18.0);
        assert_approx_eq!(solver::calc_exploitability(&rule, &prof), 0.0);

        let prof = profile::from_strt(
            &Player::P1, strategy::uniform(&rule, &Player::P1),
            &Player::P2, strategy::uniform(&rule, &Player::P2)).unwrap();
        assert!(solver::calc_exploitability(&rule, &prof) > 0.0);
    }
}
