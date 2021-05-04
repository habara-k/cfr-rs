extern crate cfr_rs;
use cfr_rs::*;

use approx_eq::assert_approx_eq;

fn main() {
    let rule = Rule::from_name("kuhn").unwrap();
    println!("uniform ev: {}", solver::calc_ev(&rule, &profile::uniform(&rule)));
    // println!("uniform exploitabilyty: {}", solver::calc_exploitability(&rule, &profile::uniform(&rule)));
    let nash_prof = cfr::calc_nash_strt(&rule);
    println!("nash_prof: {:?}", nash_prof);
    // {
    //     let best_prof: Profile = profile::from_name("kuhn_nash").unwrap();

    //     println!("profile1: {:?}", &best_prof[&Player::P1]);
    //     println!("profile2: {:?}", &best_prof[&Player::P2]);

    //     println!("calculate_ev: {}", solver::calc_ev(&rule, &best_prof));
    //     let (best_resp_to_p1, best_val_to_p1) = solver::calc_best_resp_against_to(&rule, &Player::P1, best_prof[&Player::P1].clone());
    //     let (best_resp_to_p2, best_val_to_p2) = solver::calc_best_resp_against_to(&rule, &Player::P2, best_prof[&Player::P2].clone());
    //     println!("best_resp_to_p1: {:?}", best_resp_to_p1);
    //     println!("best_val_to_p1: {:?}", best_val_to_p1);
    //     println!("best_resp_to_p2: {:?}", best_resp_to_p2);
    //     println!("best_val_to_p2: {:?}", best_val_to_p2);

    //     assert_approx_eq!(best_val_to_p1, solver::calc_ev(&rule, 
    //         &profile::from_strt(&Player::P1, best_prof[&Player::P1].clone(), &Player::P2, best_resp_to_p1).unwrap()));

    //     assert_approx_eq!(best_val_to_p2, solver::calc_ev(&rule, 
    //         &profile::from_strt(&Player::P2, best_prof[&Player::P2].clone(), &Player::P1, best_resp_to_p2).unwrap()));

    //     println!("exploitability: {}", solver::calc_exploitability(&rule, &best_prof));
    // }

    // {
    //     let uniform_prof = profile::from_strt(
    //         &Player::P1, strategy::uniform(&rule, &Player::P1),
    //         &Player::P2, strategy::uniform(&rule, &Player::P2),
    //     ).unwrap();

    //     let (best_resp_to_p1, best_val_to_p1) = solver::calc_best_resp_against_to(&rule, &Player::P1, uniform_prof[&Player::P1].clone());
    //     let (best_resp_to_p2, best_val_to_p2) = solver::calc_best_resp_against_to(&rule, &Player::P2, uniform_prof[&Player::P2].clone());
    //     println!("best_resp_to_p1: {:?}", best_resp_to_p1);
    //     println!("best_val_to_p1: {:?}", best_val_to_p1);
    //     println!("best_resp_to_p2: {:?}", best_resp_to_p2);
    //     println!("best_val_to_p2: {:?}", best_val_to_p2);

    //     assert_approx_eq!(best_val_to_p1, solver::calc_ev(&rule, 
    //         &profile::from_strt(&Player::P1, uniform_prof[&Player::P1].clone(), &Player::P2, best_resp_to_p1).unwrap()));

    //     assert_approx_eq!(best_val_to_p2, solver::calc_ev(&rule, 
    //         &profile::from_strt(&Player::P2, uniform_prof[&Player::P2].clone(), &Player::P1, best_resp_to_p2).unwrap()));

    //     println!("exploitability: {}", solver::calc_exploitability(&rule, &uniform_prof));
    // }
}
