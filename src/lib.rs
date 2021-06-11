//! A rust implementation of Counterfacutual Regret Minimization
//! # Example
//! ```
//! use cfr_rs::*;
//! let rule = rule::from_path("src/rule/kuhn.json");
//!
//! let strt = strategy::uniform(&rule);
//! let before = solver::calc_exploitability(&rule, &strt);
//!
//! let step = 1000;
//! let strt = cfr::calc_nash_strt(&rule, strt, step);
//! let after = solver::calc_exploitability(&rule, &strt);
//!
//! assert!(after < before); // exploitability is decreasing!
//! ```
#[macro_use]
extern crate log;

pub mod action;
pub mod node;
pub mod player;
pub mod rule;
pub mod strategy;

pub mod visualizer;

pub mod cfr;
pub mod solver;
