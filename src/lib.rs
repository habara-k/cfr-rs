//! A rust implementation of Counterfacutual Regret Minimization
//! # Example
//! ```
//! use cfr_rs::*;
//! let rule = rule::from_path("src/rule/kuhn.json");
//!
//! let init_strt = strategy::uniform(&rule);
//! let step = 1000;
//! let strt = cfr::calc_nash_strt(&rule, init_strt, step);
//! ```
#[macro_use]
extern crate log;

pub mod action;
pub mod node;
pub mod player;
pub mod rule;
pub mod strategy;

pub mod solver;
pub mod cfr;
