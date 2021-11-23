//! Representation of *strategy* (probability distribution for legal *actions* in each *information set*)

use super::{
    action::{Action, ActionId},
    node::InformationSetId,
    rule::Rule,
};
use std::collections::BTreeMap;
use std::fs;

pub type Strategy = BTreeMap<InformationSetId, BTreeMap<ActionId, f64>>;

/// Get the `Strategy` filled with zeros.
pub fn new(rule: &Rule) -> Strategy {
    rule.info_sets
        .iter()
        .map(|(info_set_id, _)| {
            (
                *info_set_id,
                rule.actions_at_info_set[info_set_id]
                    .iter()
                    .map(|action_id| (*action_id, 0.0))
                    .collect(),
            )
        })
        .collect()
}

/// Get a uniform `Strategy` (taking actions with equal probability).
pub fn uniform(rule: &Rule) -> Strategy {
    rule.info_sets
        .iter()
        .map(|(info_set_id, _)| {
            (*info_set_id, {
                let prob = 1.0 / rule.actions_at_info_set[info_set_id].len() as f64;
                rule.actions_at_info_set[info_set_id]
                    .iter()
                    .map(|action_id| (*action_id, prob))
                    .collect()
            })
        })
        .collect()
}

/// Get the `Strategy` from the JSON string
/// # Example
/// ```
/// use cfr_rs::*;
/// let strt = strategy::from_json(r#"{
///   "0": {
///     "0": 0.40,
///     "1": 0.20,
///     "2": 0.40
///   },
///   "1": {
///     "0": 0.40,
///     "1": 0.20,
///     "2": 0.40
///   }
/// }"#);   // A nash strategy of Glico(weighted rock paper scissors)
/// ```
pub fn from_json(json: &str) -> Strategy {
    let strt: Strategy = serde_json::from_str(json).expect("failed to deserialize json");
    strt
}

/// Get the `Strategy` from the JSON file.
/// # Example
/// ```
/// use cfr_rs::*;
/// let strt = strategy::from_path("src/strategy/kuhn_nash.json"); // A nash strategy of Kuhn poker
/// ```
pub fn from_path(path: &str) -> Strategy {
    from_json(&fs::read_to_string(path).expect("failed to read file"))
}

pub fn to_string(strt: &Strategy, rule: &Rule) -> String {
    let strt: BTreeMap<String, BTreeMap<Action, f64>> = strt
        .iter()
        .map(|(info_set_id, dist)| {
            (
                rule.info_set_details[&info_set_id].clone(),
                dist.iter()
                    .map(|(action, p)| (rule.actions[action].clone(), *p))
                    .collect(),
            )
        })
        .collect();
    serde_json::to_string(&strt).unwrap()
}
