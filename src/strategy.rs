use super::{
    action::ActionId,
    rule::{InformationSetId, Rule},
};
use std::collections::BTreeMap;
use std::fs;

pub type Strategy = BTreeMap<InformationSetId, BTreeMap<ActionId, f64>>;

pub fn ones(rule: &Rule) -> Strategy {
    filled_with(rule,  &1.0)
}

pub fn zeros(rule: &Rule) -> Strategy {
    filled_with(rule, &0.0)
}

pub fn filled_with(rule: &Rule, prob: &f64) -> Strategy {
    rule.info_partition
        .iter()
        .map(|(info_set_id, _)| (
            *info_set_id, 
            rule.actions_by_info_set[info_set_id]
                .iter()
                .map(|action_id| (*action_id, *prob))
                .collect()
        ))
        .collect()
}

pub fn uniform(rule: &Rule) -> Strategy {
    rule.info_partition
        .iter()
        .map(|(info_set_id, _)| (
            *info_set_id, {
                let prob = 1.0 / rule.actions_by_info_set[info_set_id].len() as f64;
                rule.actions_by_info_set[info_set_id]
                    .iter()
                    .map(|action_id| (*action_id, prob))
                    .collect()
            }
        ))
        .collect()
}

pub fn from_json(json: &str) -> Strategy {
    let strt: Strategy = serde_json::from_str(json).expect("failed to deserialize json");
    strt
}

pub fn from_file(path: &str) -> Strategy {
    from_json(&fs::read_to_string(path).expect("failed to read file"))
}

pub fn from_name(strt_name: &str) -> Strategy {
    match strt_name {
        "kuhn_nash" => from_file("src/strategy/kuhn_nash.json"),
        "glico_nash" => from_file("src/strategy/glico_nash.json"),
        _ => panic!("invalid strategy name"),
    }
}