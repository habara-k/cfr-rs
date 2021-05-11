use super::{
    action::ActionId,
    player::Player,
    rule::{InformationSetId, Rule},
};
use std::collections::BTreeMap;

pub type Strategy = BTreeMap<InformationSetId, BTreeMap<ActionId, f64>>;

pub fn ones(rule: &Rule, player: &Player) -> Strategy {
    filled_with(rule, player, &1.0)
}

pub fn zeros(rule: &Rule, player: &Player) -> Strategy {
    filled_with(rule, player, &0.0)
}

pub fn filled_with(rule: &Rule, player: &Player, prob: &f64) -> Strategy {
    rule.info_partitions[player]
        .iter()
        .map(|(info_set_id, _)| {
            (*info_set_id, {
                rule.actions_by_info_set[info_set_id]
                    .iter()
                    .map(|action_id| (*action_id, *prob))
                    .collect()
            })
        })
        .collect()
}

pub fn uniform(rule: &Rule, player: &Player) -> Strategy {
    rule.info_partitions[player]
        .iter()
        .map(|(info_set_id, _)| {
            (*info_set_id, {
                let prob = 1.0 / rule.actions_by_info_set[info_set_id].len() as f64;
                rule.actions_by_info_set[info_set_id]
                    .iter()
                    .map(|action_id| (*action_id, prob))
                    .collect()
            })
        })
        .collect()
}
