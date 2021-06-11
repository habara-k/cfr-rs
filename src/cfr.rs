//! Provide CFR calculations.

use super::{
    action::ActionId,
    node::{InformationSetId, Node, NodeId},
    player::Player,
    rule::Rule,
    strategy::{self, Strategy},
};
use indicatif::ProgressIterator;
use std::collections::BTreeMap;

/// Calculate an ε-Nash strategy
/// # Example
/// ```
/// use cfr_rs::*;
/// let rule = rule::from_path("src/rule/kuhn.json");
/// let step = 1000;
/// let strt = cfr::calc_nash_strt(&rule, strategy::uniform(&rule), step);
/// ```
pub fn calc_nash_strt(rule: &Rule, init_strt: Strategy, step: usize) -> Strategy {
    let mut regret_sum = regret::new(rule);
    let mut latest_strt = init_strt;
    let mut strt_sum = strategy::new(rule);

    for _ in (1..step + 1).progress() {
        cfr_dfs(
            rule,
            &mut regret_sum,
            &mut strt_sum,
            &latest_strt,
            &rule.root,
            1.0,
            1.0,
            1.0,
        );

        latest_strt = to_strategy(&regret_sum);
    }

    strt_sum
        .into_iter()
        .map(|(info_set_id, dist)| (info_set_id, normalized(dist)))
        .collect()
}

mod regret {
    use super::*;
    pub type Regret = BTreeMap<InformationSetId, BTreeMap<ActionId, f64>>;
    pub fn new(rule: &Rule) -> Regret {
        rule.info_partition
            .iter()
            .map(|(info_set_id, _)| {
                (
                    *info_set_id,
                    rule.actions_by_info_set[info_set_id]
                        .iter()
                        .map(|action_id| (*action_id, 0.0))
                        .collect(),
                )
            })
            .collect()
    }
}
use regret::Regret;

fn to_strategy(regret: &Regret) -> Strategy {
    regret
        .iter()
        .map(|(info_set_id, v)| (*info_set_id, normalized(positive_part(v.clone()))))
        .collect()
}

fn positive_part<T>(v: BTreeMap<T, f64>) -> BTreeMap<T, f64>
where
    T: Ord,
{
    v.into_iter()
        .map(|(key, prob)| (key, if prob < 0.0 { 0.0 } else { prob }))
        .collect()
}
fn normalized<T>(v: BTreeMap<T, f64>) -> BTreeMap<T, f64>
where
    T: Ord,
{
    let norm: f64 = v.iter().map(|(_, prob)| prob).sum();
    if norm < 1e-9 {
        let len = v.len() as f64;
        v.into_iter().map(|(key, _)| (key, 1.0 / len)).collect()
    } else {
        v.into_iter()
            .map(|(key, prob)| (key, prob / norm))
            .collect()
    }
}

fn cfr_dfs(
    rule: &Rule,
    regret_sum: &mut Regret,
    strt_sum: &mut Strategy,
    strt: &Strategy,
    node_id: &NodeId,
    pr1: f64,
    pr2: f64,
    prc: f64,
) -> f64 {
    match &rule.nodes[node_id] {
        Node::Terminal { value } => *value,
        Node::NonTerminal { player, edges } => {
            if *player == Player::C {
                return edges
                    .iter()
                    .map(|(action_id, child_id)| {
                        let t = &rule.transition[node_id][action_id];
                        cfr_dfs(
                            rule,
                            regret_sum,
                            strt_sum,
                            strt,
                            child_id,
                            pr1,
                            pr2,
                            prc * t,
                        ) * t
                    })
                    .sum();
            }

            let info_set_id = &rule.info_set_id_by_node[node_id];
            let action_util: BTreeMap<ActionId, f64> = match player {
                Player::P1 => {
                    for (action, _) in edges {
                        *strt_sum
                            .get_mut(info_set_id)
                            .unwrap()
                            .get_mut(action)
                            .unwrap() += pr1 * strt[info_set_id][action];
                    }
                    edges
                        .iter()
                        .map(|(action_id, child_id)| {
                            (
                                *action_id,
                                cfr_dfs(
                                    rule,
                                    regret_sum,
                                    strt_sum,
                                    strt,
                                    child_id,
                                    pr1 * strt[info_set_id][action_id],
                                    pr2,
                                    prc,
                                ),
                            )
                        })
                        .collect()
                }
                Player::P2 => {
                    for (action, _) in edges {
                        *strt_sum
                            .get_mut(info_set_id)
                            .unwrap()
                            .get_mut(action)
                            .unwrap() += pr2 * strt[info_set_id][action];
                    }
                    edges
                        .iter()
                        .map(|(action_id, child_id)| {
                            (
                                *action_id,
                                cfr_dfs(
                                    rule,
                                    regret_sum,
                                    strt_sum,
                                    strt,
                                    child_id,
                                    pr1,
                                    pr2 * strt[info_set_id][action_id],
                                    prc,
                                ),
                            )
                        })
                        .collect()
                }
                _ => panic!(),
            };

            let ret: f64 = action_util
                .iter()
                .map(|(action_id, util)| strt[info_set_id][action_id] * util)
                .sum();

            let pr_except = match player {
                Player::P1 => pr2 * prc,
                Player::P2 => pr1 * prc,
                _ => panic!(),
            };
            for (action, util) in &action_util {
                *regret_sum
                    .get_mut(info_set_id)
                    .unwrap()
                    .get_mut(action)
                    .unwrap() += player.sign() * (util - ret) * pr_except;
            }
            ret
        }
    }
}
