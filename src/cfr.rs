//! Provide CFR calculations.

use super::{
    action::ActionId,
    node::{InformationSetId, Node, NodeId},
    player::Player,
    rule::Rule,
    solver,
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

    let split = 100;
    let mut nash_conv: BTreeMap<usize, f64> = BTreeMap::new();

    for t in (1..step + 1).progress() {
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

        if ((t as f64).log2() * split as f64 / (step as f64).log2()) as i32
            != (((t - 1) as f64).log2() * split as f64 / (step as f64).log2()) as i32
        {
            nash_conv.insert(
                t,
                solver::calc_nash_conv(
                    &rule,
                    &strt_sum
                        .iter()
                        .map(|(info_set_id, dist)| (*info_set_id, normalized(dist.clone())))
                        .collect(),
                ),
            );
        }

        latest_strt = to_strategy(&regret_sum);
    }

    debug!("nash_conv: {}", serde_json::to_string(&nash_conv).unwrap());

    strt_sum
        .into_iter()
        .map(|(info_set_id, dist)| (info_set_id, normalized(dist)))
        .collect()
}

mod regret {
    use super::*;
    pub type Regret = BTreeMap<InformationSetId, BTreeMap<ActionId, f64>>;
    pub fn new(rule: &Rule) -> Regret {
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

            let info_set_id = &rule.info_set_including_node[node_id];
            let pr_myself = match player {
                Player::P1 => pr1,
                Player::P2 => pr2,
                _ => unreachable!(),
            };
            for (action_id, _) in edges {
                *strt_sum
                    .get_mut(info_set_id)
                    .unwrap()
                    .get_mut(action_id)
                    .unwrap() += pr_myself * strt[info_set_id][action_id];
            }
            let action_util: BTreeMap<ActionId, f64> = match player {
                Player::P1 => edges
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
                    .collect(),
                Player::P2 => edges
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
                    .collect(),
                _ => unreachable!(),
            };

            let avg_util: f64 = action_util
                .iter()
                .map(|(action_id, util)| strt[info_set_id][action_id] * util)
                .sum();

            let pr_except = match player {
                Player::P1 => pr2 * prc,
                Player::P2 => pr1 * prc,
                _ => unreachable!(),
            };
            for (action, util) in &action_util {
                *regret_sum
                    .get_mut(info_set_id)
                    .unwrap()
                    .get_mut(action)
                    .unwrap() += player.sign() * (util - avg_util) * pr_except;
            }
            avg_util
        }
    }
}
