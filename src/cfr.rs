use super::{
    action::ActionId,
    node::{Node, NodeId},
    player::Player,
    profile::{self, Profile},
    rule::{InformationSetId, Rule},
};
use indicatif::ProgressIterator;
use std::collections::BTreeMap;

type RegretType = BTreeMap<InformationSetId, BTreeMap<ActionId, f64>>;

fn positive_part(v: BTreeMap<ActionId, f64>) -> BTreeMap<ActionId, f64> {
    v.into_iter()
        .map(|(action_id, prob)| (action_id, if prob < 0.0 { 0.0 } else { prob }))
        .collect()
}

fn normalized(v: BTreeMap<ActionId, f64>) -> BTreeMap<ActionId, f64> {
    let eps = 1e-9;
    let norm: f64 = v.iter().map(|(_, prob)| prob).sum();
    let len = v.len() as f64;
    v.into_iter()
        .map(|(action_id, prob)| {
            (
                action_id,
                if norm.abs() < eps {
                    1.0 / len
                } else {
                    prob / norm
                },
            )
        })
        .collect()
}

pub fn calc_nash_strt(rule: &Rule, init_prof: Profile, step: usize) -> Profile {
    let mut regret: BTreeMap<Player, RegretType> = rule
        .info_partitions
        .iter()
        .map(|(player, partition)| {
            (
                *player,
                partition
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
                    .collect(),
            )
        })
        .collect();

    let mut latest_prof = init_prof;
    let mut avg_prof = latest_prof.clone();

    for t in (1..step + 1).progress() {
        regret = regret
            .iter()
            .map(|(myself, reg)| {
                (*myself, {
                    let prob_to_reach_node_except_myself =
                        calc_prob_to_reach_node_except(rule, &latest_prof, myself);

                    let ev_under_node_for_myself =
                        calc_ev_under_node_for_player(rule, &latest_prof, myself);

                    let s: RegretType = rule.info_partitions[myself]
                        .iter()
                        .map(|(info_set_id, node_ids)| {
                            (
                                *info_set_id,
                                rule.actions_by_info_set[info_set_id]
                                    .iter()
                                    .map(|action_id| {
                                        (
                                            *action_id,
                                            node_ids
                                                .iter()
                                                .map(|node_id| {
                                                    prob_to_reach_node_except_myself[node_id]
                                                        * ev_under_node_for_myself[&rule.nodes
                                                            [node_id]
                                                            .edges()[action_id]]
                                                })
                                                .sum(),
                                        )
                                    })
                                    .collect(),
                            )
                        })
                        .collect();

                    let u: BTreeMap<InformationSetId, f64> = rule.info_partitions[myself]
                        .iter()
                        .map(|(info_set_id, _)| {
                            (
                                *info_set_id,
                                rule.actions_by_info_set[info_set_id]
                                    .iter()
                                    .map(|action_id| {
                                        latest_prof[myself][info_set_id][action_id]
                                            * s[info_set_id][action_id]
                                    })
                                    .sum(),
                            )
                        })
                        .collect();

                    let t = t as f64;
                    rule.info_partitions[myself]
                        .iter()
                        .map(|(info_set_id, _)| {
                            (
                                *info_set_id,
                                rule.actions_by_info_set[info_set_id]
                                    .iter()
                                    .map(|action_id| {
                                        (
                                            *action_id,
                                            (t * reg[info_set_id][action_id]
                                                + s[info_set_id][action_id]
                                                - u[info_set_id])
                                                / (t + 1.0),
                                        )
                                    })
                                    .collect(),
                            )
                        })
                        .collect()
                })
            })
            .collect();

        latest_prof = regret
            .iter()
            .map(|(myself, reg)| {
                (*myself, {
                    reg.iter()
                        .map(|(info_set_id, dist)| {
                            (*info_set_id, normalized(positive_part(dist.clone())))
                        })
                        .collect()
                })
            })
            .collect();

        avg_prof = [Player::P1, Player::P2]
            .iter()
            .map(|myself| {
                let latest_prob_to_reach_info_set_only_myself =
                    calc_prob_to_reach_info_set_only(rule, &latest_prof, myself);
                let avg_prob_to_reach_info_set_only_myself =
                    calc_prob_to_reach_info_set_only(rule, &avg_prof, myself);
                (*myself, {
                    rule.info_partitions[myself]
                        .iter()
                        .map(|(info_set_id, _)| {
                            (*info_set_id, {
                                let latest = latest_prob_to_reach_info_set_only_myself[info_set_id];
                                let total =
                                    avg_prob_to_reach_info_set_only_myself[info_set_id] * t as f64;
                                rule.actions_by_info_set[info_set_id]
                                    .iter()
                                    .map(|action_id| {
                                        (*action_id, {
                                            (avg_prof[myself][info_set_id][action_id] * total
                                                + latest_prof[myself][info_set_id][action_id]
                                                    * latest)
                                                / (latest + total)
                                        })
                                    })
                                    .collect()
                            })
                        })
                        .collect()
                })
            })
            .collect();
    }

    avg_prof
}

fn calc_prob_to_reach_info_set_only(
    rule: &Rule,
    prof: &Profile,
    myself: &Player,
) -> BTreeMap<InformationSetId, f64> {
    let mut probs: BTreeMap<InformationSetId, f64> = BTreeMap::new();
    calc_prob_to_reach_info_set_only_inner(&mut probs, rule, prof, &rule.root, 1.0, myself);
    probs
}

fn calc_prob_to_reach_info_set_only_inner(
    probs: &mut BTreeMap<InformationSetId, f64>,
    rule: &Rule,
    prof: &Profile,
    node_id: &NodeId,
    prob: f64,
    myself: &Player,
) {
    if let Node::NonTerminal { player, edges } = &rule.nodes[node_id] {
        if player == myself {
            *probs
                .entry(rule.info_set_id_by_node[node_id])
                .or_insert(0.0) += prob;
        }
        for (action_id, child_id) in edges {
            calc_prob_to_reach_info_set_only_inner(
                probs,
                rule,
                prof,
                child_id,
                prob * if player == myself {
                    profile::prob_to_take_action(rule, prof, node_id, action_id)
                } else {
                    1.0
                },
                myself,
            );
        }
    }
}

fn calc_prob_to_reach_node_except(
    rule: &Rule,
    prof: &Profile,
    myself: &Player,
) -> BTreeMap<NodeId, f64> {
    let mut probs: BTreeMap<NodeId, f64> = BTreeMap::new();
    calc_prob_to_reach_node_except_inner(&mut probs, rule, prof, &rule.root, 1.0, myself);
    probs
}

fn calc_prob_to_reach_node_except_inner(
    probs: &mut BTreeMap<NodeId, f64>,
    rule: &Rule,
    prof: &Profile,
    node_id: &NodeId,
    prob: f64,
    myself: &Player,
) {
    probs.insert(*node_id, prob);
    if let Node::NonTerminal { player, edges } = &rule.nodes[node_id] {
        for (action_id, child_id) in edges {
            calc_prob_to_reach_node_except_inner(
                probs,
                rule,
                prof,
                child_id,
                prob * if player == myself {
                    1.0
                } else {
                    profile::prob_to_take_action(rule, prof, node_id, action_id)
                },
                myself,
            );
        }
    }
}

fn calc_ev_under_node_for_player(
    rule: &Rule,
    prof: &Profile,
    myself: &Player,
) -> BTreeMap<NodeId, f64> {
    let mut ev: BTreeMap<NodeId, f64> = BTreeMap::new();
    calc_ev_under_node_for_player_inner(&mut ev, rule, prof, &rule.root, myself);
    ev
}

fn calc_ev_under_node_for_player_inner(
    evs: &mut BTreeMap<NodeId, f64>,
    rule: &Rule,
    prof: &Profile,
    node_id: &NodeId,
    myself: &Player,
) {
    let val: f64 = match &rule.nodes[node_id] {
        Node::Terminal { value } => *value as f64 * myself.sign() as f64,
        Node::NonTerminal { edges, .. } => edges
            .iter()
            .map(|(action_id, child_id)| {
                calc_ev_under_node_for_player_inner(evs, rule, prof, child_id, myself);
                evs[child_id] * profile::prob_to_take_action(rule, prof, node_id, action_id)
            })
            .sum(),
    };
    evs.insert(*node_id, val);
}
