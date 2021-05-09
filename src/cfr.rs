use std::collections::BTreeMap;
use indicatif::ProgressIterator;
use super::{
    action::ActionId,
    player::Player,
    node::{Node, NodeId},
    rule::{Rule, InformationSetId},
    transition::{self, Transition},
    strategy,
    profile::{self, Profile},
};

type RegretType = BTreeMap<InformationSetId, BTreeMap<ActionId, f64>>;

fn positive_part(v: BTreeMap<ActionId, f64>) -> BTreeMap<ActionId, f64> {
    v.into_iter().map(|(action_id, prob)| 
        (action_id, if prob < 0.0 { 0.0 } else { prob })
    ).collect()
}
fn normalized(v: BTreeMap<ActionId, f64>) -> BTreeMap<ActionId, f64> {
    let eps = 1e-9;
    let norm: f64 = v.iter().map(|(_, prob)| prob).sum();
    let len = v.len() as f64;
    v.into_iter().map(|(action_id, prob)| 
        (action_id, if norm.abs() < eps { 1.0 / len } else { prob / norm })
    ).collect()
}

#[allow(dead_code)]
fn exploitability_upper_bound(rule: &Rule, t: usize) -> f64 {
    let a1 = rule.info_partitions[&Player::P1].len() as f64 * 
        (rule.max_action_size_of[&Player::P1] as f64).sqrt();
    let a2 = rule.info_partitions[&Player::P2].len() as f64 *
        (rule.max_action_size_of[&Player::P2] as f64).sqrt();

    let max = if a1 < a2 { a2 } else { a1 };

    2.0 * max / (t as f64).sqrt()
}

pub fn calc_nash_strt(rule: &Rule, init_prof: Profile, step: usize) -> Profile {
    let mut regret: BTreeMap<Player, RegretType> = rule.info_partitions.iter().map(|(player, partition)| {
        (*player, partition.iter().map(|(info_set_id, _)| {
            (*info_set_id, rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                (*action_id, 0.0)
            }).collect())
        }).collect())
    }).collect();

    let mut latest_prof = init_prof;
    let mut avg_prof = latest_prof.clone();

    for t in (1..step+1).progress() {
        regret = regret.iter().map(|(myself, reg)| {
            (*myself, {                        
                let prob_to_reach_node_except_myself = calc_prob_to_reach_node_except(rule, &latest_prof, myself);

                let ev_under_node_for_myself = calc_ev_under_node_for_player(rule, &latest_prof, myself);

                let s: RegretType = rule.info_partitions[myself].iter().map(|(info_set_id, node_ids)| {
                    (*info_set_id, rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                        (*action_id, node_ids.iter().map(|node_id| {
                            prob_to_reach_node_except_myself[node_id] * ev_under_node_for_myself[
                                &rule.nodes[node_id].edges()[action_id]
                            ]
                        }).sum())
                    }).collect())
                }).collect();

                let u: BTreeMap<InformationSetId, f64> = rule.info_partitions[myself].iter().map(|(info_set_id, _)| {
                    (*info_set_id, rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                        latest_prof[myself][info_set_id][action_id] * s[info_set_id][action_id]
                    }).sum())
                }).collect();

                let t = t as f64;
                rule.info_partitions[myself].iter().map(|(info_set_id, _)| {
                    (*info_set_id, rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                        (*action_id, (t * reg[info_set_id][action_id] + s[info_set_id][action_id] - u[info_set_id]) / (t + 1.0))
                    }).collect())
                }).collect()
            })
        }).collect();


        latest_prof = regret.iter().map(|(myself, reg)| {
            (*myself, {
                reg.iter().map(|(info_set_id, dist)| {
                    (*info_set_id, normalized(positive_part(dist.clone()).clone()))
                }).collect()
            })
        }).collect();


        avg_prof = [Player::P1, Player::P2].iter().map(|myself| {
            let latest_prob_to_reach_info_set_only_myself = calc_prob_to_reach_info_set_only(rule, &latest_prof, myself);
            let avg_prob_to_reach_info_set_only_myself = calc_prob_to_reach_info_set_only(rule, &avg_prof, myself);
            (*myself, {
                rule.info_partitions[myself].iter().map(|(info_set_id, _)| {                            
                    (*info_set_id, {
                        rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                            (*action_id, {
                                let latest = latest_prob_to_reach_info_set_only_myself[info_set_id];
                                let total = avg_prob_to_reach_info_set_only_myself[info_set_id] * t as f64;
                                (avg_prof[myself][info_set_id][action_id] * total +
                                latest_prof[myself][info_set_id][action_id] * latest) / (latest + total)
                            })
                        }).collect()
                    })
                }).collect()
            })
        }).collect();
    }

    avg_prof
}

fn calc_prob_to_reach_info_set_only(rule: &Rule, prof: &Profile, myself: &Player) -> BTreeMap<InformationSetId, f64> {
    let probs = calc_prob_to_reach_node_only(rule, prof, myself);
    rule.info_partitions[myself].iter().map(|(info_set_id, nodes)| {
        (*info_set_id, nodes.iter().map(|node_id| probs[node_id]).sum())
    }).collect()
}

fn calc_prob_to_reach_node_only(rule: &Rule, prof: &Profile, myself: &Player) -> BTreeMap<NodeId, f64> {
    let mut probs: BTreeMap<NodeId, f64> = BTreeMap::new();
    let opponent = myself.opponent();
    calc_prob_to_reach_node_inner(&mut probs, rule, &profile::from_strt(
        myself, prof[myself].clone(), &opponent, strategy::ones(rule, &opponent)
        ), &transition::ones(rule), &rule.root, 1.0);
    probs
}

fn calc_prob_to_reach_node_except(rule: &Rule, prof: &Profile, myself: &Player) -> BTreeMap<NodeId, f64> {
    let mut probs: BTreeMap<NodeId, f64> = BTreeMap::new();
    let opponent = myself.opponent();
    calc_prob_to_reach_node_inner(&mut probs, rule, &profile::from_strt(
        myself, strategy::ones(rule, myself), &opponent, prof[&opponent].clone()
        ), &rule.transition, &rule.root, 1.0);
    probs
}

fn calc_prob_to_reach_node_inner(probs: &mut BTreeMap<NodeId, f64>, rule: &Rule, prof: &Profile, trans: &Transition, node_id: &NodeId, prob: f64) {
    probs.insert(*node_id, prob);
    if let Node::NonTerminal{ edges, .. } = &rule.nodes[node_id] {
        for (action_id, child_id) in edges {
            calc_prob_to_reach_node_inner(probs, rule, prof, trans, child_id, 
                prob * profile::prob_to_take_action(rule, prof, node_id, action_id)
            );
        }
    }
}

fn calc_ev_under_node_for_player(rule: &Rule, prof: &Profile, myself: &Player) -> BTreeMap<NodeId, f64> {
    let mut ev: BTreeMap<NodeId, f64> = BTreeMap::new();
    calc_ev_under_node_inner_for_player(&mut ev, rule, prof, &rule.root, myself);
    ev
}
fn calc_ev_under_node_inner_for_player(evs: &mut BTreeMap<NodeId, f64>, rule: &Rule, prof: &Profile, node_id: &NodeId, myself: &Player) {
    let val: f64 = match &rule.nodes[node_id] {
        Node::Terminal{ value } => {
            *value as f64 * myself.sign() as f64
        },
        Node::NonTerminal{ edges, .. } => {
            edges.iter().map(|(action_id, child_id)| {
                    calc_ev_under_node_inner_for_player(evs, rule, prof, child_id, myself);
                    evs[child_id] * profile::prob_to_take_action(rule, prof, node_id, action_id)
            }).sum()
        }
    };
    evs.insert(*node_id, val);
}