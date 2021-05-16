use super::{
    action::ActionId,
    node::{Node, NodeId},
    player::{Player, ProbContribute},
    rule::{InformationSetId, Rule},
    strategy::Strategy,
};
use ord_subset::OrdSubsetIterExt;
use std::collections::BTreeMap;

pub fn calc_ev(rule: &Rule, strt: &Strategy) -> f64 {
    calc_ev_inner(rule, strt, &rule.root, 1.0)
}

fn calc_ev_inner(rule: &Rule, strt: &Strategy, node_id: &NodeId, prob: f64) -> f64 {
    match &rule.nodes[node_id] {
        Node::Terminal { value } => *value as f64 * prob,
        Node::NonTerminal { edges, player } => {
            match player {
                Player::C => edges
                        .iter()
                        .map(|(action_id, child_id)| {
                            calc_ev_inner(rule, strt, child_id, prob)
                                * rule.transition[node_id][action_id]
                        })
                        .sum(),
                _ => {
                    let info_set_id = &rule.info_set_id_by_node[node_id];
                    edges
                        .iter()
                        .map(|(action_id, child_id)| {
                            calc_ev_inner(rule, strt, child_id, prob)
                                * strt[info_set_id][action_id]
                        })
                        .sum()

                },
            }
        },
    }
}

pub fn calc_best_resp(
    rule: &Rule,
    myself: &Player,
    strt: &Strategy,
) -> f64 {
    let prob_to_reach_terminal_node = calc_prob_to_reach_terminal_node(rule, &strt);

    let best_action_at =
        |vals: &BTreeMap<NodeId, f64>, info_set_id: &InformationSetId| -> &ActionId {
            assert_eq!(*myself, rule.player_by_info_set[info_set_id]);
            rule.actions_by_info_set[info_set_id]
                .iter()
                .ord_subset_max_by_key(|action_id| -> f64 {
                    rule.info_partition[info_set_id]
                        .iter()
                        .map(|node_id| vals[&rule.nodes[node_id].edges()[action_id]])
                        .sum::<f64>()
                        * myself.sign() as f64
                })
                .unwrap()
        };

    let ord = rule.bfs_ord();
    trace!("ord: {:?}", ord);
    let mut vals: BTreeMap<NodeId, f64> = BTreeMap::new();
    let mut best_actions: BTreeMap<InformationSetId, ActionId> = BTreeMap::new();

    for node_id in ord.iter().rev() {
        match &rule.nodes[node_id] {
            Node::Terminal { value } => {
                vals.insert(
                    *node_id,
                    (*value as f64) * prob_to_reach_terminal_node[node_id].except(myself),
                );
            }
            Node::NonTerminal { player, edges } => {
                vals.insert(
                    *node_id,
                    if player == myself {
                        let info_set_id = &rule.info_set_id_by_node[node_id];
                        if !best_actions.contains_key(&info_set_id) {
                            let best_action_id = best_action_at(&vals, info_set_id);
                            best_actions.insert(*info_set_id, *best_action_id);
                        }
                        vals[&edges[&best_actions[info_set_id]]]
                    } else {
                        edges.iter().map(|(_, child_id)| vals[child_id]).sum()
                    },
                );
            }
        }
    }
    trace!("vals: {:?}", vals);
    trace!("best_action: {:?}", best_actions);

    vals[&rule.root]
}

fn calc_prob_to_reach_terminal_node(rule: &Rule, strt: &Strategy) -> BTreeMap<NodeId, ProbContribute> {
    let mut probs: BTreeMap<NodeId, ProbContribute> = BTreeMap::new();
    calc_prob_to_reach_terminal_node_inner(&mut probs, rule, strt, &rule.root, ProbContribute::new());
    probs
}

fn calc_prob_to_reach_terminal_node_inner(
    probs: &mut BTreeMap<NodeId, ProbContribute>,
    rule: &Rule,
    strt: &Strategy,
    node_id: &NodeId,
    prob: ProbContribute
) {
    match &rule.nodes[node_id] {
        Node::Terminal { .. } => {
            probs.insert(*node_id, prob);
        }
        Node::NonTerminal { player, edges, .. } => {
            match player {
                Player::C => {
                    for (action_id, child_id) in edges {
                        calc_prob_to_reach_terminal_node_inner(
                            probs,
                            rule,
                            strt,
                            child_id,
                            prob.prod(player, rule.transition[node_id][action_id]))
                    }
                }
                _ => {
                    let info_set_id = &rule.info_set_id_by_node[node_id];
                    for (action_id, child_id) in edges {
                        calc_prob_to_reach_terminal_node_inner(
                            probs,
                            rule,
                            strt,
                            child_id,
                            prob.prod(player, strt[info_set_id][action_id]))
                    }

                },
            }
        }
    }
}

pub fn calc_exploitability(rule: &Rule, strt: &Strategy) -> f64 {
    calc_best_resp(rule, &Player::P1, strt) - 
    calc_best_resp(rule, &Player::P2, strt)
}
