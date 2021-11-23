//! Provide basic calculations.

use super::{
    action::ActionId,
    node::{InformationSetId, Node, NodeId},
    player::Player,
    rule::Rule,
    strategy::Strategy,
};
use ord_subset::OrdSubsetIterExt;
use std::collections::{BTreeMap, VecDeque};

/// Calculate the expected value.
/// # Example
/// ```
/// use cfr_rs::*;
/// use approx_eq::assert_approx_eq;
///
/// let rule = rule::from_path("src/rule/kuhn.json");
/// let strt = strategy::from_path("src/strategy/kuhn_nash.json");
/// assert_approx_eq!(solver::calc_ev(&rule, &strt), -1.0 / 18.0); // The optimal expected value is `-1/18`
/// ```
pub fn calc_ev(rule: &Rule, strt: &Strategy) -> f64 {
    calc_ev_inner(rule, strt, &rule.root, 1.0)
}

fn calc_ev_inner(rule: &Rule, strt: &Strategy, node_id: &NodeId, prob: f64) -> f64 {
    match &rule.nodes[node_id] {
        Node::Terminal { value } => *value * prob,
        Node::NonTerminal { edges, player } => match player {
            Player::C => &rule.transition[node_id],
            _ => &strt[&rule.info_set_including_node[node_id]],
        }
        .iter()
        .map(|(action, p)| calc_ev_inner(rule, strt, &edges[action], prob) * p)
        .sum(),
    }
}

/// Calculate the optimal value of the expected value when `myself` is free to change the strategy.
/// # Example
/// ```
/// use cfr_rs::*;
/// use approx_eq::assert_approx_eq;
///
/// let rule = rule::from_path("src/rule/kuhn.json");
/// let strt = strategy::from_path("src/strategy/kuhn_nash.json");
/// assert_approx_eq!(solver::calc_best_resp(&rule, &player::Player::P1, &strt), -1.0 / 18.0); // The expected value cannot changes.
/// assert_approx_eq!(solver::calc_best_resp(&rule, &player::Player::P2, &strt), -1.0 / 18.0); // The expected value cannot changes.
///
/// let strt = strategy::uniform(&rule);
/// assert!(solver::calc_best_resp(&rule, &player::Player::P1, &strt) >= -1.0 / 18.0);  // The expected value never decreases.
/// assert!(solver::calc_best_resp(&rule, &player::Player::P2, &strt) <= -1.0 / 18.0);  // The expected value never increases.
/// ```
pub fn calc_best_resp(rule: &Rule, myself: &Player, strt: &Strategy) -> f64 {
    let best_action_at =
        |utils: &BTreeMap<NodeId, f64>, info_set_id: &InformationSetId| -> &ActionId {
            assert_eq!(*myself, rule.player_at_info_set[info_set_id]);
            rule.actions_at_info_set[info_set_id]
                .iter()
                .ord_subset_max_by_key(|action_id| -> f64 {
                    rule.info_sets[info_set_id]
                        .iter()
                        .map(|node_id| utils[&rule.nodes[node_id].edges()[action_id]])
                        .sum::<f64>()
                        * myself.sign()
                })
                .unwrap()
        };

    let reach_pr: BTreeMap<NodeId, f64> = calc_prob_to_reach_terminal_node(rule, &strt, myself);
    let ord = bfs_ord(rule);
    trace!("ord: {:?}", ord);
    let mut utils: BTreeMap<NodeId, f64> = BTreeMap::new();
    let mut best_actions: BTreeMap<InformationSetId, ActionId> = BTreeMap::new();

    for node_id in ord.iter().rev() {
        match &rule.nodes[node_id] {
            Node::Terminal { value } => {
                utils.insert(*node_id, *value * reach_pr[node_id]);
            }
            Node::NonTerminal { player, edges } => {
                utils.insert(
                    *node_id,
                    if player == myself {
                        let info_set_id = &rule.info_set_including_node[node_id];
                        if !best_actions.contains_key(&info_set_id) {
                            let best_action_id = best_action_at(&utils, info_set_id);
                            best_actions.insert(*info_set_id, *best_action_id);
                        }
                        utils[&edges[&best_actions[info_set_id]]]
                    } else {
                        edges.values().map(|child_id| utils[child_id]).sum()
                    },
                );
            }
        }
    }
    trace!("utils: {:?}", utils);
    trace!("best_action: {:?}", best_actions);

    utils[&rule.root]
}

fn calc_prob_to_reach_terminal_node(
    rule: &Rule,
    strt: &Strategy,
    except: &Player,
) -> BTreeMap<NodeId, f64> {
    let mut probs: BTreeMap<NodeId, f64> = BTreeMap::new();
    calc_prob_to_reach_terminal_node_inner(&mut probs, rule, strt, &rule.root, except, 1.0);
    probs
}

fn calc_prob_to_reach_terminal_node_inner(
    probs: &mut BTreeMap<NodeId, f64>,
    rule: &Rule,
    strt: &Strategy,
    node_id: &NodeId,
    except: &Player,
    p: f64,
) {
    match &rule.nodes[node_id] {
        Node::Terminal { .. } => {
            probs.insert(*node_id, p);
        }
        Node::NonTerminal { player, edges, .. } => {
            let dist = match player {
                Player::C => &rule.transition[node_id],
                _ => &strt[&rule.info_set_including_node[node_id]],
            };
            for (action_id, child_id) in edges {
                calc_prob_to_reach_terminal_node_inner(
                    probs,
                    rule,
                    strt,
                    child_id,
                    except,
                    if except == player {
                        p
                    } else {
                        p * dist[action_id]
                    },
                )
            }
        }
    }
}

fn bfs_ord(rule: &Rule) -> Vec<NodeId> {
    let mut ord: Vec<NodeId> = Vec::new();
    let mut que: VecDeque<NodeId> = VecDeque::new();
    que.push_back(rule.root);
    ord.push(rule.root);
    while !que.is_empty() {
        let node_id = *que.front().unwrap();
        que.pop_front();
        if let Node::NonTerminal { edges, .. } = &rule.nodes[&node_id] {
            for (_, child_id) in edges.iter() {
                que.push_back(*child_id);
                ord.push(*child_id);
            }
        }
    }
    ord
}

/// Calculate NashConv: A common metric for determining the rates of convergence
/// # Example
/// ```
/// use cfr_rs::*;
/// use approx_eq::assert_approx_eq;
///
/// let rule = rule::from_path("src/rule/kuhn.json");
/// let strt = strategy::from_path("src/strategy/kuhn_nash.json");
/// assert_approx_eq!(solver::calc_nash_conv(&rule, &strt), 0.0);
/// ```
pub fn calc_nash_conv(rule: &Rule, strt: &Strategy) -> f64 {
    calc_best_resp(rule, &Player::P1, strt) - calc_best_resp(rule, &Player::P2, strt)
}
