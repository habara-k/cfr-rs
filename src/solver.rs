use super::{
    action::ActionId,
    node::{InformationSetId, Node, NodeId},
    player::Player,
    rule::Rule,
    strategy::Strategy,
};
use ord_subset::OrdSubsetIterExt;
use std::collections::{BTreeMap, VecDeque};

pub fn calc_ev(rule: &Rule, strt: &Strategy) -> f64 {
    calc_ev_inner(rule, strt, &rule.root, 1.0)
}

fn calc_ev_inner(rule: &Rule, strt: &Strategy, node_id: &NodeId, prob: f64) -> f64 {
    match &rule.nodes[node_id] {
        Node::Terminal { value } => *value * prob,
        Node::NonTerminal { edges, player } => match player {
            Player::C => &rule.transition[node_id],
            _ => &strt[&rule.info_set_id_by_node[node_id]],
        }
        .iter()
        .map(|(action, p)| calc_ev_inner(rule, strt, &edges[action], prob) * p)
        .sum(),
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

pub fn calc_best_resp(rule: &Rule, myself: &Player, strt: &Strategy) -> f64 {
    let reach_pr: BTreeMap<NodeId, f64> = calc_prob_to_reach_terminal_node(rule, &strt)
        .iter()
        .map(|(node_id, (pr1, pr2, prc))| {
            (
                *node_id,
                match myself {
                    Player::P1 => pr2 * prc,
                    Player::P2 => pr1 * prc,
                    _ => panic!(),
                },
            )
        })
        .collect();

    let best_action_at =
        |utils: &BTreeMap<NodeId, f64>, info_set_id: &InformationSetId| -> &ActionId {
            assert_eq!(*myself, rule.player_by_info_set[info_set_id]);
            rule.actions_by_info_set[info_set_id]
                .iter()
                .ord_subset_max_by_key(|action_id| -> f64 {
                    rule.info_partition[info_set_id]
                        .iter()
                        .map(|node_id| utils[&rule.nodes[node_id].edges()[action_id]])
                        .sum::<f64>()
                        * myself.sign()
                })
                .unwrap()
        };

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
                        let info_set_id = &rule.info_set_id_by_node[node_id];
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
) -> BTreeMap<NodeId, (f64, f64, f64)> {
    let mut probs: BTreeMap<NodeId, (f64, f64, f64)> = BTreeMap::new();
    calc_prob_to_reach_terminal_node_inner(&mut probs, rule, strt, &rule.root, 1.0, 1.0, 1.0);
    probs
}

fn calc_prob_to_reach_terminal_node_inner(
    probs: &mut BTreeMap<NodeId, (f64, f64, f64)>,
    rule: &Rule,
    strt: &Strategy,
    node_id: &NodeId,
    pr1: f64,
    pr2: f64,
    prc: f64,
) {
    match &rule.nodes[node_id] {
        Node::Terminal { .. } => {
            probs.insert(*node_id, (pr1, pr2, prc));
        }
        Node::NonTerminal { player, edges, .. } => match player {
            Player::C => {
                let dist = &rule.transition[node_id];
                for (action_id, child_id) in edges {
                    calc_prob_to_reach_terminal_node_inner(
                        probs,
                        rule,
                        strt,
                        child_id,
                        pr1,
                        pr2,
                        prc * &dist[action_id],
                    )
                }
            }
            Player::P1 => {
                let dist = &strt[&rule.info_set_id_by_node[node_id]];
                for (action_id, child_id) in edges {
                    calc_prob_to_reach_terminal_node_inner(
                        probs,
                        rule,
                        strt,
                        child_id,
                        pr1 * &dist[action_id],
                        pr2,
                        prc,
                    )
                }
            }
            Player::P2 => {
                let dist = &strt[&rule.info_set_id_by_node[node_id]];
                for (action_id, child_id) in edges {
                    calc_prob_to_reach_terminal_node_inner(
                        probs,
                        rule,
                        strt,
                        child_id,
                        pr1,
                        pr2 * &dist[action_id],
                        prc,
                    )
                }
            }
        },
    }
}

pub fn calc_exploitability(rule: &Rule, strt: &Strategy) -> f64 {
    calc_best_resp(rule, &Player::P1, strt) - calc_best_resp(rule, &Player::P2, strt)
}
