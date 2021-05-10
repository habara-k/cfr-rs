use std::collections::BTreeMap;
use ord_subset::OrdSubsetIterExt;
use super::{
    action::ActionId,
    player::Player,
    node::{Node, NodeId},
    rule::{Rule, InformationSetId},
    strategy::{self, Strategy},
    profile::{self, Profile},
};

pub fn calc_ev(rule: &Rule, prof: &Profile) -> f64 {
    calc_ev_inner(rule, prof, &rule.root, 1.0)
}

fn calc_ev_inner(rule: &Rule, prof: &Profile, node_id: &NodeId, prob: f64) -> f64 {            
    match &rule.nodes[node_id] {
        Node::Terminal{ value } => {                    
            *value as f64 * prob
        },
        Node::NonTerminal{ edges, .. } => {
            edges.iter().map(|(action_id, child_id)| {
                calc_ev_inner(rule, prof, child_id,  prob) * profile::prob_to_take_action(
                    rule, prof, node_id, action_id)
            }).sum()
        },
    }
}

pub fn calc_best_resp_against_to(rule: &Rule, opponent: &Player, opp_strt: Strategy) -> (Strategy, f64) {
    let myself = opponent.opponent();
    let prof = profile::from_strt(
        &myself, strategy::ones(rule, &myself),
        opponent,  opp_strt);

    let prob_to_reach_terminal_node = calc_prob_to_reach_terminal_node(rule, &prof);
    trace!("prob_to_reach_terminal_node: {:?}", prob_to_reach_terminal_node);

    let best_action_at = |vals: &BTreeMap<NodeId, f64>, info_set_id: &InformationSetId| -> ActionId {                
        assert_eq!(myself, rule.player_by_info_set[info_set_id]);
        rule.actions_by_info_set[info_set_id].iter()
            .ord_subset_max_by_key(
                |action_id| -> f64 {
                    rule.info_partitions[&myself][info_set_id].iter().map(|node_id| {                                
                        vals[&rule.nodes[node_id].edges()[action_id]]
                    }).sum::<f64>() * myself.sign() as f64
                }
            ).unwrap().clone()
    };

    let ord = rule.bfs_ord();
    trace!("ord: {:?}", ord);
    let mut vals: BTreeMap<NodeId, f64> = BTreeMap::new();
    let mut best_actions: BTreeMap<InformationSetId, ActionId> = BTreeMap::new();
    let mut best_strt: Strategy = strategy::zeros(&rule, &myself);

    for node_id in ord.iter().rev() {                        
        match &rule.nodes[node_id] {
            Node::Terminal{ value} => {
                vals.insert(*node_id, (*value as f64) * prob_to_reach_terminal_node[node_id]);
            },
            Node::NonTerminal{ player, edges} => {                                
                vals.insert(*node_id, if *player == myself {
                        let info_set_id = &rule.info_set_id_by_node[node_id];
                        if !best_actions.contains_key(&info_set_id) {
                            let best_action_id = best_action_at(&vals, info_set_id);
                            best_actions.insert(*info_set_id, best_action_id);
                            best_strt.get_mut(info_set_id).unwrap().insert(best_action_id, 1.0);
                        }
                        vals[&edges[&best_actions[info_set_id]]]
                    } else {
                        edges.iter().map(|(_, child_id)| vals[child_id]).sum()
                    });
            }
        }
    }
    trace!("vals: {:?}", vals);
    trace!("best_action: {:?}", best_actions);

    (best_strt, vals[&rule.root])
}

fn calc_prob_to_reach_terminal_node(rule: &Rule, prof: &Profile) -> BTreeMap<NodeId, f64> {
    let mut probs: BTreeMap<NodeId, f64> = BTreeMap::new();
    calc_prob_to_reach_terminal_node_inner(&mut probs, rule, prof, &rule.root, 1.0);
    probs
}

fn calc_prob_to_reach_terminal_node_inner(probs: &mut BTreeMap<NodeId, f64>, rule: &Rule, prof: &Profile, node_id: &NodeId, prob: f64) {
    match &rule.nodes[node_id] {
        Node::Terminal{ .. } => {
            probs.insert(*node_id, prob);
        },                
        Node::NonTerminal{ edges, .. } => {
            for (action_id, child_id) in edges.iter() {
                calc_prob_to_reach_terminal_node_inner(
                    probs, rule, prof, child_id, 
                    prob * profile::prob_to_take_action(rule, prof, node_id, action_id)
                    );
            }
        }
    }
}

pub fn calc_exploitability(rule: &Rule, prof: &Profile) -> f64 {
    let (_, best_resp_to_p2) = calc_best_resp_against_to(rule, &Player::P2, prof[&Player::P2].clone());
    let (_, best_resp_to_p1) = calc_best_resp_against_to(rule, &Player::P1, prof[&Player::P1].clone());
    best_resp_to_p2 - best_resp_to_p1
}