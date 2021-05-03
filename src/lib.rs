use std::collections::{BTreeMap,VecDeque};
use serde::{Deserialize, Serialize};
use serde_json;
use std::fs;
use ord_subset::OrdSubsetIterExt;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize, Debug)]
pub enum Player {
    P1,
    P2,
    C,
}

pub fn opponent_of(player: &Player) -> Option<Player> {
    match player {
        Player::P1 => Some(Player::P2),
        Player::P2 => Some(Player::P1),
        Player::C => None,
    }
}


pub type Action = String;

pub type NodeValue = i32;
pub type NodeId = usize;
pub type InformationSetId = usize;
pub type ActionId = usize;

pub type InformationSet = Vec<NodeId>;
pub type InformationPartition = BTreeMap<InformationSetId, InformationSet>;


#[derive(Clone, Serialize, Deserialize)]
pub enum Node {
    Terminal {
        value: NodeValue,
    },
    NonTerminal {
        player: Player,
        edges: BTreeMap<ActionId, NodeId>,
    },
}


#[derive(Deserialize, Serialize)]
pub struct Rule {
    actions: BTreeMap<ActionId, Action>,
    nodes: BTreeMap<NodeId, Node>,
    root: NodeId,
    info_partitions: BTreeMap<Player,InformationPartition>,
    transition: Transition,

    // for utils
    #[serde(skip_deserializing)]
    info_set_id_by_node: BTreeMap<NodeId, InformationSetId>,
    #[serde(skip_deserializing)]
    actions_by_info_set: BTreeMap<InformationSetId, Vec<ActionId>>,
    #[serde(skip_deserializing)]
    player_by_info_set: BTreeMap<InformationSetId, Player>,
}

impl Rule {
    pub fn from_name(rule_name: &str) -> Option<Self> {
        match rule_name {
            "kuhn" => {
                let mut rule: Rule = serde_json::from_str(
                    &fs::read_to_string("src/rule/kuhn.json").unwrap()
                ).unwrap();
                rule.build();
                Some(rule)
            },
            _ => None
        } 
    }

    fn build(&mut self) {
        // build info_set_id_by_node
        for (_, partition) in self.info_partitions.iter() {                
            for (info_set_id, info_set) in partition.iter() {
                for node_id in info_set.iter() {
                    self.info_set_id_by_node.insert(*node_id, *info_set_id);
                }
            }
        }

        // build actions_by_info_set, player_by_info_set
        for (_, partition) in self.info_partitions.iter() {                
            for (info_set_id, info_set) in partition.iter() {
                for node_id in info_set.iter() {                        
                    if let Node::NonTerminal{ player, edges} = &self.nodes[node_id] {
                        let actions: Vec<ActionId> = edges.keys().cloned().collect();
                        if self.actions_by_info_set.contains_key(info_set_id) {
                            assert_eq!(actions, self.actions_by_info_set[info_set_id]);
                        } else {
                            self.actions_by_info_set.insert(*info_set_id, actions);
                        }

                        if self.player_by_info_set.contains_key(info_set_id) {
                            assert_eq!(*player, self.player_by_info_set[info_set_id]);
                        } else {
                            self.player_by_info_set.insert(*info_set_id, player.clone());
                        }
                    }
                }
            }
        }
    }

    pub fn bfs_ord(&self) -> Vec<NodeId> {
        let mut ord: Vec<NodeId> = Vec::new();
        let mut que: VecDeque<NodeId> = VecDeque::new();
        que.push_back(self.root);
        ord.push(self.root);
        while !que.is_empty() {                        
            let node_id = que.front().unwrap().clone();
            que.pop_front();
            if let Node::NonTerminal{ edges, .. } = &self.nodes[&node_id] {
                for (_, child_id) in edges.iter() {                                    
                    que.push_back(*child_id);
                    ord.push(*child_id);
                }
            }
        }
        ord
    }
}

pub type Value = f64;
pub type Distribution = BTreeMap<ActionId, Value>;
pub type Transition = BTreeMap<NodeId, Distribution>;
pub type Strategy = BTreeMap<InformationSetId, Distribution>;
pub type Profile = BTreeMap<Player,Strategy>;

pub mod strategy {
    use super::*;
    pub fn ones(rule: &Rule, player: &Player) -> Strategy {
        filled_with(&rule, &player, &1.0)
    }
    pub fn zeros(rule: &Rule, player: &Player) -> Strategy {
        filled_with(&rule, &player, &0.0)
    }
    pub fn filled_with(rule: &Rule, player: &Player, prob: &Value) -> Strategy {
        let mut strt = Strategy::new();
        for (info_set_id, _) in rule.info_partitions[&player].iter() {                
            strt.insert(
                *info_set_id, 
                rule.actions_by_info_set[info_set_id].iter().cloned().map(|action| (action, prob.clone())).collect()
            );
        }
        strt
    }
    pub fn uniform(rule: &Rule, player: &Player) -> Strategy {
        let mut strt = Strategy::new();
        for (info_set_id, _) in rule.info_partitions[&player].iter() {                
            let prob: Value = 1.0 / rule.actions_by_info_set[&info_set_id].len() as Value;
            strt.insert(
                *info_set_id, 
                rule.actions_by_info_set[info_set_id].iter().cloned().map(|action| (action, prob)).collect()
            );
        }
        strt
    }
}

pub mod profile {
    use super::*;
    pub fn from_name(prof_name: &str) -> Option<Profile> {
        match prof_name {
            "kuhn_nash" => {
                let prof: Profile = serde_json::from_str(
                    &fs::read_to_string("src/profile/kuhn_nash.json").unwrap()
                ).unwrap();
                Some(prof)
            },
            _ => None
        }
    }
    pub fn from_strt(p1: &Player, p1_strt: Strategy, p2: &Player, p2_strt: Strategy) -> Option<Profile> {
        if *p1 == Player::C || *p2 == Player::C || *p1 == *p2 {
            return None
        }
        let mut prof = Profile::new();
        prof.insert(p1.clone(), p1_strt);
        prof.insert(p2.clone(), p2_strt);
        Some(prof)
    }
}

pub mod solver {
    use super::*;

    pub fn calc_ev(rule: &Rule, prof: &Profile) -> Value {
        calc_ev_inner(rule, prof, &rule.root, 1.0)
    }

    fn calc_ev_inner(rule: &Rule, profile: &Profile, node_id: &NodeId, prob: Value) -> Value {            
        match &rule.nodes[node_id] {
            Node::Terminal{ value } => {                    
                *value as Value * prob
            },
            Node::NonTerminal{ player, edges} => {
                let mut sum: Value = 0.0;
                for (action_id, child_id) in edges.iter() {                        
                    sum = sum + calc_ev_inner(rule, profile, child_id,  prob) * match player {
                        Player::P1 | Player::P2 => {
                            profile[player][&rule.info_set_id_by_node[node_id]][action_id]
                        },
                        Player::C => {
                            rule.transition[&node_id][action_id]
                        }
                    }
                }
                sum
            },
        }
    }

    pub fn calc_best_resp_against_to(rule: &Rule, opponent: &Player, opp_strt: Strategy) -> (Strategy, Value) {
        let myself = opponent_of(&opponent).unwrap();
        let prof = profile::from_strt(
            &myself, strategy::ones(rule, &myself),
            &opponent,  opp_strt).unwrap();

        let mut terminal_probs: BTreeMap<NodeId, Value> = BTreeMap::new();
        calc_terminal_probs(&mut terminal_probs, &rule, &prof, &rule.root, 1.0);

        let best_action = |vals: &BTreeMap<NodeId, Value>, info_set_id: &InformationSetId| -> ActionId {                
            assert_eq!(myself, rule.player_by_info_set[&info_set_id]);
            rule.actions_by_info_set[&info_set_id].iter()
                .ord_subset_max_by_key(
                    |action| {
                        let sum: Value = rule.info_partitions[&myself][&info_set_id].iter().map(
                            |other_node_id| {
                                vals[match &rule.nodes[&other_node_id] {
                                    Node::NonTerminal{ edges, .. } => {
                                        &edges[action]
                                    },
                                    Node::Terminal{ .. } => {
                                        panic!()
                                    }
                                }]
                            }
                        ).sum();
                        sum * match myself {
                            Player::P1 => 1.0,
                            Player::P2 => -1.0,
                            _ => panic!(),
                        }
                    }
                ).unwrap().clone()
        };

        let ord = rule.bfs_ord();
        let mut vals: BTreeMap<NodeId, Value> = BTreeMap::new();
        let mut best_act: BTreeMap<InformationSetId, ActionId> = BTreeMap::new();
        let mut best_strt: Strategy = strategy::zeros(&rule, &myself);

        for node_id in ord.iter().rev() {                        
            match &rule.nodes[node_id] {
                Node::Terminal{ value} => {
                    vals.insert(*node_id, (*value as Value) * terminal_probs[&node_id]);
                },
                Node::NonTerminal{ player, edges} => {                                
                    vals.insert(*node_id, if *player == myself {
                            let info_set_id = &rule.info_set_id_by_node[node_id];
                            if !best_act.contains_key(&info_set_id) {
                                let best_action_id = best_action(&vals, &info_set_id);
                                best_act.insert(*info_set_id, best_action_id);
                                best_strt.get_mut(&info_set_id).unwrap().insert(best_action_id, 1.0);
                            }
                            vals[&edges[&best_act[&info_set_id]]]
                        } else {
                            edges.iter().map(|(_, child_id)| vals[child_id]).sum()
                        });
                }
            }
        }

        (best_strt, vals[&rule.root])
    }

    fn calc_terminal_probs(terminal_probs: &mut BTreeMap<NodeId, Value>, rule: &Rule, profile: &Profile, node_id: &NodeId, prob: Value) {
        match &rule.nodes[&node_id] {
            Node::Terminal{ .. } => {
                terminal_probs.insert(*node_id, prob);
            },                
            Node::NonTerminal{ player, edges} => {
                for (action, child_id) in edges.iter() {
                    calc_terminal_probs(
                        terminal_probs, rule, profile, child_id, 
                        prob * match player {
                            Player::P1 | Player::P2 => {
                                profile[player][&rule.info_set_id_by_node[&node_id]][action]
                            },
                            Player::C => {
                                rule.transition[&node_id][action]
                            }
                        });
                }
            }
        }
    }

    pub fn calc_exploitability(rule: &Rule, prof: &Profile) -> Value {
        let (_, best_resp_to_p2) = calc_best_resp_against_to(rule, &Player::P2, prof[&Player::P2].clone());
        let (_, best_resp_to_p1) = calc_best_resp_against_to(rule, &Player::P1, prof[&Player::P1].clone());
        best_resp_to_p2 - best_resp_to_p1
    }
}