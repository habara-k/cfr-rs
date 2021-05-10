use std::collections::{BTreeMap, VecDeque};
use std::fs;
use serde::{Deserialize, Serialize};
use super::{
    action::{Action, ActionId},
    node::{Node, NodeId, NodeValue},
    player::Player,
};

pub type InformationSet = Vec<NodeId>;

#[derive(Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct InformationSetId(usize);

pub type InformationPartition = BTreeMap<InformationSetId, InformationSet>;

pub type Transition = BTreeMap<NodeId, BTreeMap<ActionId, f64>>;

#[derive(Deserialize, Serialize)]
pub struct Rule {
    pub actions: BTreeMap<ActionId, Action>,
    pub nodes: BTreeMap<NodeId, Node>,
    pub root: NodeId,
    pub info_partitions: BTreeMap<Player,InformationPartition>,
    pub transition: Transition,

    // for utils
    #[serde(skip_deserializing)]
    pub info_set_id_by_node: BTreeMap<NodeId, InformationSetId>,
    #[serde(skip_deserializing)]
    pub actions_by_info_set: BTreeMap<InformationSetId, Vec<ActionId>>,
    #[serde(skip_deserializing)]
    pub player_by_info_set: BTreeMap<InformationSetId, Player>,
    #[serde(skip_deserializing)]
    pub node_value_scale: NodeValue,
    #[serde(skip_deserializing)]
    pub max_action_size_of: BTreeMap<Player, usize>,
    #[serde(skip_deserializing)]
    pub history: BTreeMap<NodeId, Vec<ActionId>>
}

impl Rule {
    fn build(&mut self) {
        self.build_info_set_id_by_node();
        self.build_actions_by_info_set();
        self.build_player_by_info_set();
        self.build_node_value_scale();
        self.build_max_action_size_of();
        self.build_history();
    }

    fn build_info_set_id_by_node(&mut self) {
        trace!("start: build_info_set_id_by_node");
        for (_, partition) in self.info_partitions.iter() {                
            for (info_set_id, info_set) in partition.iter() {
                for node_id in info_set.iter() {
                    self.info_set_id_by_node.insert(*node_id, *info_set_id);
                }
            }
        }
        trace!("finish: build_info_set_id_by_node");
    }

    fn build_actions_by_info_set(&mut self) {
        trace!("start: build_actions_by_info_set");
        for (_, partition) in self.info_partitions.iter() {                
            for (info_set_id, info_set) in partition.iter() {
                for node_id in info_set.iter() {                        
                    if let Node::NonTerminal{ edges, .. } = &self.nodes[node_id] {
                        let actions: Vec<ActionId> = edges.keys().cloned().collect();
                        if self.actions_by_info_set.contains_key(info_set_id) {
                            assert_eq!(actions, self.actions_by_info_set[info_set_id]);
                        } else {
                            self.actions_by_info_set.insert(*info_set_id, actions);
                        }
                    }
                }
            }
        }
        trace!("finish: build_actions_by_info_set");
    }

    fn build_player_by_info_set(&mut self) {
        trace!("start: build_player_by_info_set");
        for (_, partition) in self.info_partitions.iter() {                
            for (info_set_id, info_set) in partition.iter() {
                for node_id in info_set.iter() {                        
                    if let Node::NonTerminal{ player, .. } = &self.nodes[node_id] {
                        if self.player_by_info_set.contains_key(info_set_id) {
                            assert_eq!(*player, self.player_by_info_set[info_set_id]);
                        } else {
                            self.player_by_info_set.insert(*info_set_id, *player);
                        }
                    }
                }
            }
        }
        trace!("finish: build_player_by_info_set");
    }

    fn build_node_value_scale(&mut self) {
        trace!("start: build_node_value_scale");
        self.node_value_scale = {
            let iter = self.nodes.iter().filter(|&(_, node)| {
                node.is_terminal()
            }).map(|(_, node)| {
                node.value()
            });
            iter.clone().max().unwrap() - iter.min().unwrap()
        };
        trace!("finish: build_node_value_scale");
    }

    fn build_max_action_size_of(&mut self) {
        trace!("start: build_max_action_size_of");
        self.max_action_size_of = [Player::P1, Player::P2].iter().map(|player| {
            (*player, {
                self.info_partitions[player].iter().map(|(info_set_id, _)| {
                    self.actions_by_info_set[info_set_id].len()
                }).max().unwrap()
            })
        }).collect();
        trace!("finish: build_max_action_size_of");
    }

    fn build_history(&mut self) {
        trace!("start: build_history");
        let mut actions: Vec<ActionId> = Vec::new();
        let mut history: BTreeMap<NodeId, Vec<ActionId>> = BTreeMap::new();
        self.build_history_inner(&self.root, &mut actions, &mut history);
        self.history = history;
        trace!("finish: build_history");
    }

    fn build_history_inner(&self, node_id: &NodeId, actions: &mut Vec<ActionId>, history: &mut BTreeMap<NodeId, Vec<ActionId>>) {
        history.insert(*node_id, actions.clone());
        if let Node::NonTerminal { edges, .. } = &self.nodes[node_id] {
            for (action_id, child_id) in edges.iter() {
                actions.push(*action_id);
                self.build_history_inner(child_id, actions, history);
                actions.pop();
            }
        }
    }

    pub fn info_set_by_id(&self, info_set_id: &InformationSetId) -> &InformationSet {
        &self.info_partitions[&self.player_by_info_set[info_set_id]][info_set_id]
    }

    pub fn bfs_ord(&self) -> Vec<NodeId> {
        let mut ord: Vec<NodeId> = Vec::new();
        let mut que: VecDeque<NodeId> = VecDeque::new();
        que.push_back(self.root);
        ord.push(self.root);
        while !que.is_empty() {                        
            let node_id = *que.front().unwrap();
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
pub fn from_json(json: &str) -> Rule {
    let mut rule: Rule = serde_json::from_str(json).expect("failed to deserialize json");
    rule.build();
    rule
}

pub fn from_file(path: &str) -> Rule {
    from_json(&fs::read_to_string(path).expect("failed to read file"))
}

pub fn from_name(rule_name: &str) -> Rule {
    match rule_name {
        "kuhn" => from_file("src/rule/kuhn.json"),
        "glico" => from_file("src/rule/glico.json"),
        _ => panic!("invalid rule name"),
    } 
}