use std::collections::{BTreeMap,VecDeque};
use serde::{Deserialize, Serialize};
use serde_json;
use std::fs;
use ord_subset::OrdSubsetIterExt;
use indicatif::ProgressIterator;

#[macro_use]
extern crate log;

pub mod action {
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Debug)]
    pub struct ActionId(usize);

    #[derive(Serialize, Deserialize, Debug)]
    pub struct Action(String);
}

pub use action::*;

pub mod player {
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize, Debug)]
    pub enum Player {
        P1,
        P2,
        C,
    }

    impl Player {
        pub fn opponent(&self) -> Player {
            match self {
                Player::P1 => Player::P2,
                Player::P2 => Player::P1,
                Player::C => panic!("Player::C has no opponent"),
            }
        }
        pub fn sign(&self) -> i32 {
            match self {
                Player::P1 => 1,
                Player::P2 => -1,
                Player::C => panic!("Player::C has no sign"),
            }
        }
    }
}

pub use player::*;

pub mod node {
    use std::collections::BTreeMap;
    use serde::{Deserialize, Serialize};
    use super::{player::Player, action::ActionId};

    #[derive(Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Debug)]
    pub struct NodeId(usize);

    pub type NodeValue = i32;

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

    impl Node {
        pub fn is_terminal(&self) -> bool {
            match self {
                Node::Terminal{ .. } => true,
                Node::NonTerminal{ .. } => false,
            }
        }

        pub fn is_non_terminal(&self) -> bool {
            match self {
                Node::Terminal{ .. } => false,
                Node::NonTerminal{ .. } => true,
            }
        }

        pub fn value(&self) -> NodeValue {
            match self {
                Node::Terminal{ value } => *value,
                Node::NonTerminal{ .. } => panic!("non terminal has no value"),
            }
        }
        pub fn player(&self) -> Player {
            match self {
                Node::Terminal{ .. } => panic!("terminal has no player"),
                Node::NonTerminal{ player, .. } => *player,
            }
        }
        pub fn edges(&self) -> BTreeMap<ActionId, NodeId> {
            match self {
                Node::Terminal{ .. } => panic!("terminal has no edges"),
                Node::NonTerminal{ edges, .. } => edges.clone(),
            }
        }
    }
}
pub use node::{Node, NodeId, NodeValue};


pub mod rule {
    use std::collections::{BTreeMap, VecDeque};
    use std::fs;
    use serde::{Deserialize, Serialize};
    use super::{
        action::{Action, ActionId},
        node::{Node, NodeId, NodeValue},
        player::Player,
        transition::Transition,  // Interdependent
    };

    pub type InformationSet = Vec<NodeId>;

    #[derive(Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Debug)]
    pub struct InformationSetId(usize);

    pub type InformationPartition = BTreeMap<InformationSetId, InformationSet>;

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
}

pub use rule::{Rule, InformationSetId};

pub mod transition {
    use std::collections::BTreeMap;
    use super::{
        action::ActionId,
        player::Player,
        node::NodeId,
        rule::Rule,  // Interdependent
    };
    pub type Transition = BTreeMap<NodeId, BTreeMap<ActionId, f64>>;

    pub fn ones(rule: &Rule) -> Transition {
        rule.nodes.iter().filter(|&(_, node)| {
            node.is_non_terminal() && node.player() == Player::C
        }).map(|(node_id, node)| {
            (*node_id, {
                node.edges().iter().map(|(action_id, _)| {
                    (*action_id, 1.0)
                }).collect()
            })
        }).collect()
    }
}
pub use transition::Transition;

pub mod strategy {
    use std::collections::BTreeMap;
    use super::{
        action::ActionId,
        player::Player,
        rule::{Rule, InformationSetId},
    };
    pub type Strategy = BTreeMap<InformationSetId, BTreeMap<ActionId, f64>>;

    pub fn ones(rule: &Rule, player: &Player) -> Strategy {
        filled_with(rule, player, &1.0)
    }
    pub fn zeros(rule: &Rule, player: &Player) -> Strategy {
        filled_with(rule, player, &0.0)
    }
    pub fn filled_with(rule: &Rule, player: &Player, prob: &f64) -> Strategy {
        rule.info_partitions[player].iter().map(|(info_set_id, _)| {
            (*info_set_id, {
                rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                    (*action_id, *prob)
                }).collect()
            })
        }).collect()
    }
    pub fn uniform(rule: &Rule, player: &Player) -> Strategy {
        rule.info_partitions[player].iter().map(|(info_set_id, _)| {
            (*info_set_id, {
                let prob = 1.0 / rule.actions_by_info_set[info_set_id].len() as f64;
                rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                    (*action_id, prob)
                }).collect()
            })
        }).collect()
    }
}
pub use strategy::Strategy;

pub mod profile {
    // use super::*;
    use std::collections::BTreeMap;
    use std::fs;
    use super::{
        action::ActionId,
        player::Player,
        node::{Node, NodeId},
        strategy::{self, Strategy},
        rule::Rule,
    };
    pub type Profile = BTreeMap<Player,Strategy>;
    pub fn from_json(json: &str) -> Profile {
        let prof: Profile = serde_json::from_str(json).expect("failed to deserialize json");
        prof
    }

    pub fn from_file(path: &str) -> Profile {
        from_json(&fs::read_to_string(path).expect("failed to read file"))
    }

    pub fn from_name(prof_name: &str) -> Profile {
        match prof_name {
            "kuhn_nash" => from_file("src/profile/kuhn_nash.json"),
            "glico_nash" => from_file("src/profile/glico_nash.json"),
            _ => panic!("invalid profile name"),
        }
    }
    pub fn uniform(rule: &Rule) -> Profile {
        from_strt(
            &Player::P1, strategy::uniform(rule, &Player::P1),
            &Player::P2, strategy::uniform(rule, &Player::P2))
    }
    pub fn from_strt(a: &Player, a_strt: Strategy, b: &Player, b_strt: Strategy) -> Profile {
        if *a == Player::C || *b == Player::C || *a == *b {
            panic!("invalid arguments");
        }
        let mut prof = Profile::new();
        prof.insert(a.clone(), a_strt);
        prof.insert(b.clone(), b_strt);
        prof
    }

    pub fn prob_to_take_action(rule: &Rule, prof: &Profile, node_id: &NodeId, action_id: &ActionId) -> f64 {
        match &rule.nodes[node_id] {
            Node::Terminal{ .. } => {
                panic!("terminal has no action");
            },
            Node::NonTerminal{ player, .. } => {
                match player {
                    Player::P1 | Player::P2 => prof[player][&rule.info_set_id_by_node[node_id]][action_id],
                    Player::C => rule.transition[node_id][action_id],
                }
            },
        }
    }
}
use profile::Profile;

pub mod solver {
    use super::*;

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
}

pub mod visualizer {
    use super::*;

    pub fn print_node(rule: &Rule, node_id: &NodeId) {
        print!("[");
        for (i, action_id) in rule.history[node_id].iter().enumerate() {
            if i+1 == rule.history[node_id].len() {
                print!("{:?}", rule.actions[action_id]);
            } else {
                print!("{:?}, ", rule.actions[action_id]);
            }
        }
        print!("]")
    }

    pub fn print_info_set(rule: &Rule, info_set_id: &InformationSetId) {
        println!("[");
        for node_id in rule.info_set_by_id(info_set_id).iter() {
            print!("    ");
            print_node(rule, node_id);
            println!(",");
        }
        print!("  ]");
    }

    pub fn print_dist(rule: &Rule, dist: &BTreeMap<ActionId, f64>) {
        println!("{{");
        for (action_id, prob) in dist.iter() {
            println!("    {:?}: {}", rule.actions[action_id], prob);
        }
        print!("  }}");
    }

    pub fn print_prof(rule: &Rule, prof: &Profile) {
        for player in [Player::P1, Player::P2].iter() {
            println!("{:?}: {{", player);
            for (info_set_id, dist) in prof[player].iter() {
                print!("  ");
                print_info_set(rule, info_set_id);
                print!(": ");
                print_dist(rule, dist);
                println!(",");
            }
            println!("}}");
        }
    }
}


pub mod cfr {
    use super::*;

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
            (player.clone(), partition.iter().map(|(info_set_id, _)| {
                (info_set_id.clone(), rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                    (action_id.clone(), 0.0)
                }).collect())
            }).collect())
        }).collect();

        let mut latest_prof = init_prof;
        let mut avg_prof = latest_prof.clone();

        for t in (1..step+1).progress() {
            regret = regret.iter().map(|(myself, reg)| {
                (myself.clone(), {                        
                    let prob_to_reach_node_except_myself = calc_prob_to_reach_node_except(rule, &latest_prof, myself);

                    let ev_under_node_for_myself = calc_ev_under_node_for_player(rule, &latest_prof, myself);

                    let s: RegretType = rule.info_partitions[myself].iter().map(|(info_set_id, node_ids)| {
                        (info_set_id.clone(), rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                            (action_id.clone(), node_ids.iter().map(|node_id| {
                                prob_to_reach_node_except_myself[node_id] * ev_under_node_for_myself[
                                    &rule.nodes[node_id].edges()[action_id]
                                ]
                            }).sum())
                        }).collect())
                    }).collect();

                    let u: BTreeMap<InformationSetId, f64> = rule.info_partitions[myself].iter().map(|(info_set_id, _)| {
                        (info_set_id.clone(), rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                            latest_prof[myself][info_set_id][action_id] * s[info_set_id][action_id]
                        }).sum())
                    }).collect();

                    let t = t as f64;
                    rule.info_partitions[myself].iter().map(|(info_set_id, _)| {
                        (info_set_id.clone(), rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                            (action_id.clone(), (t * reg[info_set_id][action_id] + s[info_set_id][action_id] - u[info_set_id]) / (t + 1.0))
                        }).collect())
                    }).collect()
                })
            }).collect();


            latest_prof = regret.iter().map(|(myself, reg)| {
                (myself.clone(), {
                    reg.iter().map(|(info_set_id, dist)| {
                        (info_set_id.clone(), normalized(positive_part(dist.clone()).clone()))
                    }).collect()
                })
            }).collect();


            avg_prof = [Player::P1, Player::P2].iter().map(|myself| {
                let latest_prob_to_reach_info_set_only_myself = calc_prob_to_reach_info_set_only(rule, &latest_prof, myself);
                let avg_prob_to_reach_info_set_only_myself = calc_prob_to_reach_info_set_only(rule, &avg_prof, myself);
                (myself.clone(), {
                    rule.info_partitions[myself].iter().map(|(info_set_id, _)| {                            
                        (info_set_id.clone(), {
                            rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                                (action_id.clone(), {
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
        if let Node::NonTerminal{ player, edges } = &rule.nodes[node_id] {
            if player == myself {
                *probs.entry(rule.info_set_id_by_node[node_id]).or_insert(0.0) += prob;
            }
            for (action_id, child_id) in edges {
                calc_prob_to_reach_info_set_only_inner(
                    probs,
                    rule,
                    prof,
                    child_id,
                    prob * if player == myself { profile::prob_to_take_action(rule, prof, node_id, action_id) } else { 1.0 },
                    myself,
                );
            }
        }
    }

    fn calc_prob_to_reach_node_except(rule: &Rule, prof: &Profile, myself: &Player) -> BTreeMap<NodeId, f64> {
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
        if let Node::NonTerminal{ player, edges } = &rule.nodes[node_id] {
            for (action_id, child_id) in edges {
                calc_prob_to_reach_node_except_inner(
                    probs,
                    rule,
                    prof,
                    child_id,
                    prob * if player == myself { 1.0 } else { profile::prob_to_take_action(rule, prof, node_id, action_id) },
                    myself,
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
}