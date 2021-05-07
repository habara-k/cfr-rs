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
pub fn sign_of(player: &Player) -> Option<Value> {
    match player {
        Player::P1 => Some(1.0),
        Player::P2 => Some(-1.0),
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

pub fn is_terminal(node: &Node) -> bool {
    match node {
        Node::Terminal{ .. } => true,
        Node::NonTerminal{ .. } => false,
    }
}

pub fn is_non_terminal(node: &Node) -> bool {
    match node {
        Node::Terminal{ .. } => false,
        Node::NonTerminal{ .. } => true,
    }
}

pub fn value_of(node: &Node) -> Option<NodeValue> {
    match node {
        Node::Terminal{ value } => Some(*value),
        Node::NonTerminal{ .. } => None,
    }
}
pub fn player_of(node: &Node) -> Option<Player> {
    match node {
        Node::Terminal{ .. } => None,
        Node::NonTerminal{ player, .. } => Some(player.clone()),
    }
}
pub fn edges_of(node: &Node) -> Option<BTreeMap<ActionId, NodeId>> {
    match node {
        Node::Terminal{ .. } => None,
        Node::NonTerminal{ edges, .. } => Some(edges.clone()),
    }
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
    #[serde(skip_deserializing)]
    node_value_scale: NodeValue,
    #[serde(skip_deserializing)]
    max_action_size_of: BTreeMap<Player, usize>,
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
        for (_, partition) in self.info_partitions.iter() {                
            for (info_set_id, info_set) in partition.iter() {
                for node_id in info_set.iter() {
                    self.info_set_id_by_node.insert(*node_id, *info_set_id);
                }
            }
        }

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
        
        self.node_value_scale = {
            let iter = self.nodes.iter().filter(|&(_, node)| {
                is_terminal(node)
            }).map(|(_, node)| {
                value_of(node).unwrap()
            });
            iter.clone().max().unwrap() - iter.min().unwrap()
        };
        
        self.max_action_size_of = [Player::P1, Player::P2].iter().map(|player| {
            (player.clone(), {
                self.info_partitions[player].iter().map(|(info_set_id, _)| {
                    self.actions_by_info_set[info_set_id].len()
                }).max().unwrap()
            })
        }).collect();
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

pub mod transition {
    use super::*;
    pub fn ones(rule: &Rule) -> Transition {
        rule.nodes.iter().filter(|&(_, node)| {
            is_non_terminal(node) && player_of(node).unwrap() == Player::C
        }).map(|(node_id, node)| {
            (node_id.clone(), {
                edges_of(node).unwrap().iter().map(|(action_id, _)| {
                    (action_id.clone(), 1.0)
                }).collect()
            })
        }).collect()
    }
}

pub mod strategy {
    use super::*;
    pub fn ones(rule: &Rule, player: &Player) -> Strategy {
        filled_with(rule, player, &1.0)
    }
    pub fn zeros(rule: &Rule, player: &Player) -> Strategy {
        filled_with(rule, player, &0.0)
    }
    pub fn filled_with(rule: &Rule, player: &Player, prob: &Value) -> Strategy {
        rule.info_partitions[player].iter().map(|(info_set_id, _)| {
            (info_set_id.clone(), {
                rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                    (action_id.clone(), prob.clone())
                }).collect()
            })
        }).collect()
    }
    pub fn uniform(rule: &Rule, player: &Player) -> Strategy {
        rule.info_partitions[player].iter().map(|(info_set_id, _)| {
            (info_set_id.clone(), {
                let prob = 1.0 / rule.actions_by_info_set[info_set_id].len() as Value;
                rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                    (action_id.clone(), prob.clone())
                }).collect()
            })
        }).collect()
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
    pub fn uniform(rule: &Rule) -> Profile {
        profile::from_strt(
            &Player::P1, strategy::uniform(rule, &Player::P1),
            &Player::P2, strategy::uniform(rule, &Player::P2)).unwrap()
    }
    pub fn from_strt(a: &Player, a_strt: Strategy, b: &Player, b_strt: Strategy) -> Option<Profile> {
        if *a == Player::C || *b == Player::C || *a == *b {
            return None
        }
        let mut prof = Profile::new();
        prof.insert(a.clone(), a_strt);
        prof.insert(b.clone(), b_strt);
        Some(prof)
    }

    pub fn prob_to_take_action(rule: &Rule, prof: &Profile, node_id: &NodeId, action_id: &ActionId) -> Option<Value> {
        match &rule.nodes[node_id] {
            Node::Terminal{ .. } => {
                None
            },
            Node::NonTerminal{ player, .. } => {
                Some(match player {
                    Player::P1 | Player::P2 => prof[player][&rule.info_set_id_by_node[node_id]][action_id],
                    Player::C => rule.transition[node_id][action_id],
                })
            },
        }
    }
}

pub mod solver {
    use super::*;

    pub fn calc_ev(rule: &Rule, prof: &Profile) -> Value {
        calc_ev_inner(rule, prof, &rule.root, 1.0)
    }

    fn calc_ev_inner(rule: &Rule, prof: &Profile, node_id: &NodeId, prob: Value) -> Value {            
        match &rule.nodes[node_id] {
            Node::Terminal{ value } => {                    
                *value as Value * prob
            },
            Node::NonTerminal{ edges, .. } => {
                edges.iter().map(|(action_id, child_id)| {
                    calc_ev_inner(rule, prof, child_id,  prob) * profile::prob_to_take_action(
                        rule, prof, node_id, action_id).unwrap()
                }).sum()
            },
        }
    }

    pub fn calc_best_resp_against_to(rule: &Rule, opponent: &Player, opp_strt: Strategy) -> (Strategy, Value) {
        let myself = opponent_of(opponent).unwrap();
        let prof = profile::from_strt(
            &myself, strategy::ones(rule, &myself),
            opponent,  opp_strt).unwrap();

        let prob_to_reach_terminal_node = calc_prob_to_reach_terminal_node(rule, &prof);

        let best_action_at = |vals: &BTreeMap<NodeId, Value>, info_set_id: &InformationSetId| -> ActionId {                
            assert_eq!(myself, rule.player_by_info_set[info_set_id]);
            rule.actions_by_info_set[info_set_id].iter()
                .ord_subset_max_by_key(
                    |action_id| -> Value {
                        rule.info_partitions[&myself][info_set_id].iter().map(|node_id| {                                
                            vals[&edges_of(&rule.nodes[node_id]).unwrap()[action_id]]
                        }).sum::<Value>() * sign_of(&myself).unwrap()
                    }
                ).unwrap().clone()
        };

        let ord = rule.bfs_ord();
        let mut vals: BTreeMap<NodeId, Value> = BTreeMap::new();
        let mut best_actions: BTreeMap<InformationSetId, ActionId> = BTreeMap::new();
        let mut best_strt: Strategy = strategy::zeros(&rule, &myself);

        for node_id in ord.iter().rev() {                        
            match &rule.nodes[node_id] {
                Node::Terminal{ value} => {
                    vals.insert(*node_id, (*value as Value) * prob_to_reach_terminal_node[node_id]);
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

        (best_strt, vals[&rule.root])
    }

    fn calc_prob_to_reach_terminal_node(rule: &Rule, prof: &Profile) -> BTreeMap<NodeId, Value> {
        let mut probs: BTreeMap<NodeId, Value> = BTreeMap::new();
        calc_prob_to_reach_terminal_node_inner(&mut probs, rule, prof, &rule.root, 1.0);
        probs
    }

    fn calc_prob_to_reach_terminal_node_inner(probs: &mut BTreeMap<NodeId, Value>, rule: &Rule, prof: &Profile, node_id: &NodeId, prob: Value) {
        match &rule.nodes[node_id] {
            Node::Terminal{ .. } => {
                probs.insert(*node_id, prob);
            },                
            Node::NonTerminal{ edges, .. } => {
                for (action_id, child_id) in edges.iter() {
                    calc_prob_to_reach_terminal_node_inner(
                        probs, rule, prof, child_id, 
                        prob * profile::prob_to_take_action(rule, prof, node_id, action_id).unwrap()
                        );
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

pub mod visualizer {
    use super::*;
    pub fn print(rule: &Rule, prof: &Profile) {
        print_inner(rule, prof, &rule.root, "".to_string());
    }

    fn print_inner(rule: &Rule, prof: &Profile, node_id: &NodeId, margin: String) {
        if let Node::NonTerminal{ edges, .. } = &rule.nodes[node_id] {
            for (i, (action_id, child_id)) in edges.iter().enumerate() {
                println!("{}|- {} ({})", &margin, rule.actions[action_id], profile::prob_to_take_action(rule, prof, node_id, action_id).unwrap());
                print_inner(rule, prof, child_id, margin.clone() +
                     if i+1 == edges.len() { "   " } else { "|  " });
            }
        }
    }
}


pub mod cfr {
    use super::*;

    type RegretType = BTreeMap<InformationSetId, BTreeMap<ActionId, Value>>;

    fn positive_part(v: BTreeMap<ActionId, Value>) -> BTreeMap<ActionId, Value> {
        v.into_iter().map(|(action_id, prob)| 
            (action_id, if prob < 0.0 { 0.0 } else { prob })
        ).collect()
    }
    fn normalized(v: BTreeMap<ActionId, Value>) -> BTreeMap<ActionId, Value> {
        let eps = 1e-9;
        let norm: Value = v.iter().map(|(_, prob)| prob).sum();
        let len = v.len() as Value;
        v.into_iter().map(|(action_id, prob)| 
            (action_id, if norm.abs() < eps { 1.0 / len } else { prob / norm })
        ).collect()
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

        println!("exploitabilyty: {}", solver::calc_exploitability(rule, &avg_prof));

        let exploitability_upper_bound = |t: Value| {
            let a1 = rule.info_partitions[&Player::P1].len() as Value * 
                (rule.max_action_size_of[&Player::P1] as Value).sqrt();
            let a2 = rule.info_partitions[&Player::P2].len() as Value * 
                (rule.max_action_size_of[&Player::P2] as Value).sqrt();

            let max = if a1 < a2 { a2 } else { a1 };

            4.0 * max / t.sqrt()
        };

        for t in 1..step+1 {
            regret = regret.iter().map(|(myself, reg)| {
                (myself.clone(), {                        
                    let prob_to_reach_node_except_myself = calc_prob_to_reach_node_except(rule, &latest_prof, myself);

                    let ev_under_node_for_myself = calc_ev_under_node_for_player(rule, &latest_prof, myself);

                    let s: RegretType = rule.info_partitions[myself].iter().map(|(info_set_id, node_ids)| {
                        (info_set_id.clone(), rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                            (action_id.clone(), node_ids.iter().map(|node_id| {
                                prob_to_reach_node_except_myself[node_id] * ev_under_node_for_myself[
                                    &edges_of(&rule.nodes[node_id]).unwrap()[action_id]
                                ]
                            }).sum())
                        }).collect())
                    }).collect();

                    let u: BTreeMap<InformationSetId, Value> = rule.info_partitions[myself].iter().map(|(info_set_id, _)| {
                        (info_set_id.clone(), rule.actions_by_info_set[info_set_id].iter().map(|action_id| {
                            latest_prof[myself][info_set_id][action_id] * s[info_set_id][action_id]
                        }).sum())
                    }).collect();

                    let t = t as Value;
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
                                    let total = avg_prob_to_reach_info_set_only_myself[info_set_id] * t as Value;
                                    (avg_prof[myself][info_set_id][action_id] * total +
                                    latest_prof[myself][info_set_id][action_id] * latest) / (latest + total)
                                })
                            }).collect()
                        })
                    }).collect()
                })
            }).collect();

            if t % 1000 == 0 {
                println!("exploitabilyty: {} / ub: {}",
                    solver::calc_exploitability(rule, &avg_prof),
                    exploitability_upper_bound(t as Value)
                );
            }
        }

        avg_prof
    }

    fn calc_prob_to_reach_info_set_only(rule: &Rule, prof: &Profile, myself: &Player) -> BTreeMap<InformationSetId, Value> {
        let probs = calc_prob_to_reach_node_only(rule, prof, myself);
        rule.info_partitions[myself].iter().map(|(info_set_id, nodes)| {
            (info_set_id.clone(), 
                nodes.iter().map(|node_id| probs[node_id]).sum()
            )
        }).collect()
    }

    fn calc_prob_to_reach_node_only(rule: &Rule, prof: &Profile, myself: &Player) -> BTreeMap<NodeId, Value> {
        let mut probs: BTreeMap<NodeId, Value> = BTreeMap::new();
        let opponent = opponent_of(myself).unwrap();
        calc_prob_to_reach_node_inner(&mut probs, rule, &profile::from_strt(
            myself, prof[myself].clone(), &opponent, strategy::ones(rule, &opponent)
        ).unwrap(), &transition::ones(rule), &rule.root, 1.0);
        probs
    }

    fn calc_prob_to_reach_node_except(rule: &Rule, prof: &Profile, myself: &Player) -> BTreeMap<NodeId, Value> {
        let mut probs: BTreeMap<NodeId, Value> = BTreeMap::new();
        let opponent = opponent_of(myself).unwrap();
        calc_prob_to_reach_node_inner(&mut probs, rule, &profile::from_strt(
            myself, strategy::ones(rule, myself), &opponent, prof[&opponent].clone()
        ).unwrap(), &rule.transition, &rule.root, 1.0);
        probs
    }

    fn calc_prob_to_reach_node_inner(probs: &mut BTreeMap<NodeId, Value>, rule: &Rule, prof: &Profile, trans: &Transition, node_id: &NodeId, prob: Value) {
        probs.insert(*node_id, prob);
        if let Node::NonTerminal{ edges, .. } = &rule.nodes[node_id] {
            for (action_id, child_id) in edges {
                calc_prob_to_reach_node_inner(probs, rule, prof, trans, child_id, 
                    prob * profile::prob_to_take_action(rule, prof, node_id, action_id).unwrap()
                );
            }
        }
    }

    fn calc_ev_under_node_for_player(rule: &Rule, prof: &Profile, myself: &Player) -> BTreeMap<NodeId, Value> {
        let mut ev: BTreeMap<NodeId, Value> = BTreeMap::new();
        calc_ev_under_node_inner_for_player(&mut ev, rule, prof, &rule.root, myself);
        ev
    }
    fn calc_ev_under_node_inner_for_player(evs: &mut BTreeMap<NodeId, Value>, rule: &Rule, prof: &Profile, node_id: &NodeId, myself: &Player) {
        let val: Value = match &rule.nodes[node_id] {
            Node::Terminal{ value } => {
                *value as Value * sign_of(myself).unwrap()
            },
            Node::NonTerminal{ edges, .. } => {
                edges.iter().map(|(action_id, child_id)| {
                     calc_ev_under_node_inner_for_player(evs, rule, prof, child_id, myself);
                     evs[child_id] * profile::prob_to_take_action(rule, prof, node_id, action_id).unwrap()
                }).sum()
            }
        };
        evs.insert(*node_id, val);
    }
}