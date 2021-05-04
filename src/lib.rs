use std::collections::{BTreeMap,VecDeque};
use serde::{Deserialize, Serialize};
use serde_json;
use std::fs;
use std::cmp;
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

pub mod transition {
    use super::*;
    pub fn ones(rule: &Rule) -> Transition {
        rule.nodes.iter().filter(|&(node_id, node)| {
            is_non_terminal(&node) && player_of(&node).unwrap() == Player::C
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
    pub fn uniform(rule: &Rule) -> Profile {
        profile::from_strt(
            &Player::P1, strategy::uniform(&rule, &Player::P1),
            &Player::P2, strategy::uniform(&rule, &Player::P2)).unwrap()
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
                // TODO: refactor
                sum
            },
        }
    }

    pub fn calc_best_resp_against_to(rule: &Rule, opponent: &Player, opp_strt: Strategy) -> (Strategy, Value) {
        let myself = opponent_of(&opponent).unwrap();
        let prof = profile::from_strt(
            &myself, strategy::ones(rule, &myself),
            &opponent,  opp_strt).unwrap();

        let terminal_probs = calc_terminal_probs(&rule, &prof);

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

    fn calc_terminal_probs(rule: &Rule, prof: &Profile) -> BTreeMap<NodeId, Value> {
        let mut terminal_probs: BTreeMap<NodeId, Value> = BTreeMap::new();
        calc_terminal_probs_inner(&mut terminal_probs, &rule, &prof, &rule.root, 1.0);
        terminal_probs
    }

    fn calc_terminal_probs_inner(terminal_probs: &mut BTreeMap<NodeId, Value>, rule: &Rule, prof: &Profile, node_id: &NodeId, prob: Value) {
        match &rule.nodes[&node_id] {
            Node::Terminal{ .. } => {
                terminal_probs.insert(*node_id, prob);
            },                
            Node::NonTerminal{ player, edges} => {
                for (action_id, child_id) in edges.iter() {
                    calc_terminal_probs_inner(
                        terminal_probs, rule, prof, child_id, 
                        prob * match player {
                            Player::P1 | Player::P2 => {
                                prof[player][&rule.info_set_id_by_node[&node_id]][action_id]
                            },
                            Player::C => {
                                rule.transition[&node_id][action_id]
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


pub mod cfr {
    use approx_eq::assert_approx_eq;

    use super::*;

    type RegretType = BTreeMap<InformationSetId, BTreeMap<ActionId, Value>>;

    fn positive_part(v: BTreeMap<ActionId, Value>) -> BTreeMap<ActionId, Value> {
        v.into_iter().map(|(action_id, prob)| 
            (action_id, if prob < 0.0 { 0.0 } else { prob })
        ).collect()
    }
    fn normalized(v: BTreeMap<ActionId, Value>) -> BTreeMap<ActionId, Value> {
        let norm: Value = v.iter().map(|(_, prob)| prob).sum();
        let len = v.len() as Value;
        v.into_iter().map(|(action_id, prob)| 
            (action_id, if norm.abs() < 1e-9 { 1.0 / len } else { prob / norm })
        ).collect()
    }


    pub fn calc_nash_strt(rule: &Rule) -> Profile {
        let mut regret: BTreeMap<Player, RegretType> = rule.info_partitions.iter().map(|(player, partition)| {
            (player.clone(), partition.iter().map(|(info_id, _)| {
                (info_id.clone(), rule.actions_by_info_set[&info_id].iter().map(|action_id| {
                    (action_id.clone(), 0.0)
                }).collect())
            }).collect())
        }).collect();

        let mut latest_prof: Profile = profile::uniform(&rule);
        let mut avg_prof = latest_prof.clone();


        println!("exploitabilyty: {}", solver::calc_exploitability(&rule, &avg_prof));

        let steps = 10000;
        for t in 1..steps+1 {
            regret = regret.iter().map(|(myself, reg)| {
                (myself.clone(), {                        
                    let prob_to_reach_node_except_myself = calc_prob_to_reach_node_except(rule, &latest_prof, myself);
                    // println!("prob_to_reach_node_except_myself: {:?}", prob_to_reach_node_except_myself);

                    let ev_under_node_for_myself = calc_ev_under_node_for_player(rule, &latest_prof, myself);
                    // println!("ev_under_node_for_myself: {:?}", ev_under_node_for_myself);

                    let s: RegretType = rule.info_partitions[myself].iter().map(|(info_id, node_ids)| {
                        (info_id.clone(), rule.actions_by_info_set[info_id].iter().map(|action_id| {
                            (action_id.clone(), node_ids.iter().map(|node_id| {
                                prob_to_reach_node_except_myself[node_id] * match &rule.nodes[node_id] {
                                    Node::NonTerminal{ edges, .. } => {
                                        ev_under_node_for_myself[&edges[action_id]]
                                    },
                                    Node::Terminal{ .. } => panic!()
                                    // TODO: refactor
                                }
                            }).sum())
                        }).collect())
                    }).collect();
                    // println!("s: {:?}", s);

                    let u: BTreeMap<InformationSetId, Value> = rule.info_partitions[myself].iter().map(|(info_id, _)| {
                        (info_id.clone(), rule.actions_by_info_set[info_id].iter().map(|action_id| {
                            latest_prof[myself][info_id][action_id] * s[info_id][action_id]
                        }).sum())
                    }).collect();
                    // println!("u: {:?}", u);

                    let t = t as Value;
                    rule.info_partitions[myself].iter().map(|(info_id, _)| {
                        (info_id.clone(), rule.actions_by_info_set[info_id].iter().map(|action_id| {
                            (action_id.clone(), (t * reg[info_id][action_id] + s[info_id][action_id] - u[info_id]) / (t + 1.0))
                        }).collect())
                    }).collect()
                })
            }).collect();

            // println!("regret: {:?}", regret);

            // update latest_prof
            latest_prof = regret.iter().map(|(myself, reg)| {
                (myself.clone(), {
                    reg.iter().map(|(info_id, dist)| {
                        (info_id.clone(), normalized(positive_part(dist.clone()).clone()))
                    }).collect()
                })
            }).collect();
            // println!("latest_prof: {:?}", latest_prof);


            avg_prof = [Player::P1, Player::P2].iter().map(|myself| {
                let latest_prob_to_reach_info_set_only_myself = calc_prob_to_reach_info_set_only(rule, &latest_prof, myself);
                let avg_prob_to_reach_info_set_only_myself = calc_prob_to_reach_info_set_only(rule, &avg_prof, myself);
                // println!("latest_prob_to: {:?}", latest_prob_to_reach_info_set);
                // println!("avg_prob_to: {:?}", avg_prob_to_reach_info_set);
                (myself.clone(), {
                    rule.info_partitions[myself].iter().map(|(info_id, _)| {                            
                        (info_id.clone(), {
                            rule.actions_by_info_set[info_id].iter().map(|action_id| {
                                (action_id.clone(), {
                                    let latest = latest_prob_to_reach_info_set_only_myself[info_id];
                                    let total = avg_prob_to_reach_info_set_only_myself[info_id] * t as Value;
                                    (avg_prof[myself][info_id][action_id] * total +
                                    latest_prof[myself][info_id][action_id] * latest) / (latest + total)
                                })
                            }).collect()
                        })
                    }).collect()
                })
            }).collect();
            // println!("avg_prof: {:?}", avg_prof);

            println!("exploitabilyty: {}", solver::calc_exploitability(rule, &avg_prof));
        }

        avg_prof
    }

    fn calc_prob_to_reach_info_set_only(rule: &Rule, prof: &Profile, myself: &Player) -> BTreeMap<InformationSetId, Value> {
        let probs = calc_prob_to_reach_node_only(rule, prof, myself);
        rule.info_partitions[myself].iter().map(|(info_id, nodes)| {
            (info_id.clone(), 
                nodes.iter().map(|node_id| probs[node_id]).sum()
            )
        }).collect()
    }

    // fn calc_prob_to_reach_node(rule: &Rule, prof: &Profile) -> BTreeMap<NodeId, Value> {
    //     let mut probs: BTreeMap<NodeId, Value> = BTreeMap::new();
    //     calc_prob_to_reach_node_inner(&mut probs, rule, prof, &rule.transition, &rule.root, 1.0);
    //     probs
    // }

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
        if let Node::NonTerminal{ player, edges } = &rule.nodes[node_id] {
            for (action_id, child_id) in edges {
                calc_prob_to_reach_node_inner(probs, rule, prof, trans, child_id, prob * match player {
                    Player::P1 | Player::P2 => {
                        prof[player][&rule.info_set_id_by_node[node_id]][action_id]
                    },
                    Player::C => {
                        trans[node_id][action_id]
                    }
                });
            }
        }
    }

    fn calc_ev_under_node_for_player(rule: &Rule, prof: &Profile, myself: &Player) -> BTreeMap<NodeId, Value> {
        let mut ev: BTreeMap<NodeId, Value> = BTreeMap::new();
        calc_ev_under_node_inner_for_player(&mut ev, rule, prof, &rule.root, myself);
        ev
    }
    fn calc_ev_under_node_inner_for_player(evs: &mut BTreeMap<NodeId, Value>, rule: &Rule, prof: &Profile, node_id: &NodeId, myself: &Player) {
        match &rule.nodes[node_id] {
            Node::Terminal{ value } => {
                evs.insert(*node_id, *value as Value * match myself {
                    Player::P1 => 1.0,
                    Player::P2 => -1.0,
                    Player::C => panic!()
                    // TODO: refactor
                });
            },
            Node::NonTerminal{ player, edges } => {
                let sum: Value = edges.iter().map(|(action_id, child_id)| {
                    calc_ev_under_node_inner_for_player(evs, rule, prof, child_id, myself);
                    evs[child_id] * match player {
                        Player::P1 | Player::P2 => {
                            prof[player][&rule.info_set_id_by_node[node_id]][action_id]
                        },
                        Player::C => {
                            rule.transition[node_id][action_id]
                        }
                    }
                }).sum();
                evs.insert(*node_id, sum);
            }
        }
    }
}