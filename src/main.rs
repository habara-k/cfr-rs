use std::collections::{BTreeMap,VecDeque};
use serde::{Deserialize, Serialize};
use serde_json;
use std::fs;
use ord_subset::{OrdSubsetIterExt};

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Deserialize, Serialize, Debug)]
enum Player {
    P1,
    P2,
    C,
}

struct PlayerUtil {}
impl PlayerUtil {
    fn opponent(player: &Player) -> Option<Player> {
        match player {
            Player::P1 => Some(Player::P2),
            Player::P2 => Some(Player::P1),
            Player::C => None,
        }
    }
}

type Action = String;

type NodeValue = i32;
type NodeId = usize;
type InformationSetId = usize;
type ActionId = usize;

type InformationSet = Vec<NodeId>;
type InformationPartition = BTreeMap<InformationSetId, InformationSet>;


#[derive(Clone, Serialize, Deserialize)]
enum Node {
    Terminal {
        value: NodeValue,
    },
    NonTerminal {
        player: Player,
        edges: BTreeMap<ActionId, NodeId>,
    },
}


#[derive(Deserialize, Serialize)]
struct Rule {
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
    fn new() -> Self {
        let mut rule: Rule = serde_json::from_str(
            &fs::read_to_string("src/rule/kuhn.json").unwrap()
        ).unwrap();

        // build info_set_id_by_node
        for (_, partition) in rule.info_partitions.iter() {                
            for (info_set_id, info_set) in partition.iter() {
                for node_id in info_set.iter() {
                    rule.info_set_id_by_node.insert(*node_id, *info_set_id);
                }
            }
        }

        // build actions_by_info_set, player_by_info_set
        for (_, partition) in rule.info_partitions.iter() {                
            for (info_set_id, info_set) in partition.iter() {
                for node_id in info_set.iter() {                        
                    if let Node::NonTerminal{ player, edges} = &rule.nodes[node_id] {
                        let actions: Vec<ActionId> = edges.keys().cloned().collect();
                        if rule.actions_by_info_set.contains_key(info_set_id) {
                            assert_eq!(actions, rule.actions_by_info_set[info_set_id]);
                        } else {
                            rule.actions_by_info_set.insert(*info_set_id, actions);
                        }

                        if rule.player_by_info_set.contains_key(info_set_id) {
                            assert_eq!(*player, rule.player_by_info_set[info_set_id]);
                        } else {
                            rule.player_by_info_set.insert(*info_set_id, player.clone());
                        }
                    }
                }
            }
        }

        // debug
        let json = serde_json::to_string(&rule).unwrap();
        println!("{}", json);

        return rule
    }

    fn bfs_ord(&self) -> Vec<NodeId> {
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

type Value = f64;
type Distribution = BTreeMap<ActionId, Value>;
type Transition = BTreeMap<NodeId, Distribution>;
type Strategy = BTreeMap<InformationSetId, Distribution>;
type Profile = BTreeMap<Player,Strategy>;

struct StrategyUtil {}

impl StrategyUtil {
    fn ones(rule: &Rule, player: &Player) -> Strategy {
        let mut strt = Strategy::new();
        for (info_set_id, _) in rule.info_partitions[&player].iter() {                
            strt.insert(
                *info_set_id, 
                rule.actions_by_info_set[info_set_id].iter().cloned().map(|action| (action, 1.0)).collect()
            );
        }
        strt
    }
    fn uniform(rule: &Rule, player: &Player) -> Strategy {
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

struct ProfileUtil {}
impl ProfileUtil {
    fn from_strt(p1: &Player, p1_strategy: Strategy, p2: &Player, p2_strategy: Strategy) -> Option<Profile> {
        if *p1 == Player::C || *p2 == Player::C || *p1 == *p2 {
            return None
        }
        let mut profile = Profile::new();
        profile.insert(p1.clone(), p1_strategy);
        profile.insert(p2.clone(), p2_strategy);
        Some(profile)
    }
}

struct Game {}

impl Game {
    fn calc_ev(rule: &Rule, profile: &Profile) -> Value {
        Self::calc_ev_inner(rule, profile, &rule.root, 1.0)
    }
    fn calc_ev_inner(rule: &Rule, profile: &Profile, node_id: &NodeId, prob: Value) -> Value {            
        match &rule.nodes[node_id] {
            Node::Terminal{ value } => {                    
                *value as Value * prob
            },
            Node::NonTerminal{ player, edges} => {
                let mut sum: Value = 0.0;
                for (action_id, child_id) in edges.iter() {                        
                    sum = sum + Self::calc_ev_inner(rule, profile, child_id,  prob) * match player {
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

    fn calc_best_resp_against_to(rule: &Rule, opponent: &Player, opp_strt: Strategy) -> Value {
        let myself = PlayerUtil::opponent(&opponent).unwrap();
        let prof = ProfileUtil::from_strt(
            &myself, StrategyUtil::ones(rule, &myself),
            &opponent,  opp_strt).unwrap();

        println!("prof: {:?}", prof);

        let mut terminal_probs: BTreeMap<NodeId, Value> = BTreeMap::new();
        Self::calc_terminal_probs(&mut terminal_probs, &rule, &prof, &rule.root, 1.0);

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
        let mut best_strt: BTreeMap<InformationSetId, ActionId> = BTreeMap::new();

        for node_id in ord.iter().rev() {                        
            match &rule.nodes[node_id] {
                Node::Terminal{ value} => {
                    vals.insert(*node_id, (*value as Value) * terminal_probs[&node_id]);
                },
                Node::NonTerminal{ player, edges} => {                                
                    vals.insert(*node_id, if *player == myself {
                            let info_set_id = &rule.info_set_id_by_node[node_id];
                            if !best_strt.contains_key(&info_set_id) {
                                best_strt.insert(*info_set_id, best_action(&vals, &info_set_id));
                            }
                            vals[&edges[&best_strt[&info_set_id]]]
                        } else {
                            edges.iter().map(|(_, child_id)| vals[child_id]).sum()
                        });
                }
            }
        }
        println!("best_strt: {:?}", best_strt);
        println!("vals: {:?}", vals);
        println!("terminal_probs: {:?}", terminal_probs);

        vals[&rule.root]
    }

    fn calc_terminal_probs(terminal_probs: &mut BTreeMap<NodeId, Value>, rule: &Rule, profile: &Profile, node_id: &NodeId, prob: Value) {
        // println!("node_id: {:?}", node_id);
        match &rule.nodes[&node_id] {
            Node::Terminal{ .. } => {
                terminal_probs.insert(*node_id, prob);
            },                
            Node::NonTerminal{ player, edges} => {
                for (action, child_id) in edges.iter() {
                    Self::calc_terminal_probs(
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

    fn calc_exploitability(rule: &Rule, prof: &Profile) -> Value {
        Self::calc_best_resp_against_to(rule, &Player::P2, prof[&Player::P2].clone()) -
        Self::calc_best_resp_against_to(rule, &Player::P1, prof[&Player::P1].clone())
    }
}

fn main() {
    let rule = Rule::new();
    let best_prof: Profile = vec![
        (Player::P1, vec![                
            (0, vec![
                (0, 1.0),
                (1, 0.0),
            ].iter().cloned().collect()),
            (1, vec![
                (0, 1.0),
                (1, 0.0),
            ].iter().cloned().collect()),
            (2, vec![
                (0, 1.0),
                (1, 0.0),
            ].iter().cloned().collect()),
            (9, vec![
                (2, 1.0),
                (3, 0.0),
            ].iter().cloned().collect()),
            (10, vec![
                (2, 2.0 / 3.0),
                (3, 1.0 / 3.0),
            ].iter().cloned().collect()),
            (11, vec![
                (2, 0.0),
                (3, 1.0),
            ].iter().cloned().collect()),
        ].iter().cloned().collect()),
        (Player::P2, vec![
            (3, vec![
                (0, 1.0),
                (1, 0.0),
            ].iter().cloned().collect()),
            (4, vec![
                (2, 2.0 / 3.0),
                (3, 1.0 / 3.0),
            ].iter().cloned().collect()),
            (5, vec![
                (0, 0.0),
                (1, 1.0),
            ].iter().cloned().collect()),
            (6, vec![
                (2, 0.0),
                (3, 1.0),
            ].iter().cloned().collect()),
            (7, vec![
                (0, 2.0 / 3.0),
                (1, 1.0 / 3.0),
            ].iter().cloned().collect()),
            (8, vec![
                (2, 1.0),
                (3, 0.0),
            ].iter().cloned().collect()),
        ].iter().cloned().collect()),
    ].iter().cloned().collect();
    println!("profile1: {:?}", &best_prof[&Player::P1]);
    println!("profile2: {:?}", &best_prof[&Player::P2]);

    println!("calculate_ev: {}", Game::calc_ev(&rule, &best_prof));
    println!("best_resp: {}", Game::calc_best_resp_against_to(&rule, &Player::P1, best_prof[&Player::P1].clone()));
    println!("best_resp: {}", Game::calc_best_resp_against_to(&rule, &Player::P2, best_prof[&Player::P2].clone()));
    println!("exploitability: {}", Game::calc_exploitability(&rule, &best_prof));

    let uniform_prof = ProfileUtil::from_strt(
        &Player::P1, StrategyUtil::uniform(&rule, &Player::P1),
        &Player::P2, StrategyUtil::uniform(&rule, &Player::P2),
    ).unwrap();
    println!("exploitability: {}", Game::calc_exploitability(&rule, &uniform_prof));
}
