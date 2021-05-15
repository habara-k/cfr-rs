use super::{
    action::{Action, ActionId},
    node::{Node, NodeId},
    player::Player,
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, VecDeque};
use std::fs;

pub type InformationSet = Vec<NodeId>;

#[derive(Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct InformationSetId(usize);

impl InformationSetId {
    pub fn new(i: usize) -> Self {
        InformationSetId(i)
    }
}

pub type InformationPartition = BTreeMap<InformationSetId, InformationSet>;

pub type Transition = BTreeMap<NodeId, BTreeMap<ActionId, f64>>;

#[derive(Default, Deserialize, Serialize)]
pub struct Rule {
    pub actions: BTreeMap<ActionId, Action>,
    pub nodes: BTreeMap<NodeId, Node>,
    pub root: NodeId,
    pub info_partitions: BTreeMap<Player, InformationPartition>,
    pub transition: Transition,

    // for utils
    #[serde(skip_deserializing)]
    pub info_set_id_by_node: BTreeMap<NodeId, InformationSetId>,
    #[serde(skip_deserializing)]
    pub actions_by_info_set: BTreeMap<InformationSetId, Vec<ActionId>>,
    #[serde(skip_deserializing)]
    pub player_by_info_set: BTreeMap<InformationSetId, Player>,
    #[serde(skip_deserializing)]
    pub history: BTreeMap<NodeId, Vec<ActionId>>,
}

impl Rule {
    fn build(&mut self) {
        self.build_info_set_id_by_node();
        self.build_actions_by_info_set();
        self.build_player_by_info_set();
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
                    if let Node::NonTerminal { edges, .. } = &self.nodes[node_id] {
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
                    if let Node::NonTerminal { player, .. } = &self.nodes[node_id] {
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

    fn build_history(&mut self) {
        trace!("start: build_history");
        let mut actions: Vec<ActionId> = Vec::new();
        let mut history: BTreeMap<NodeId, Vec<ActionId>> = BTreeMap::new();
        self.build_history_inner(&self.root, &mut actions, &mut history);
        self.history = history;
        trace!("finish: build_history");
    }

    fn build_history_inner(
        &self,
        node_id: &NodeId,
        actions: &mut Vec<ActionId>,
        history: &mut BTreeMap<NodeId, Vec<ActionId>>,
    ) {
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
            if let Node::NonTerminal { edges, .. } = &self.nodes[&node_id] {
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
        "leduc" => from_file("src/rule/leduc.json"),
        "glico" => from_file("src/rule/glico.json"),
        _ => panic!("invalid rule name"),
    }
}

pub mod leduc {
    use super::*;

    pub fn rule() -> Rule {
        let actions: BTreeMap<ActionId, Action> = vec![
            "Check", "Raise", "Fold", "Call", "FlopJ", "FlopQ", "FlopK", "DealJQ", "DealJK",
            "DealQJ", "DealQK", "DealKJ", "DealKQ", "DealJJ", "DealQQ", "DealKK",
        ]
        .iter()
        .enumerate()
        .map(|(i, &action)| (ActionId::new(i), Action::new(action)))
        .collect();
        let action_id = actions
            .iter()
            .map(|(i, action)| (action.clone(), *i))
            .collect();
        let mut leduc: Leduc = Default::default();
        leduc.actions = actions;
        leduc.action_id = action_id;
        leduc
            .info_partitions
            .insert(Player::P1, InformationPartition::new());
        leduc
            .info_partitions
            .insert(Player::P2, InformationPartition::new());
        leduc.build();

        let mut rule: Rule = Default::default();
        rule.actions = leduc.actions;
        rule.nodes = leduc.nodes;
        rule.root = NodeId::new(0);
        rule.info_partitions = leduc.info_partitions;
        rule.transition = leduc.transition;
        rule
    }

    #[derive(Default)]
    pub struct Leduc {
        actions: BTreeMap<ActionId, Action>,
        action_id: BTreeMap<Action, ActionId>,
        nodes: BTreeMap<NodeId, Node>,
        node_id: usize,
        transition: Transition,
        observations: BTreeMap<Vec<&'static str>, usize>,
        info_partitions: BTreeMap<Player, InformationPartition>,
        info_set_id: usize,
    }

    #[derive(Clone, Default, Debug)]
    struct State {
        history: Vec<&'static str>,
        bet: BTreeMap<Player, i32>,
        pot: i32,
        card: BTreeMap<Player, char>,
        hole_card: Option<char>,
        last_player: Option<Player>,
        node_id: NodeId,
    }

    impl Leduc {
        fn build(&mut self) {
            let mut que: VecDeque<State> = VecDeque::new();
            let mut init_state: State = Default::default();
            init_state.bet.insert(Player::P1, 1);
            init_state.bet.insert(Player::P2, 1);
            que.push_back(init_state);

            while !que.is_empty() {
                let s = que.front().unwrap().clone();
                que.pop_front();

                if let Some(val) = terminal_val(&s) {
                    self.nodes.insert(s.node_id, Node::Terminal { value: val });
                } else if let Some(trans) = transition(&s) {
                    let mut edges: BTreeMap<ActionId, NodeId> = BTreeMap::new();
                    let mut dist: BTreeMap<ActionId, f64> = BTreeMap::new();
                    for (action, prob) in trans {
                        dist.insert(self.action_id[&Action::new(action)].clone(), prob);

                        let mut next_state = s.clone();
                        next_state.history.push(action);
                        next_state.pot += next_state.bet.values().sum::<i32>();
                        next_state.bet.insert(Player::P1, 0);
                        next_state.bet.insert(Player::P2, 0);

                        if action.starts_with("Deal") {
                            next_state
                                .card
                                .insert(Player::P1, action.chars().nth(4).unwrap());
                            next_state
                                .card
                                .insert(Player::P2, action.chars().nth(5).unwrap());
                        } else if action.starts_with("Flop") {
                            next_state.hole_card = action.chars().nth(4);
                        }
                        next_state.last_player = Some(Player::C);

                        self.node_id += 1;
                        next_state.node_id = NodeId::new(self.node_id);
                        edges.insert(self.action_id[&Action::new(action)], next_state.node_id);

                        que.push_back(next_state);
                    }
                    self.nodes.insert(
                        s.node_id,
                        Node::NonTerminal {
                            player: Player::C,
                            edges,
                        },
                    );
                    self.transition.insert(s.node_id, dist);
                } else if let Some((player, possible_actions)) = possible_actions(&s) {
                    let obs = observation(&s, player);
                    if self.observations.contains_key(&obs) {
                        let info_set_id = InformationSetId::new(self.observations[&obs]);
                        self.info_partitions
                            .get_mut(&player)
                            .unwrap()
                            .get_mut(&info_set_id)
                            .unwrap()
                            .push(s.node_id);
                    } else {
                        let info_set_id = InformationSetId::new(self.info_set_id);
                        self.observations.insert(obs, self.info_set_id);
                        self.info_set_id += 1;
                        self.info_partitions
                            .get_mut(&player)
                            .unwrap()
                            .insert(info_set_id, vec![s.node_id]);
                    }

                    let mut edges: BTreeMap<ActionId, NodeId> = BTreeMap::new();
                    for &action in possible_actions.iter() {
                        let mut next_state = s.clone();
                        next_state.history.push(action);
                        match action {
                            "Fold" | "Check" => {}
                            "Call" => {
                                next_state.bet.insert(player, s.bet[&player.opponent()]);
                            }
                            "Raise" => {
                                let raise_size =
                                    if s.history.iter().any(|&action| action.starts_with("Flop")) {
                                        4
                                    } else if s
                                        .history
                                        .iter()
                                        .any(|&action| action.starts_with("Raise"))
                                    {
                                        2
                                    } else {
                                        1
                                    };
                                next_state
                                    .bet
                                    .insert(player, s.bet[&player.opponent()] + raise_size);
                            }
                            _ => panic!(),
                        }
                        next_state.last_player = Some(player);

                        self.node_id += 1;
                        next_state.node_id = NodeId::new(self.node_id);
                        edges.insert(self.action_id[&Action::new(action)], next_state.node_id);

                        que.push_back(next_state);
                    }
                    self.nodes
                        .insert(s.node_id, Node::NonTerminal { player, edges });
                }
            }
        }
    }

    fn observation(s: &State, myself: Player) -> Vec<&'static str> {
        s.history
            .iter()
            .map(|&action| {
                if action.starts_with("Deal") {
                    match myself {
                        Player::P1 => match action {
                            "DealJJ" | "DealJQ" | "DealJK" => "DealJ",
                            "DealQJ" | "DealQQ" | "DealQK" => "DealQ",
                            "DealKJ" | "DealKQ" | "DealKK" => "DealK",
                            _ => panic!(),
                        },
                        Player::P2 => match action {
                            "DealJJ" | "DealQJ" | "DealKJ" => "DealJ",
                            "DealJQ" | "DealQQ" | "DealKQ" => "DealQ",
                            "DealJK" | "DealQK" | "DealKK" => "DealK",
                            _ => panic!(),
                        },
                        _ => panic!(),
                    }
                } else {
                    action
                }
            })
            .collect()
    }

    fn transition(s: &State) -> Option<Vec<(&'static str, f64)>> {
        let n = s.history.len();
        if n == 0 {
            return Some(
                [
                    ("DealJQ", 2.0 / 15.0),
                    ("DealJK", 2.0 / 15.0),
                    ("DealQJ", 2.0 / 15.0),
                    ("DealQK", 2.0 / 15.0),
                    ("DealKJ", 2.0 / 15.0),
                    ("DealKQ", 2.0 / 15.0),
                    ("DealJJ", 1.0 / 15.0),
                    ("DealQQ", 1.0 / 15.0),
                    ("DealKK", 1.0 / 15.0),
                ]
                .iter()
                .cloned()
                .collect(),
            );
        }
        if s.history.iter().any(|&action| action.starts_with("Flop")) {
            return None;
        }
        if s.history[n - 1] == "Call"
            || (n >= 2 && s.history[n - 1] == "Check" && s.history[n - 2] == "Check")
        {
            return Some(match s.history[0] {
                "DealJQ" => [
                    ("FlopJ", 1.0 / 4.0),
                    ("FlopQ", 1.0 / 4.0),
                    ("FlopK", 1.0 / 2.0),
                ]
                .iter()
                .cloned()
                .collect(),
                "DealJK" => [
                    ("FlopJ", 1.0 / 4.0),
                    ("FlopQ", 1.0 / 2.0),
                    ("FlopK", 1.0 / 4.0),
                ]
                .iter()
                .cloned()
                .collect(),
                "DealQJ" => [
                    ("FlopJ", 1.0 / 4.0),
                    ("FlopQ", 1.0 / 4.0),
                    ("FlopK", 1.0 / 2.0),
                ]
                .iter()
                .cloned()
                .collect(),
                "DealQK" => [
                    ("FlopJ", 1.0 / 2.0),
                    ("FlopQ", 1.0 / 4.0),
                    ("FlopK", 1.0 / 4.0),
                ]
                .iter()
                .cloned()
                .collect(),
                "DealKJ" => [
                    ("FlopJ", 1.0 / 4.0),
                    ("FlopQ", 1.0 / 2.0),
                    ("FlopK", 1.0 / 4.0),
                ]
                .iter()
                .cloned()
                .collect(),
                "DealKQ" => [
                    ("FlopJ", 1.0 / 2.0),
                    ("FlopQ", 1.0 / 4.0),
                    ("FlopK", 1.0 / 4.0),
                ]
                .iter()
                .cloned()
                .collect(),
                "DealJJ" => [("FlopQ", 1.0 / 2.0), ("FlopK", 1.0 / 2.0)]
                    .iter()
                    .cloned()
                    .collect(),
                "DealQQ" => [("FlopJ", 1.0 / 2.0), ("FlopK", 1.0 / 2.0)]
                    .iter()
                    .cloned()
                    .collect(),
                "DealKK" => [("FlopJ", 1.0 / 2.0), ("FlopQ", 1.0 / 2.0)]
                    .iter()
                    .cloned()
                    .collect(),
                _ => panic!(),
            });
        }
        return None;
    }

    fn terminal_val(s: &State) -> Option<i32> {
        let n = s.history.len();
        if n == 0 {
            return None;
        }

        if s.history[n - 1] == "Fold" {
            let myself = s.last_player.unwrap();
            return Some(-myself.sign() * (s.bet[&myself] + (s.pot >> 1)));
        }

        if s.history.iter().any(|&action| action.starts_with("Flop")) {
            if (n >= 2 && s.history[n - 1] == "Check" && s.history[n - 2] == "Check")
                || s.history[n - 1] == "Call"
            {
                let winner = winner(
                    s.card[&Player::P1],
                    s.card[&Player::P2],
                    s.hole_card.unwrap(),
                );
                if let Some(winner) = winner {
                    return Some(winner.sign() * (s.bet[&winner] + (s.pot >> 1)));
                } else {
                    return Some(0);
                }
            }
        }

        return None;
    }

    fn winner(p1_card: char, p2_card: char, hole_card: char) -> Option<Player> {
        if p1_card == p2_card {
            return None;
        } else if p1_card == hole_card {
            return Some(Player::P1);
        } else if p2_card == hole_card {
            return Some(Player::P2);
        } else if p1_card == 'J' || p2_card == 'K' {
            return Some(Player::P2);
        } else {
            return Some(Player::P1);
        }
    }

    fn possible_actions(s: &State) -> Option<(Player, Vec<&'static str>)> {
        if terminal_val(s).is_some() {
            return None;
        }
        if transition(s).is_some() {
            return None;
        }
        let n = s.history.len();
        match s.last_player.unwrap() {
            Player::P1 => Some((
                Player::P2,
                match s.history[n - 1] {
                    "Check" => vec!["Check", "Raise"],
                    "Raise" => {
                        if s.bet[&Player::P2] == 0 {
                            vec!["Fold", "Call", "Raise"]
                        } else {
                            vec!["Fold", "Call"]
                        }
                    }
                    _ => panic!(),
                },
            )),
            Player::P2 => Some((
                Player::P1,
                match s.history[n - 1] {
                    "Raise" => {
                        if s.bet[&Player::P1] == 0 {
                            vec!["Fold", "Call", "Raise"]
                        } else {
                            vec!["Fold", "Call"]
                        }
                    }
                    _ => panic!("state: {:?}", s),
                },
            )),
            Player::C => Some((Player::P1, vec!["Check", "Raise"])),
        }
    }
}
