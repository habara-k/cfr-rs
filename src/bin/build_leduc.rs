fn main() {
    let leduc = serde_json::to_string_pretty(&leduc::rule()).unwrap();
    println!("{}", leduc);
}

mod leduc {
    use cfr_rs::{
        action::{Action, ActionId},
        node::{InformationSet, InformationSetId, Node, NodeId},
        player::Player,
        rule::Rule,
    };
    use std::collections::{BTreeMap, VecDeque};

    /// Get `Rule` of Leduc Hold'em
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
        leduc.build();

        let mut rule: Rule = Default::default();
        rule.actions = leduc.actions;
        rule.nodes = leduc.nodes;
        rule.root = NodeId::new(0);
        rule.info_partition = leduc.info_partition;
        rule.transition = leduc.transition;
        rule.info_set_details = leduc.info_set_details;
        rule
    }

    #[derive(Default)]
    struct Leduc {
        actions: BTreeMap<ActionId, Action>,
        action_id: BTreeMap<Action, ActionId>,
        nodes: BTreeMap<NodeId, Node>,
        node_id: usize,
        transition: BTreeMap<NodeId, BTreeMap<ActionId, f64>>,
        observations: BTreeMap<Vec<&'static str>, usize>,
        info_partition: BTreeMap<InformationSetId, InformationSet>,
        info_set_id: usize,
        info_set_details: BTreeMap<InformationSetId, String>,
    }

    #[derive(Clone, Default, Debug)]
    struct State {
        history: Vec<&'static str>,
        bet: BTreeMap<Player, i32>,
        pot: i32,
        hole_card: BTreeMap<Player, char>,
        flop_card: Option<char>,
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
                    // terminal node
                    self.nodes.insert(s.node_id, Node::Terminal { value: val });
                } else if let Some(trans) = transition(&s) {
                    // chance node
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
                                .hole_card
                                .insert(Player::P1, action.chars().nth(4).unwrap());
                            next_state
                                .hole_card
                                .insert(Player::P2, action.chars().nth(5).unwrap());
                        } else if action.starts_with("Flop") {
                            next_state.flop_card = action.chars().nth(4);
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
                    // playable node
                    let obs = observation(&s, player);
                    if self.observations.contains_key(&obs) {
                        let info_set_id = InformationSetId::new(self.observations[&obs]);
                        self.info_partition
                            .get_mut(&info_set_id)
                            .unwrap()
                            .push(s.node_id);
                    } else {
                        let info_set_id = InformationSetId::new(self.info_set_id);
                        self.info_partition.insert(info_set_id, vec![s.node_id]);
                        self.info_set_details
                            .insert(info_set_id, detail(&obs, &player));
                        self.observations.insert(obs, self.info_set_id);
                        self.info_set_id += 1;
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

    fn detail(observation: &Vec<&'static str>, myself: &Player) -> String {
        let mut detail = String::from(match myself {
            Player::P1 => "P1:",
            Player::P2 => "P2:",
            _ => panic!(),
        });
        for &action in observation {
            if action.starts_with("Deal") {
                detail += &format!(" {}", action.chars().nth(4).unwrap());
            } else {
                detail += &format!(" {}", action);
            }
        }
        detail
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
            return Some(
                match s.history[0] {
                    "DealJQ" => vec![
                        ("FlopJ", 1.0 / 4.0),
                        ("FlopQ", 1.0 / 4.0),
                        ("FlopK", 1.0 / 2.0),
                    ],
                    "DealJK" => vec![
                        ("FlopJ", 1.0 / 4.0),
                        ("FlopQ", 1.0 / 2.0),
                        ("FlopK", 1.0 / 4.0),
                    ],
                    "DealQJ" => vec![
                        ("FlopJ", 1.0 / 4.0),
                        ("FlopQ", 1.0 / 4.0),
                        ("FlopK", 1.0 / 2.0),
                    ],
                    "DealQK" => vec![
                        ("FlopJ", 1.0 / 2.0),
                        ("FlopQ", 1.0 / 4.0),
                        ("FlopK", 1.0 / 4.0),
                    ],
                    "DealKJ" => vec![
                        ("FlopJ", 1.0 / 4.0),
                        ("FlopQ", 1.0 / 2.0),
                        ("FlopK", 1.0 / 4.0),
                    ],
                    "DealKQ" => vec![
                        ("FlopJ", 1.0 / 2.0),
                        ("FlopQ", 1.0 / 4.0),
                        ("FlopK", 1.0 / 4.0),
                    ],
                    "DealJJ" => vec![("FlopQ", 1.0 / 2.0), ("FlopK", 1.0 / 2.0)],
                    "DealQQ" => vec![("FlopJ", 1.0 / 2.0), ("FlopK", 1.0 / 2.0)],
                    "DealKK" => vec![("FlopJ", 1.0 / 2.0), ("FlopQ", 1.0 / 2.0)],
                    _ => panic!(),
                }
                .iter()
                .cloned()
                .collect(),
            );
        }
        return None;
    }

    fn terminal_val(s: &State) -> Option<f64> {
        let n = s.history.len();
        if n == 0 {
            return None;
        }

        if s.history[n - 1] == "Fold" {
            let myself = s.last_player.unwrap();
            return Some(-myself.sign() * (s.bet[&myself] + (s.pot >> 1)) as f64);
        }

        if s.history.iter().any(|&action| action.starts_with("Flop")) {
            if (n >= 2 && s.history[n - 1] == "Check" && s.history[n - 2] == "Check")
                || s.history[n - 1] == "Call"
            {
                let winner = winner(
                    s.hole_card[&Player::P1],
                    s.hole_card[&Player::P2],
                    s.flop_card.unwrap(),
                );
                if let Some(winner) = winner {
                    return Some(winner.sign() * (s.bet[&winner] + (s.pot >> 1)) as f64);
                } else {
                    return Some(0.0);
                }
            }
        }

        return None;
    }

    fn winner(p1_card: char, p2_card: char, flop_card: char) -> Option<Player> {
        if p1_card == p2_card {
            return None;
        } else if p1_card == flop_card {
            return Some(Player::P1);
        } else if p2_card == flop_card {
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
