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
        rule.info_sets = leduc.info_sets;
        rule.transition = leduc.transition;
        rule.info_set_details = leduc.info_set_details;
        rule
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

    impl State {
        fn transition(&self) -> Option<Vec<(&'static str, f64)>> {
            let n = self.history.len();
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
            if self
                .history
                .iter()
                .any(|&action| action.starts_with("Flop"))
            {
                return None;
            }
            if self.history[n - 1] == "Call"
                || (n >= 2 && self.history[n - 1] == "Check" && self.history[n - 2] == "Check")
            {
                return Some(
                    match self.history[0] {
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

        fn terminal_val(&self) -> Option<f64> {
            let n = self.history.len();
            if n == 0 {
                return None;
            }

            if self.history[n - 1] == "Fold" {
                let myself = self.last_player.unwrap();
                return Some(-myself.sign() * (self.bet[&myself] + (self.pot / 2)) as f64);
            }

            if self.history.iter().any(|&action| action.starts_with("Flop")) {
                if (n >= 2 && self.history[n - 1] == "Check" && self.history[n - 2] == "Check")
                    || self.history[n - 1] == "Call"
                {
                    let winner = winner(
                        self.hole_card[&Player::P1],
                        self.hole_card[&Player::P2],
                        self.flop_card.unwrap(),
                    );
                    return if let Some(winner) = winner {
                        Some(winner.sign() * (self.bet[&winner] + (self.pot / 2)) as f64)
                    } else {
                        // draw
                        Some(0.0)
                    }
                }
            }

            return None;
        }

        fn legal_actions(&self) -> Option<(Player, Vec<&'static str>)> {
            if self.terminal_val().is_some() {
                return None;
            }
            if self.transition().is_some() {
                return None;
            }
            let n = self.history.len();
            match self.last_player.unwrap() {
                Player::P1 => Some((
                    Player::P2,
                    match self.history[n - 1] {
                        "Check" => vec!["Check", "Raise"],
                        "Raise" => {
                            if self.bet[&Player::P2] == 0 {
                                // P2 can raise if P2 has not raised yet.
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
                    match self.history[n - 1] {
                        "Raise" => {
                            if self.bet[&Player::P1] == 0 {
                                // P1 can raise if P1 has not raised yet.
                                vec!["Fold", "Call", "Raise"]
                            } else {
                                vec!["Fold", "Call"]
                            }
                        }
                        _ => panic!("state: {:?}", self),
                    },
                )),
                Player::C => Some((Player::P1, vec!["Check", "Raise"])),
            }
        }

        fn observation(&self, myself: Player) -> Vec<&'static str> {
            self.history
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

    #[derive(Default)]
    struct Leduc {
        actions: BTreeMap<ActionId, Action>,
        action_id: BTreeMap<Action, ActionId>,
        nodes: BTreeMap<NodeId, Node>,
        node_size: usize,
        transition: BTreeMap<NodeId, BTreeMap<ActionId, f64>>,
        info_sets: BTreeMap<InformationSetId, InformationSet>,
        info_set_size: usize,
        info_set_details: BTreeMap<InformationSetId, String>,
        obs_to_info_set_id: BTreeMap<Vec<&'static str>, InformationSetId>,
    }

    impl Leduc {
        fn build(&mut self) {
            let mut que: VecDeque<State> = VecDeque::new();
            let mut init_state: State = Default::default();
            init_state.bet.insert(Player::P1, 0);
            init_state.bet.insert(Player::P2, 0);
            init_state.pot = 2;
            que.push_back(init_state);
            self.node_size = 1;

            while !que.is_empty() {
                let s = que.front().unwrap().clone();
                que.pop_front();

                if let Some(val) = s.terminal_val() {
                    // terminal node
                    self.nodes.insert(s.node_id, Node::Terminal { value: val });
                } else if let Some(trans) = s.transition() {
                    // chance node
                    let mut edges: BTreeMap<ActionId, NodeId> = BTreeMap::new();
                    for (action, _) in trans.iter() {
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
                        }
                        if action.starts_with("Flop") {
                            next_state.flop_card = action.chars().nth(4);
                        }
                        next_state.last_player = Some(Player::C);

                        next_state.node_id = NodeId::new(self.node_size);
                        self.node_size += 1;

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

                    let dist = trans.iter().map(|(action, prob)| (self.action_id[&Action::new(action)], prob.clone())).collect();
                    self.transition.insert(s.node_id, dist);
                } else if let Some((player, legal_actions)) = s.legal_actions() {
                    // playable node
                    let obs = s.observation(player);
                    if self.obs_to_info_set_id.contains_key(&obs) {
                        self.info_sets
                            .get_mut(&self.obs_to_info_set_id[&obs])
                            .unwrap()
                            .push(s.node_id);
                    } else {
                        let info_set_id = InformationSetId::new(self.info_set_size);
                        self.info_set_size += 1;

                        self.info_sets.insert(info_set_id, vec![s.node_id]);
                        self.info_set_details
                            .insert(info_set_id, detail(&obs, &player));
                        self.obs_to_info_set_id.insert(obs, info_set_id);
                    }

                    let mut edges: BTreeMap<ActionId, NodeId> = BTreeMap::new();
                    for &action in legal_actions.iter() {
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

                        next_state.node_id = NodeId::new(self.node_size);
                        self.node_size += 1;
                        edges.insert(self.action_id[&Action::new(action)], next_state.node_id);

                        que.push_back(next_state);
                    }
                    self.nodes
                        .insert(s.node_id, Node::NonTerminal { player, edges });
                }
            }
        }
    }
}
