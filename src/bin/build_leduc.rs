fn main() {
    let leduc = serde_json::to_string_pretty(&leduc::rule()).unwrap();
    println!("{}", leduc);
}

mod leduc {
    use cfr_rs::{
        action::{Action, ActionId},
        node::{InformationSetId, Node, NodeId},
        player::Player,
        rule::Rule,
    };
    use std::collections::BTreeMap;

    /// Get `Rule` of Leduc Hold'em
    pub fn rule() -> Rule {
        let mut builder: RuleBuilder = Default::default();
        builder.build();
        for (obs, info_set_id) in builder.obs_to_info_set_id {
            builder.rule.info_set_details.insert(info_set_id, obs);
        }
        builder.rule
    }

    #[derive(Default)]
    struct RuleBuilder {
        rule: Rule,
        action_id: BTreeMap<Action, ActionId>,
        node_size: usize,
        info_set_size: usize,
        obs_to_info_set_id: BTreeMap<String, InformationSetId>,
    }

    impl RuleBuilder {
        fn build(&mut self) {
            self.initialize();

            let bet: BTreeMap<Player, i32> = vec![(Player::P1, 0), (Player::P2, 0)]
                .iter()
                .cloned()
                .collect();
            let hole_card: BTreeMap<Player, char> = BTreeMap::new();
            let history: Vec<(Player, ActionId)> = Vec::new();
            self.rule.root = self.dfs(2, &bet, &hole_card, None, &history);
        }

        fn dfs(
            &mut self,
            pot: i32,
            bet: &BTreeMap<Player, i32>,
            hole_card: &BTreeMap<Player, char>,
            flop_card: Option<char>,
            history: &Vec<(Player, ActionId)>,
        ) -> NodeId {
            // chance node
            if hole_card.is_empty() {
                // deal hole cards
                let mut trans: BTreeMap<ActionId, f64> = BTreeMap::new();
                let mut edges: BTreeMap<ActionId, NodeId> = BTreeMap::new();
                for (action_id, prob, hole_card) in self.deal_hole_cards() {
                    trans.insert(action_id, prob.clone());

                    let mut history = history.clone();
                    history.push((Player::C, action_id));
                    let child_id = self.dfs(pot, bet, &hole_card, flop_card, &history);
                    edges.insert(action_id, child_id);
                }
                let node_id = NodeId::new(self.node_size);
                self.node_size += 1;
                self.rule.transition.insert(node_id, trans);
                self.rule.nodes.insert(
                    node_id,
                    Node::NonTerminal {
                        player: Player::C,
                        edges,
                    },
                );
                return node_id;
            }

            if flop_card.is_none() && self.ready_to_next_phase(history) {
                // open flop card

                let pot = pot + bet.values().sum::<i32>();
                let bet: BTreeMap<Player, i32> = vec![(Player::P1, 0), (Player::P2, 0)]
                    .iter()
                    .cloned()
                    .collect();

                let mut trans: BTreeMap<ActionId, f64> = BTreeMap::new();
                let mut edges: BTreeMap<ActionId, NodeId> = BTreeMap::new();
                for (action_id, prob, flop_card) in self.open_flop_card(hole_card) {
                    trans.insert(action_id, prob.clone());

                    let mut history = history.clone();
                    history.push((Player::C, action_id));
                    let child_id = self.dfs(pot, &bet, hole_card, Some(flop_card), &history);
                    edges.insert(action_id, child_id);
                }
                let node_id = NodeId::new(self.node_size);
                self.node_size += 1;
                self.rule.transition.insert(node_id, trans);
                self.rule.nodes.insert(
                    node_id,
                    Node::NonTerminal {
                        player: Player::C,
                        edges,
                    },
                );
                return node_id;
            }

            // terminal node
            if let Some(value) = self.terminal_val(history, hole_card, flop_card, pot, bet) {
                let node_id = NodeId::new(self.node_size);
                self.rule.nodes.insert(node_id, Node::Terminal { value });
                self.node_size += 1;
                return node_id;
            }

            // playable node
            let (player, legal_actions) = self.legal_actions(history, bet);

            // update nodes
            let mut edges: BTreeMap<ActionId, NodeId> = BTreeMap::new();
            for action_id in legal_actions {
                let mut bet = bet.clone();
                match self.rule.actions[&action_id].to_str() {
                    "Fold" | "Check" => {}
                    "Call" => {
                        bet.insert(player, bet[&player.opponent()]);
                    }
                    "Raise" => {
                        let raise_size = bet[&player.opponent()]
                            + if flop_card.is_some() {
                                4
                            } else if pot == 2 && bet[&player.opponent()] == 0 {
                                1
                            } else {
                                2
                            };
                        bet.insert(player, raise_size);
                    }
                    _ => unreachable!(),
                }
                let mut history = history.clone();
                history.push((player, action_id));
                let child_id = self.dfs(pot, &bet, &hole_card, flop_card, &history);

                edges.insert(action_id, child_id);
            }
            let node_id = NodeId::new(self.node_size);
            self.node_size += 1;
            self.rule
                .nodes
                .insert(node_id, Node::NonTerminal { player, edges });

            // update information set
            let obs = self.observation(&player, history, hole_card);
            if self.obs_to_info_set_id.contains_key(&obs) {
                self.rule
                    .info_sets
                    .get_mut(&self.obs_to_info_set_id[&obs])
                    .unwrap()
                    .push(node_id);
            } else {
                let info_set_id = InformationSetId::new(self.info_set_size);
                self.info_set_size += 1;

                self.rule.info_sets.insert(info_set_id, vec![node_id]);
                self.obs_to_info_set_id.insert(obs, info_set_id);
            }

            return node_id;
        }

        fn legal_actions(
            &self,
            history: &Vec<(Player, ActionId)>,
            bet: &BTreeMap<Player, i32>,
        ) -> (Player, Vec<ActionId>) {
            // Require: next player is Player::P1 or Player::P2
            let last_player = history.last().unwrap().0;
            let last_action = &self.rule.actions[&history.last().unwrap().1];
            match last_player {
                Player::C => (
                    Player::P1,
                    vec!["Check", "Raise"]
                        .iter()
                        .map(|action| self.action_id[&Action::new(action)])
                        .collect(),
                ),
                Player::P1 => (
                    Player::P2,
                    match last_action.to_str() {
                        "Check" => vec!["Check", "Raise"],
                        "Raise" => {
                            if bet[&Player::P2] == 0 {
                                vec!["Fold", "Call", "Raise"]
                            } else {
                                vec!["Fold", "Call"]
                            }
                        }
                        _ => unreachable!(),
                    }
                    .iter()
                    .map(|action| self.action_id[&Action::new(action)])
                    .collect(),
                ),
                Player::P2 => (
                    Player::P1,
                    match last_action.to_str() {
                        "Raise" => {
                            if bet[&Player::P1] == 0 {
                                vec!["Fold", "Call", "Raise"]
                            } else {
                                vec!["Fold", "Call"]
                            }
                        }
                        _ => unreachable!(),
                    }
                    .iter()
                    .map(|action| self.action_id[&Action::new(action)])
                    .collect(),
                ),
            }
        }

        fn terminal_val(
            &self,
            history: &Vec<(Player, ActionId)>,
            hole_card: &BTreeMap<Player, char>,
            flop_card: Option<char>,
            pot: i32,
            bet: &BTreeMap<Player, i32>,
        ) -> Option<f64> {
            if flop_card.is_some() && self.ready_to_next_phase(history) {
                // showdown
                let p1_card = hole_card[&Player::P1];
                let p2_card = hole_card[&Player::P2];
                let winner = if p1_card == p2_card {
                    None
                } else if p1_card == flop_card.unwrap() {
                    Some(Player::P1)
                } else if p2_card == flop_card.unwrap() {
                    Some(Player::P2)
                } else if p1_card == 'J' || p2_card == 'K' {
                    Some(Player::P2)
                } else {
                    Some(Player::P1)
                };

                if let Some(winner) = winner {
                    return Some(winner.sign() * (bet[&winner] + pot / 2) as f64);
                } else {
                    // draw
                    return Some(0.0);
                }
            }
            if self.rule.actions[&history.last().unwrap().1].to_str() == "Fold" {
                // fold
                let loser = history.last().unwrap().0;
                return Some(-loser.sign() * (bet[&loser] + pot / 2) as f64);
            }
            None
        }

        fn open_flop_card(&self, hole_card: &BTreeMap<Player, char>) -> Vec<(ActionId, f64, char)> {
            let mut rem: BTreeMap<char, i32> =
                vec![('J', 2), ('Q', 2), ('K', 2)].iter().cloned().collect();
            *rem.get_mut(&hole_card[&Player::P1]).unwrap() -= 1;
            *rem.get_mut(&hole_card[&Player::P2]).unwrap() -= 1;

            let mut trans: Vec<(ActionId, f64, char)> = Vec::new();
            for card in vec!['J', 'Q', 'K'] {
                if rem[&card] == 0 {
                    continue;
                }
                trans.push((
                    self.action_id[&Action::new(&format!("Flop{}", card))],
                    rem[&card] as f64 / 4.0,
                    card,
                ));
            }
            trans
        }

        fn deal_hole_cards(&self) -> Vec<(ActionId, f64, BTreeMap<Player, char>)> {
            let mut trans: Vec<(ActionId, f64, BTreeMap<Player, char>)> = Vec::new();
            for p1_card in vec!['J', 'Q', 'K'] {
                for p2_card in vec!['J', 'Q', 'K'] {
                    let prob = if p1_card == p2_card {
                        1.0 / 15.0
                    } else {
                        2.0 / 15.0
                    };
                    let hole_card = vec![(Player::P1, p1_card), (Player::P2, p2_card)]
                        .iter()
                        .cloned()
                        .collect();
                    trans.push((
                        self.action_id[&Action::new(&format!("Deal{}{}", p1_card, p2_card))],
                        prob,
                        hole_card,
                    ));
                }
            }
            trans
        }

        fn initialize(&mut self) {
            self.rule.actions = vec![
                "Check", "Raise", "Fold", "Call", "FlopJ", "FlopQ", "FlopK", "DealJQ", "DealJK",
                "DealQJ", "DealQK", "DealKJ", "DealKQ", "DealJJ", "DealQQ", "DealKK",
            ]
            .iter()
            .enumerate()
            .map(|(i, action)| (ActionId::new(i), Action::new(action)))
            .collect();

            self.action_id = self
                .rule
                .actions
                .iter()
                .map(|(i, action)| (action.clone(), *i))
                .collect();
        }

        fn observation(
            &self,
            myself: &Player,
            history: &Vec<(Player, ActionId)>,
            hole_card: &BTreeMap<Player, char>,
        ) -> String {
            if history.len() == 1 {
                return format!("{:?}: {}", myself, hole_card[&myself]);
            }
            format!(
                "{:?}: {} {}",
                myself,
                hole_card[&myself],
                history
                    .iter()
                    .skip(1)
                    .map(|(_player, action_id)| String::from(self.rule.actions[action_id].to_str()))
                    .collect::<Vec<String>>()
                    .join(" ")
            )
        }

        fn ready_to_next_phase(&self, history: &Vec<(Player, ActionId)>) -> bool {
            let n = history.len();
            return self.rule.actions[&history[n - 1].1].to_str() == "Call"
                || (n >= 2
                    && self.rule.actions[&history[n - 1].1].to_str() == "Check"
                    && self.rule.actions[&history[n - 2].1].to_str() == "Check");
        }
    }
}
