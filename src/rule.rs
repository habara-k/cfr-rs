//! Representation of *game rule*

use super::{
    action::{Action, ActionId},
    node::{InformationSet, InformationSetId, Node, NodeId},
    player::Player,
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs;

#[derive(Default, Deserialize, Serialize)]
pub struct Rule {
    pub actions: BTreeMap<ActionId, Action>,
    pub nodes: BTreeMap<NodeId, Node>,
    pub root: NodeId,
    pub info_sets: BTreeMap<InformationSetId, InformationSet>,
    pub info_set_details: BTreeMap<InformationSetId, String>,
    pub transition: BTreeMap<NodeId, BTreeMap<ActionId, f64>>,

    #[serde(skip_deserializing, skip_serializing)]
    pub info_set_including_node: BTreeMap<NodeId, InformationSetId>,
    #[serde(skip_deserializing, skip_serializing)]
    pub actions_at_info_set: BTreeMap<InformationSetId, Vec<ActionId>>,
    #[serde(skip_deserializing, skip_serializing)]
    pub player_at_info_set: BTreeMap<InformationSetId, Player>,
}

impl Rule {
    /// Calculate values that are not needed for game definition but are helpful for calculations.
    pub fn build(&mut self) {
        self.build_node_to_info_set();
        self.build_info_set_to_actions();
        self.build_info_set_to_player();
    }

    /// Calculate the mapping from a node id to its information set id.
    fn build_node_to_info_set(&mut self) {
        trace!("start: build_node_to_info_set");
        for (info_set_id, info_set) in self.info_sets.iter() {
            for node_id in info_set.iter() {
                self.info_set_including_node.insert(*node_id, *info_set_id);
            }
        }
        trace!("finish: build_node_to_info_set");
    }
    /// Calculate the mapping from the information set id to the legal actions there.
    fn build_info_set_to_actions(&mut self) {
        trace!("start: build_info_set_to_actions");
        for (info_set_id, info_set) in self.info_sets.iter() {
            for node_id in info_set.iter() {
                if let Node::NonTerminal { edges, .. } = &self.nodes[node_id] {
                    let actions: Vec<ActionId> = edges.keys().cloned().collect();
                    if self.actions_at_info_set.contains_key(info_set_id) {
                        assert_eq!(actions, self.actions_at_info_set[info_set_id]);
                    } else {
                        self.actions_at_info_set.insert(*info_set_id, actions);
                    }
                }
            }
        }
        trace!("finish: build_info_set_to_actions");
    }

    /// Calculate the mapping from the information set id to the player taking action there.
    fn build_info_set_to_player(&mut self) {
        trace!("start: build_info_set_to_player");
        for (info_set_id, info_set) in self.info_sets.iter() {
            for node_id in info_set.iter() {
                if let Node::NonTerminal { player, .. } = &self.nodes[node_id] {
                    if self.player_at_info_set.contains_key(info_set_id) {
                        assert_eq!(*player, self.player_at_info_set[info_set_id]);
                    } else {
                        self.player_at_info_set.insert(*info_set_id, *player);
                    }
                }
            }
        }
        trace!("finish: build_info_set_to_player");
    }
}

/// Get the `Rule` from the JSON string
/// # Example
/// ```
/// use cfr_rs::*;
/// let rule = rule::from_json(r#"{
///   "actions": {
///     "0": "R",
///     "1": "P",
///     "2": "S"
///   },
///   "nodes": {
///     "0": {
///       "NonTerminal": {
///         "player": "P1",
///         "edges": {
///           "0": 1,
///           "1": 2,
///           "2": 3
///         }
///       }
///     },
///     "1": {
///       "NonTerminal": {
///         "player": "P2",
///         "edges": {
///           "0": 4,
///           "1": 5,
///           "2": 6
///         }
///       }
///     },
///     "2": {
///       "NonTerminal": {
///         "player": "P2",
///         "edges": {
///           "0": 7,
///           "1": 8,
///           "2": 9
///         }
///       }
///     },
///     "3": {
///       "NonTerminal": {
///         "player": "P2",
///         "edges": {
///           "0": 10,
///           "1": 11,
///           "2": 12
///         }
///       }
///     },
///     "4": {
///       "Terminal": {
///         "value": 0
///       }
///     },
///     "5": {
///       "Terminal": {
///         "value": -6
///       }
///     },
///     "6": {
///       "Terminal": {
///         "value": 3
///       }
///     },
///     "7": {
///       "Terminal": {
///         "value": 6
///       }
///     },
///     "8": {
///       "Terminal": {
///         "value": 0
///       }
///     },
///     "9": {
///       "Terminal": {
///         "value": -6
///       }
///     },
///     "10": {
///       "Terminal": {
///         "value": -3
///       }
///     },
///     "11": {
///       "Terminal": {
///         "value": 6
///       }
///     },
///     "12": {
///       "Terminal": {
///         "value": 0
///       }
///     }
///   },
///   "root": 0,
///   "info_sets": {
///     "0": [0],
///     "1": [1,2,3]
///   },
///   "info_set_details": {
///     "0": "",
///     "1": ""
///   },
///   "transition": {
///   }
/// }"#); // Game rule of Glico(weighted rock paper scissors)
/// ```
pub fn from_json(json: &str) -> Rule {
    let mut rule: Rule = serde_json::from_str(json).expect("failed to deserialize JSON");
    rule.build();
    rule
}

/// Get the `Rule` from the JSON file.
/// # Example
/// ```
/// use cfr_rs::*;
/// let rule = rule::from_path("src/rule/kuhn.json"); // Game rule of Kuhn poker
/// ```
pub fn from_path(path: &str) -> Rule {
    from_json(&fs::read_to_string(path).expect("failed to read file"))
}
