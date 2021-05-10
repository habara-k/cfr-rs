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