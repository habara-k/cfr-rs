use super::{action::ActionId, player::Player};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Clone, Copy, Default, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct NodeId(usize);

impl NodeId {
    pub fn new(i: usize) -> NodeId {
        NodeId(i)
    }
}

pub type NodeValue = i32;

#[derive(Clone, Deserialize, Serialize)]
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
            Node::Terminal { .. } => true,
            Node::NonTerminal { .. } => false,
        }
    }

    pub fn is_non_terminal(&self) -> bool {
        match self {
            Node::Terminal { .. } => false,
            Node::NonTerminal { .. } => true,
        }
    }

    pub fn value(&self) -> &NodeValue {
        match self {
            Node::Terminal { value } => value,
            Node::NonTerminal { .. } => panic!("non terminal has no value"),
        }
    }

    pub fn player(&self) -> &Player {
        match self {
            Node::Terminal { .. } => panic!("terminal has no player"),
            Node::NonTerminal { player, .. } => player,
        }
    }

    pub fn edges(&self) -> &BTreeMap<ActionId, NodeId> {
        match self {
            Node::Terminal { .. } => panic!("terminal has no edges"),
            Node::NonTerminal { edges, .. } => edges,
        }
    }
}

pub type InformationSet = Vec<NodeId>;

#[derive(Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct InformationSetId(usize);

impl InformationSetId {
    pub fn new(i: usize) -> Self {
        InformationSetId(i)
    }
}

pub type InformationPartition = BTreeMap<InformationSetId, InformationSet>;
