use std::collections::BTreeMap;
use super::{
    action::ActionId,
    player::Player,
    node::NodeId,
    rule::Rule,  // Interdependent
};
pub type Transition = BTreeMap<NodeId, BTreeMap<ActionId, f64>>;

pub fn ones(rule: &Rule) -> Transition {
    rule.nodes.iter().filter(|&(_, node)| {
        node.is_non_terminal() && node.player() == Player::C
    }).map(|(node_id, node)| {
        (*node_id, {
            node.edges().iter().map(|(action_id, _)| {
                (*action_id, 1.0)
            }).collect()
        })
    }).collect()
}