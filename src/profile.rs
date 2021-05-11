use super::{
    action::ActionId,
    node::{Node, NodeId},
    player::Player,
    rule::Rule,
    strategy::{self, Strategy},
};
use std::collections::BTreeMap;
use std::fs;

pub type Profile = BTreeMap<Player, Strategy>;

pub fn from_json(json: &str) -> Profile {
    let prof: Profile = serde_json::from_str(json).expect("failed to deserialize json");
    prof
}

pub fn from_file(path: &str) -> Profile {
    from_json(&fs::read_to_string(path).expect("failed to read file"))
}

pub fn from_name(prof_name: &str) -> Profile {
    match prof_name {
        "kuhn_nash" => from_file("src/profile/kuhn_nash.json"),
        "glico_nash" => from_file("src/profile/glico_nash.json"),
        _ => panic!("invalid profile name"),
    }
}

pub fn uniform(rule: &Rule) -> Profile {
    from_strt(
        Player::P1,
        strategy::uniform(rule, &Player::P1),
        Player::P2,
        strategy::uniform(rule, &Player::P2),
    )
}

pub fn from_strt(a: Player, a_strt: Strategy, b: Player, b_strt: Strategy) -> Profile {
    if a == Player::C || b == Player::C || a == b {
        panic!("invalid arguments");
    }
    let mut prof = Profile::new();
    prof.insert(a, a_strt);
    prof.insert(b, b_strt);
    prof
}

pub fn prob_to_take_action(
    rule: &Rule,
    prof: &Profile,
    node_id: &NodeId,
    action_id: &ActionId,
) -> f64 {
    match &rule.nodes[node_id] {
        Node::Terminal { .. } => {
            panic!("terminal has no action");
        }
        Node::NonTerminal { player, .. } => match player {
            Player::P1 | Player::P2 => prof[player][&rule.info_set_id_by_node[node_id]][action_id],
            Player::C => rule.transition[node_id][action_id],
        },
    }
}
