use super::{
    action::ActionId,
    node::NodeId,
    player::Player,
    profile::Profile,
    rule::{InformationSetId, Rule},
};
use std::collections::BTreeMap;

pub fn print_node(rule: &Rule, node_id: &NodeId) {
    print!("[");
    for (i, action_id) in rule.history[node_id].iter().enumerate() {
        if i + 1 == rule.history[node_id].len() {
            print!("{:?}", rule.actions[action_id]);
        } else {
            print!("{:?}, ", rule.actions[action_id]);
        }
    }
    print!("]")
}

pub fn print_info_set(rule: &Rule, info_set_id: &InformationSetId) {
    println!("[");
    for node_id in rule.info_partition[info_set_id].iter() {
        print!("    ");
        print_node(rule, node_id);
        println!(",");
    }
    print!("  ]");
}

pub fn print_dist(rule: &Rule, dist: &BTreeMap<ActionId, f64>) {
    println!("{{");
    for (action_id, prob) in dist.iter() {
        println!("    {:?}: {}", rule.actions[action_id], prob);
    }
    print!("  }}");
}

pub fn print_prof(rule: &Rule, prof: &Profile) {
    for player in [Player::P1, Player::P2].iter() {
        println!("{:?}: {{", player);
        for (info_set_id, dist) in prof[player].iter() {
            print!("  ");
            print_info_set(rule, info_set_id);
            print!(": ");
            print_dist(rule, dist);
            println!(",");
        }
        println!("}}");
    }
}
