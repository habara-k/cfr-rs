use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize, Debug)]
pub enum Player {
    P1,
    P2,
    C,
}

impl Player {
    pub fn opponent(&self) -> Player {
        match self {
            Player::P1 => Player::P2,
            Player::P2 => Player::P1,
            Player::C => panic!("Player::C has no opponent"),
        }
    }
    pub fn sign(&self) -> i32 {
        match self {
            Player::P1 => 1,
            Player::P2 => -1,
            Player::C => panic!("Player::C has no sign"),
        }
    }
}
