use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Debug)]
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

    pub fn sign(&self) -> f64 {
        match self {
            Player::P1 => 1.0,
            Player::P2 => -1.0,
            Player::C => panic!("Player::C has no sign"),
        }
    }
}
