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

    pub fn sign(&self) -> i32 {
        match self {
            Player::P1 => 1,
            Player::P2 => -1,
            Player::C => panic!("Player::C has no sign"),
        }
    }
}

#[derive(Clone)]
pub struct ProbContribute {
    p1: f64,
    p2: f64,
    c: f64,
}

impl ProbContribute {
    pub fn new() -> Self {
        ProbContribute{ p1: 1.0, p2: 1.0, c: 1.0 }
    }
    pub fn only(&self, player: &Player) -> f64 {
        match player {
            Player::P1 => self.p1,
            Player::P2 => self.p2,
            Player::C => self.c,
        }
    }
    pub fn except(&self, player: &Player) -> f64 {
        match player {
            Player::P1 => self.p2 * self.c,
            Player::P2 => self.p1 * self.c,
            Player::C => self.p1 * self.p2,
        }
    }
    pub fn prod(&self, player: &Player, prob: f64) -> Self {
        match player {
            Player::P1 => {
                Self { p1: self.p1 * prob, ..*self }
            },
            Player::P2 => {
                Self { p2: self.p2 * prob, ..*self }
            },
            Player::C => {
                Self { c: self.c * prob, ..*self }
            },
        }
    }
}

