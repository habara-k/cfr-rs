//! Representation of *actions*

use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ActionId(usize);
impl ActionId {
    pub fn new(id: usize) -> Self {
        ActionId(id)
    }
}

#[derive(Clone, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Action(String);

impl Action {
    pub fn new(action: &str) -> Self {
        Action(String::from(action))
    }
    pub fn to_str(&self) -> &str {
        &self.0
    }
}
