use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ActionId(usize);

#[derive(Serialize, Deserialize, Debug)]
pub struct Action(String);
