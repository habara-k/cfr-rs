use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ActionId(usize);

#[derive(Deserialize, Serialize, Debug)]
pub struct Action(String);
