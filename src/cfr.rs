use super::{
    action::ActionId,
    node::{InformationSetId, Node, NodeId},
    player::{Player, ProbContribute},
    rule::Rule,
    strategy::{self, Strategy},
};
use indicatif::ProgressIterator;
use std::collections::BTreeMap;

struct Regret(BTreeMap<InformationSetId, BTreeMap<ActionId, f64>>);

impl Regret {
    fn new(rule: &Rule) -> Self {
        Regret(
            rule.info_partition
                .iter()
                .map(|(info_set_id, _)| {
                    (
                        *info_set_id,
                        rule.actions_by_info_set[info_set_id]
                            .iter()
                            .map(|action_id| (*action_id, 0.0))
                            .collect(),
                    )
                })
                .collect(),
        )
    }
    fn to_strategy(&self) -> Strategy {
        self.0
            .iter()
            .map(|(info_set_id, v)| {
                (
                    *info_set_id,
                    Regret::normalized(Regret::positive_part(v.clone())),
                )
            })
            .collect()
    }

    fn add(&mut self, other: &Regret) {
        for (info_set_id, _) in &other.0 {
            self.add_at(info_set_id, &other.0[info_set_id]);
        }
    }

    fn add_at(&mut self, info_set_id: &InformationSetId, regret: &BTreeMap<ActionId, f64>) {
        for (action_id, reg) in regret {
            *self
                .0
                .get_mut(info_set_id)
                .unwrap()
                .get_mut(action_id)
                .unwrap() += reg;
        }
    }

    fn positive_part(v: BTreeMap<ActionId, f64>) -> BTreeMap<ActionId, f64> {
        v.into_iter()
            .map(|(action_id, prob)| (action_id, if prob < 0.0 { 0.0 } else { prob }))
            .collect()
    }

    fn normalized(v: BTreeMap<ActionId, f64>) -> BTreeMap<ActionId, f64> {
        let norm: f64 = v.iter().map(|(_, prob)| prob).sum();
        if norm < 1e-9 {
            let len = v.len() as f64;
            v.into_iter()
                .map(|(action_id, _)| (action_id, 1.0 / len))
                .collect()
        } else {
            v.into_iter()
                .map(|(action_id, prob)| (action_id, prob / norm))
                .collect()
        }
    }
}

fn cfr_dfs(
    rule: &Rule,
    regret: &mut Regret,
    reach_prob: &mut BTreeMap<InformationSetId, f64>,
    strt: &Strategy,
    node_id: &NodeId,
    prob: ProbContribute,
) -> f64 {
    match &rule.nodes[node_id] {
        Node::Terminal { value } => *value as f64,
        Node::NonTerminal { player, edges } => match player {
            Player::C => edges
                .iter()
                .map(|(action_id, child_id)| {
                    cfr_dfs(
                        rule,
                        regret,
                        reach_prob,
                        strt,
                        child_id,
                        prob.prod(player, rule.transition[node_id][action_id]),
                    )
                })
                .sum(),
            _ => {
                let info_set_id = &rule.info_set_id_by_node[node_id];
                *reach_prob.get_mut(info_set_id).unwrap() += prob.except(player);

                let action_util: BTreeMap<ActionId, f64> = edges
                    .iter()
                    .map(|(action_id, child_id)| {
                        (
                            *action_id,
                            cfr_dfs(
                                rule,
                                regret,
                                reach_prob,
                                strt,
                                child_id,
                                prob.prod(player, strt[info_set_id][action_id]),
                            ),
                        )
                    })
                    .collect();

                let ret: f64 = action_util
                    .iter()
                    .map(|(action_id, util)| strt[info_set_id][action_id] * util)
                    .sum();

                regret.add_at(
                    info_set_id,
                    &action_util
                        .iter()
                        .map(|(action_id, util)| {
                            (
                                *action_id,
                                player.sign() as f64 * (util - ret) * prob.except(player),
                            )
                        })
                        .collect::<BTreeMap<ActionId, f64>>(),
                );
                ret
            }
        },
    }
}

pub fn calc_nash_strt(rule: &Rule, init_strt: Strategy, step: usize) -> Strategy {
    let mut regret_sum = Regret::new(rule);

    let mut latest_strt = init_strt;
    let mut strt_sum = strategy::zeros(rule);

    for _ in (1..step + 1).progress() {
        let mut regret = Regret::new(rule);
        let mut reach_prob: BTreeMap<InformationSetId, f64> = rule
            .info_partition
            .iter()
            .map(|(info_set_id, _)| (*info_set_id, 0.0))
            .collect();
        cfr_dfs(
            rule,
            &mut regret,
            &mut reach_prob,
            &latest_strt,
            &rule.root,
            ProbContribute::new(),
        );

        for (info_set_id, reach) in &reach_prob {
            for (action, strt) in &latest_strt[info_set_id] {
                *strt_sum
                    .get_mut(info_set_id)
                    .unwrap()
                    .get_mut(action)
                    .unwrap() += reach * strt;
            }
        }
        regret_sum.add(&regret);

        latest_strt = regret_sum.to_strategy();
    }

    strt_sum
        .iter()
        .map(|(info_set_id, dist)| (*info_set_id, {
            let norm: f64 = dist.values().sum();
            dist.iter().map(|(action_id, prob)| (*action_id, prob / norm)).collect()
        }))
        .collect()
}
