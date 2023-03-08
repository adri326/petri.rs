use std::collections::{HashSet, HashMap};

use crate::*;

use super::Simulator;

/// A simulator that thoroughly checks every combination of conflicting transitions.
/// Only combinations that are exhaustive are kept, ie. combinations that do not leave
/// a transition in a state where it hasn't fired but can fire.
pub struct ExhaustiveBrancher<'a> {
    network: &'a PetriNetwork,
}

#[derive(Debug)]
struct TransitionGroup<'a> {
    transitions: Vec<&'a PetriTransition>,
    nodes: HashSet<usize>,
}

#[derive(Debug)]
struct ResolvedTransitionGroup<'a>(Vec<Vec<&'a PetriTransition>>);

impl<'a> Simulator<'a> for ExhaustiveBrancher<'a> {
    fn init(network: &'a PetriNetwork) -> Self {
        // TODO: pre-compute groups of transitions here
        Self { network }
    }

    fn get_next_states(&self, state: &[u8]) -> HashMap<Vec<u8>, f64> {
        let groups = self.group_active_transitions(state);
        let groups = groups
            .iter()
            .map(|group| group.resolve(state))
            .collect::<Vec<_>>();

        self.recurse(&groups, Vec::from(state), 1.0)
    }
}

impl<'a> ExhaustiveBrancher<'a> {
    fn recurse(&self, groups: &[ResolvedTransitionGroup], state: Vec<u8>, current_probability: f64) -> HashMap<Vec<u8>, f64> {
        let mut res = HashMap::new();

        if groups.len() == 0 {
            res.insert(state, current_probability);
            return res;
        }

        for transitions in &groups[0].0 {
            let mut new_state = state.clone();
            let mut current_probability = current_probability;

            for transition in transitions {
                transition.apply(&mut new_state);
                current_probability *= transition.probability;
            }

            res.extend(self.recurse(&groups[1..], new_state, current_probability));
        }

        res
    }

    fn group_active_transitions<'b>(&'b self, state: &[u8]) -> Vec<TransitionGroup<'b>> {
        let mut groups: Vec<TransitionGroup> = Vec::new();

        for transition in self.network.transitions() {
            if !transition.is_active(state) {
                continue;
            }

            let mut found_group = false;
            for group in &mut groups {
                found_group = transition
                    .inputs()
                    .iter()
                    .any(|input| group.nodes.contains(input));

                if found_group {
                    group.transitions.push(transition);
                    for input in transition.inputs() {
                        group.nodes.insert(*input);
                    }
                    break;
                }
            }

            if !found_group {
                groups.push(TransitionGroup {
                    transitions: vec![transition],
                    nodes: transition.inputs().iter().copied().collect(),
                });
            }
        }

        // Combine together groups that overlap (which can happen when a transition "bridges" two groups)
        let mut final_groups: Vec<TransitionGroup> = Vec::with_capacity(groups.len());

        for mut group in groups {
            if let Some(target_group) = final_groups
                .iter_mut()
                .find(|final_group| !group.nodes.is_disjoint(&final_group.nodes))
            {
                target_group.transitions.append(&mut group.transitions);
                target_group.nodes.extend(group.nodes);
            } else {
                final_groups.push(group);
            }
        }

        final_groups.shrink_to_fit();
        final_groups
    }
}

impl<'a> TransitionGroup<'a> {
    fn resolve_recurse(
        &self,
        mut remaining_state: Vec<u8>,
        current_choice: &[bool],
    ) -> Vec<Vec<bool>> {
        let index = current_choice.len();

        if index >= self.transitions.len() {
            if self
                .transitions
                .iter()
                .enumerate()
                .any(|(index, transition)| {
                    !current_choice[index] && transition.is_active(&remaining_state)
                })
            {
                // Invalid state: there is still an unfired transition
                return vec![];
            } else {
                return vec![Vec::from(current_choice)];
            }
        }

        if !self.transitions[index].is_active(&remaining_state) {
            let mut next_choice = Vec::with_capacity(current_choice.len() + 1);
            next_choice.extend(current_choice);
            next_choice.push(false);
            return self.resolve_recurse(remaining_state, &next_choice);
        }

        let mut res = Vec::new();
        let mut next_choice = Vec::with_capacity(current_choice.len() + 1);
        next_choice.extend(current_choice);

        if self
            .transitions
            .iter()
            .skip(index)
            .any(|transition| transition.is_active(&remaining_state))
        {
            next_choice.push(false);
            res.append(&mut self.resolve_recurse(remaining_state.clone(), &next_choice));
            next_choice.pop();
        }

        next_choice.push(true);
        self.transitions[index].apply_inputs(&mut remaining_state);

        res.append(&mut self.resolve_recurse(remaining_state, &next_choice));

        return res;
    }

    fn resolve(&'a self, state: &[u8]) -> ResolvedTransitionGroup<'a> {
        let choices = self.resolve_recurse(Vec::from(state), &[]);

        ResolvedTransitionGroup(
            choices
                .into_iter()
                .map(|choice| {
                    self.transitions
                        .iter()
                        .enumerate()
                        .filter(|(index, _)| choice[*index])
                        .map(|(_, transition)| *transition)
                        .collect()
                })
                .collect(),
        )
    }
}
