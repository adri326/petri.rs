use std::collections::{HashMap, HashSet};
use std::borrow::Cow;
use crate::graph::PetriGraph;

#[cfg(feature = "export_dot")]
mod export_dot;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PetriTransition {
    pub inputs: Vec<usize>,
    pub outputs: Vec<usize>,
}

/* Invariants:
    nodes.len() > max(max(transitions→inputs), max(transitions→outputs))
    => ∀ transition ∈ transitions, (
        ∀ input ∈ transition.inputs, input < nodes.len() &&
        ∀ output ∈ transition.outputs, output < nodes.len()
    )
*/
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PetriNetwork {
    pub(crate) nodes: Vec<u8>,
    // pub(crate) node_data: Vec<Option<PetriNodeData>>,
    pub(crate) transitions: Vec<PetriTransition>,
}

impl PetriTransition {
    pub fn new(inputs: Vec<usize>, outputs: Vec<usize>) -> Self {
        Self {
            inputs,
            outputs,
        }
    }

    #[inline]
    pub fn involves(&self, node: usize) -> bool {
        for input in self.inputs.iter() {
            if *input == node {
                return true;
            }
        }

        for output in self.outputs.iter() {
            if *output == node {
                return true;
            }
        }

        false
    }

    fn is_active(&self, nodes: &Vec<u8>) -> bool {
        let mut active = true;
        for &input in self.inputs.iter() {
            active = active && nodes[input] > 0;
        }
        active
    }

    fn apply(&self, state_in: &mut Vec<u8>, state_out: &mut Vec<u8>) {
        for &input in self.inputs.iter() {
            state_in[input] -= 1;
            state_out[input] -= 1;
        }
        for &output in self.outputs.iter() {
            state_out[output] += 1;
        }
    }
}

impl PetriNetwork {
    pub fn new(nodes: Vec<u8>, transitions: Vec<PetriTransition>) -> Self {
        let mut res = Self {
            nodes,
            transitions
        };
        let max_index = res.max_index();
        if res.nodes.len() <= max_index {
            res.nodes.resize(max_index, 0);
        }
        res
    }

    pub fn set_node(&mut self, index: usize, value: u8) {
        if self.nodes.len() <= index {
            self.nodes.resize(index, 0);
        }
        self.nodes[index] = value;
    }

    pub fn get_node(&self, index: usize) -> u8 {
        if self.nodes.len() <= index {
            0
        } else {
            self.nodes[index]
        }
    }

    pub fn nodes(&self) -> &Vec<u8> {
        &self.nodes
    }

    pub fn transitions(&self) -> &Vec<PetriTransition> {
        &self.transitions
    }

    pub fn get_transition<'b>(&'b self, index: usize) -> Option<&'b PetriTransition> {
        self.transitions.get(index)
    }

    /// This method is unsafe because it might break the class invariants.
    /// You should call `uphold_invariants` to avoid bugs.
    pub unsafe fn get_transition_mut<'b>(&'b mut self, index: usize) -> Option<&'b mut PetriTransition> {
        self.transitions.get_mut(index)
    }

    /// Makes sure that the class invariants are upheld
    pub fn uphold_invariants(&mut self) {
        let max_index = self.max_index();
        if self.nodes.len() <= max_index {
            self.nodes.resize(max_index, 0);
        }
    }

    /// A safe variant to `get_transition_mut`, which makes sure that the class invariants are upheld.
    /// It has an additional overhead, so you should use `PetriBuilder` and make changes there first.
    pub fn with_transition_mut<F, T: Copy>(&mut self, index: usize, callback: F) -> Option<T>
    where
        F: for<'c> FnOnce(&'c mut PetriTransition) -> T
    {
        match self.transitions.get_mut(index) {
            Some(ref mut transition) => {
                let res = (callback)(transition);

                let mut max_index = 0;
                for &input in transition.inputs.iter() {
                    max_index = max_index.max(input);
                }
                for &output in transition.outputs.iter() {
                    max_index = max_index.max(output);
                }

                if self.nodes.len() <= max_index {
                    self.nodes.resize(max_index, 0);
                }

                Some(res)
            }
            None => None,
        }
    }

    pub fn push_transition(&mut self, transition: PetriTransition) {
        let mut max_index = 0;
        for &input in transition.inputs.iter() {
            max_index = max_index.max(input);
        }
        for &output in transition.outputs.iter() {
            max_index = max_index.max(output);
        }

        if self.nodes.len() <= max_index {
            self.nodes.resize(max_index, 0);
        }

        self.transitions.push(transition);
    }

    pub fn step(&self) -> Vec<Vec<u8>> {
        let res = self.recurse(0, Cow::Borrowed(&self.nodes), Cow::Borrowed(&self.nodes), &self.get_order());

        if cfg!(feature = "conflict_fast") {
            res
        } else {
            // Deduplicate output (TODO: do this in `recurse`?)
            res.into_iter().collect::<HashSet<_>>().into_iter().collect::<Vec<_>>()
        }
    }

    fn get_order(&self) -> Vec<usize> {
        if cfg!(feature = "conflict_normal") {
            let mut order = (0..self.nodes.len()).collect::<Vec<_>>();
            let mut weights = vec![0usize; self.nodes.len()];
            for transition in self.transitions.iter() {
                for input in transition.inputs.iter() {
                    weights[*input] += 1;
                }
            }

            order.sort_unstable_by_key(|&i| weights[i]);
            order.reverse();

            order
        } else {
            (0..self.nodes.len()).collect::<Vec<_>>()
        }
    }

    pub fn generate_graph(&self) -> PetriGraph {
        let mut map: HashMap<Vec<u8>, HashSet<Vec<u8>>> = HashMap::new();
        let order = self.get_order();

        let mut stack = vec![self.nodes.clone()];
        while let Some(current_node) = stack.pop() {
            if map.get(&current_node).is_none() {
                let mut hashset = HashSet::new();

                let next_states = self.recurse(0, Cow::Borrowed(&current_node), Cow::Borrowed(&current_node), &order);

                let next_states = if cfg!(feature = "conflict_fast") {
                    next_states
                } else {
                    next_states.into_iter().collect::<HashSet<_>>().into_iter().collect::<Vec<_>>()
                };

                for next_state in next_states {
                    stack.push(next_state.clone());
                    hashset.insert(next_state);
                }
                map.insert(current_node.clone(), hashset);
            }
        }

        // Shrink result hashmap and hashsets
        for (_, node) in map.iter_mut() {
            node.shrink_to_fit();
        }

        map.shrink_to_fit();

        PetriGraph::new(map)
    }

    fn max_index(&self) -> usize {
        let mut max = 0;
        for transition in self.transitions.iter() {
            for &input in transition.inputs.iter() {
                max = max.max(input);
            }

            for &output in transition.outputs.iter() {
                max = max.max(output);
            }
        }
        max
    }

    fn recurse(&self, mut index: usize, state_in: Cow<Vec<u8>>, state_out: Cow<Vec<u8>>, order: &[usize]) -> Vec<Vec<u8>> {
        while index < self.nodes.len() && state_out[order[index]] == 0 {
            index += 1;
        }
        if index >= self.nodes.len() {
            return vec![state_out.into_owned()];
        }

        let mut transition_candidates = Vec::new();
        for (n, transition) in self.transitions.iter().enumerate() {
            let is_relevant = if cfg!(any(feature = "conflict_fast", feature = "conflict_normal")) {
                transition.inputs.iter().any(|x| *x == order[index])
            } else {
                true
            };

            if is_relevant && transition.is_active(&state_in) {
                transition_candidates.push(n);
            }
        }

        if transition_candidates.len() == 0 {
            return self.recurse(index + 1, state_in, state_out, order);
        }

        if cfg!(not(any(
            feature = "conflict_fast",
            feature = "conflict_normal",
            feature = "conflict_slow"
        ))) && transition_candidates.len() > 1 {
            panic!("Conflict found in network while no conflict strategy was chosen!");
        }

        let mut res = Vec::new();

        for transition in transition_candidates {
            let mut new_state_in = state_in.clone().into_owned();
            let mut new_state_out = state_out.clone().into_owned();

            self.transitions[transition].apply(&mut new_state_in, &mut new_state_out);

            res.append(&mut self.recurse(index + 1, Cow::Owned(new_state_in), Cow::Owned(new_state_out), order));
        }

        res
    }

    #[cfg(feature = "export_dot")]
    pub fn export_dot<W: std::io::Write>(&self, writer: &mut W) {
        let mut writer = dot_writer::DotWriter::from(writer);

        export_dot::export_network(self, &mut writer);
    }

    #[cfg(feature = "export_dot")]
    pub fn get_dot_string(&self) -> String {
        let mut vec = Vec::new();
        let mut writer = dot_writer::DotWriter::from(&mut vec);

        export_dot::export_network(self, &mut writer);

        String::from_utf8_lossy(&vec).into_owned()
    }
}

#[cfg(test)]
pub(crate) mod test {
    use super::*;

    pub(crate) fn test_recurse(network: &PetriNetwork, expected: Vec<Vec<u8>>) {
        assert_eq!(
            network.recurse(
                0,
                Cow::Borrowed(&network.nodes),
                Cow::Borrowed(&network.nodes),
                &network.get_order()
            ).into_iter().collect::<HashSet<_>>(),
            expected.into_iter().collect::<HashSet<_>>()
        );
    }

    #[test]
    fn test_recurse_simple() {
        let mut network = PetriNetwork {
            nodes: vec![0, 0],
            transitions: vec![]
        };

        test_recurse(&network, vec![vec![0, 0]]);

        network.nodes[0] = 1;

        test_recurse(&network, vec![vec![1, 0]]);

        network.transitions.push(PetriTransition::new(vec![0], vec![1]));

        test_recurse(&network, vec![vec![0, 1]]);

        network.nodes[1] = 1;

        test_recurse(&network, vec![vec![0, 2]]);
    }

    #[test]
    fn test_recurse_parallel() {
        let mut network = PetriNetwork {
            nodes: vec![0; 4],
            transitions: vec![
                PetriTransition::new(vec![0], vec![1]),
                PetriTransition::new(vec![2], vec![3]),
            ]
        };

        network.nodes[0] = 1;
        network.nodes[2] = 1;

        test_recurse(&network, vec![vec![0, 1, 0, 1]]);

        network.transitions.push(PetriTransition::new(vec![1], vec![2]));

        test_recurse(&network, vec![vec![0, 1, 0, 1]]);
    }

    #[test]
    fn test_recurse_fork() {
        let mut network = PetriNetwork {
            nodes: vec![0; 3],
            transitions: vec![
                PetriTransition::new(vec![0], vec![1]),
                PetriTransition::new(vec![0], vec![2]),
                PetriTransition::new(vec![1], vec![0]),
                PetriTransition::new(vec![2], vec![0]),
            ]
        };

        network.nodes[0] = 1;

        test_recurse(&network, vec![
            vec![0, 1, 0],
            vec![0, 0, 1],
        ]);
    }

    #[test]
    fn test_recurse_join() {
        let mut network = PetriNetwork {
            nodes: vec![1, 1, 0],
            transitions: vec![
                PetriTransition::new(vec![0, 1], vec![2]),
            ]
        };

        test_recurse(&network, vec![vec![0, 0, 1]]);

        network.nodes[0] = 0;
        test_recurse(&network, vec![vec![0, 1, 0]]);

        network.nodes[0] = 1;
        network.nodes[1] = 0;
        test_recurse(&network, vec![vec![1, 0, 0]]);
    }

    #[test]
    #[cfg(any(feature = "conflict_normal", feature = "conflict_slow"))]
    fn test_recurse_conflict() {
        let mut network = PetriNetwork {
            nodes: vec![1, 1, 1, 0],
            transitions: vec![
                PetriTransition::new(vec![0, 1], vec![3]),
                PetriTransition::new(vec![1, 2], vec![3]),
            ]
        };

        test_recurse(&network, vec![
            vec![0, 0, 1, 1],
            vec![1, 0, 0, 1],
        ]);

        network.transitions.reverse();

        test_recurse(&network, vec![
            vec![0, 0, 1, 1],
            vec![1, 0, 0, 1],
        ]);
    }
}
