use crate::graph::PetriGraph;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

pub mod data;
use data::PetriNodeData;

pub(crate) mod export_dot;

// TODO: reimplement PartialEq
#[derive(Clone, Debug)]
pub struct PetriTransition {
    pub inputs: Vec<usize>,
    pub outputs: Vec<usize>,
    pub groups: Vec<String>,
    pub label: Option<String>,
}

/* Invariants:
    nodes.len() > max(max(transitions→inputs), max(transitions→outputs))
    => ∀ transition ∈ transitions, (
        ∀ input ∈ transition.inputs, input < nodes.len() &&
        ∀ output ∈ transition.outputs, output < nodes.len()
    )

    node_data.len() = nodes.len()
*/
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PetriNetwork {
    pub(crate) nodes: Vec<u8>,
    pub(crate) node_data: Vec<PetriNodeData>,
    pub(crate) transitions: Vec<PetriTransition>,
}

impl PetriTransition {
    pub fn new(inputs: Vec<usize>, outputs: Vec<usize>, groups: Vec<String>) -> Self {
        Self {
            inputs,
            outputs,
            groups,
            label: None,
        }
    }

    pub fn label(&mut self, label: String) {
        self.label = Some(label);
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

impl PartialEq for PetriTransition {
    fn eq(&self, other: &Self) -> bool {
        self.inputs == other.inputs && self.outputs == other.outputs
    }
}

impl Eq for PetriTransition {}

impl PetriNetwork {
    pub fn new(
        nodes: Vec<u8>,
        node_data: Vec<PetriNodeData>,
        transitions: Vec<PetriTransition>,
    ) -> Self {
        let mut res = Self {
            nodes,
            node_data,
            transitions,
        };
        let max_index = res.max_index();
        if res.nodes.len() <= max_index {
            res.nodes.resize(max_index, 0);
        }
        res.node_data
            .resize(res.nodes.len(), PetriNodeData::default());
        res
    }

    pub fn set_node(&mut self, index: usize, value: u8, data: PetriNodeData) {
        if self.nodes.len() <= index {
            self.nodes.resize(index, 0);
            self.node_data.resize(index, Default::default());
        }
        self.nodes[index] = value;
        self.node_data[index] = data;
    }

    pub fn get_node(&self, index: usize) -> Option<(u8, &PetriNodeData)> {
        if self.nodes.len() <= index {
            None
        } else {
            Some((self.nodes[index], &self.node_data[index]))
        }
    }

    pub fn nodes(&self) -> &Vec<u8> {
        &self.nodes
    }

    pub fn node_data(&self) -> &Vec<PetriNodeData> {
        &self.node_data
    }

    pub fn transitions(&self) -> &Vec<PetriTransition> {
        &self.transitions
    }

    pub fn get_transition<'b>(&'b self, index: usize) -> Option<&'b PetriTransition> {
        self.transitions.get(index)
    }

    /// This method is unsafe because it might break the class invariants.
    /// You should call `uphold_invariants` to avoid bugs.
    pub unsafe fn get_transition_mut<'b>(
        &'b mut self,
        index: usize,
    ) -> Option<&'b mut PetriTransition> {
        self.transitions.get_mut(index)
    }

    /// Makes sure that the class invariants are upheld
    pub fn uphold_invariants(&mut self) {
        let max_index = self.max_index();
        if self.nodes.len() <= max_index {
            self.nodes.resize(max_index, 0);
        }
        self.node_data.resize(self.nodes.len(), Default::default());
    }

    /// A safe variant to `get_transition_mut`, which makes sure that the class invariants are upheld.
    /// It has an additional overhead, so you should use `PetriBuilder` and make changes there first.
    pub fn with_transition_mut<F, T: Copy>(&mut self, index: usize, callback: F) -> Option<T>
    where
        F: for<'c> FnOnce(&'c mut PetriTransition) -> T,
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
                    self.node_data.resize(max_index, Default::default());
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
        let res = self.get_next_states(&self.nodes);

        // Deduplicate output (TODO: do this in `recurse`?)
        res.into_iter()
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>()
    }

    fn get_order(&self) -> Vec<usize> {
        // TODO: memoize?
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
    }

    pub fn generate_graph(&self) -> PetriGraph {
        let mut map: HashMap<Vec<u8>, HashSet<Vec<u8>>> = HashMap::new();

        let mut stack = vec![self.nodes.clone()];
        while let Some(current_node) = stack.pop() {
            if map.get(&current_node).is_none() {
                let mut hashset = HashSet::new();

                let next_states = self.get_next_states(&current_node);

                let next_states = next_states
                    .into_iter()
                    .collect::<HashSet<_>>()
                    .into_iter()
                    .collect::<Vec<_>>();

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

    fn get_next_states(&self, state: &Vec<u8>) -> Vec<Vec<u8>> {
        assert!(state.len() == self.nodes.len());

        self.recurse(
            0,
            Cow::Borrowed(state),
            Cow::Borrowed(state),
            &self.get_order(),
            &mut vec![false; self.transitions.len()],
        )
    }

    /// Recursively attempts to progress the simulation to the next step.
    ///
    /// When used, you should set `index` to 0, `state_in` and `state_out` to the same
    /// state, `order` to `get_order()` and `brownout` to `[false; state_in.len()]`
    fn recurse(
        &self,
        mut index: usize,
        state_in: Cow<Vec<u8>>,
        state_out: Cow<Vec<u8>>,
        order: &[usize],
        brownout: &mut Vec<bool>,
    ) -> Vec<Vec<u8>> {
        while index < self.nodes.len() && state_out[order[index]] == 0 {
            index += 1;
        }
        if index >= self.nodes.len() {
            return vec![state_out.into_owned()];
        }

        debug_assert!(state_in.len() == state_out.len());
        debug_assert!(brownout.len() == self.transitions.len());

        let mut transition_candidates = Vec::new();
        for (n, transition) in self.transitions.iter().enumerate() {
            if !brownout[n] && transition.is_active(&state_in) {
                transition_candidates.push(n);
            }
        }

        if transition_candidates.len() == 0 {
            return self.recurse(index + 1, state_in, state_out, order, &mut brownout.clone());
        }

        let mut res = Vec::new();

        for transition in transition_candidates {
            let mut new_state_in = state_in.clone().into_owned();
            let mut new_state_out = state_out.clone().into_owned();

            self.transitions[transition].apply(&mut new_state_in, &mut new_state_out);

            brownout[transition] = true;

            res.append(&mut self.recurse(
                index + 1,
                Cow::Owned(new_state_in),
                Cow::Owned(new_state_out),
                order,
                brownout,
            ));

            brownout[transition] = false;
        }

        res
    }

    pub fn export_dot<W: std::io::Write>(&self, writer: &mut W) {
        let mut writer = dot_writer::DotWriter::from(writer);

        export_dot::export_network(self, &mut writer);
    }

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
            network
                .get_next_states(&network.nodes)
                .into_iter()
                .collect::<HashSet<_>>(),
            expected.into_iter().collect::<HashSet<_>>()
        );
    }

    #[test]
    fn test_recurse_simple() {
        let mut network = PetriNetwork {
            nodes: vec![0, 0],
            node_data: vec![Default::default(); 2],
            transitions: vec![],
        };

        test_recurse(&network, vec![vec![0, 0]]);

        network.nodes[0] = 1;

        test_recurse(&network, vec![vec![1, 0]]);

        network
            .transitions
            .push(PetriTransition::new(vec![0], vec![1], vec![]));

        test_recurse(&network, vec![vec![0, 1]]);

        network.nodes[1] = 1;

        test_recurse(&network, vec![vec![0, 2]]);
    }

    #[test]
    fn test_recurse_parallel() {
        let mut network = PetriNetwork {
            nodes: vec![0; 4],
            node_data: vec![Default::default(); 4],
            transitions: vec![
                PetriTransition::new(vec![0], vec![1], vec![]),
                PetriTransition::new(vec![2], vec![3], vec![]),
            ],
        };

        network.nodes[0] = 1;
        network.nodes[2] = 1;

        test_recurse(&network, vec![vec![0, 1, 0, 1]]);

        network
            .transitions
            .push(PetriTransition::new(vec![1], vec![2], vec![]));

        test_recurse(&network, vec![vec![0, 1, 0, 1]]);
    }

    #[test]
    fn test_recurse_fork() {
        let mut network = PetriNetwork {
            nodes: vec![0; 3],
            node_data: vec![Default::default(); 3],
            transitions: vec![
                PetriTransition::new(vec![0], vec![1], vec![]),
                PetriTransition::new(vec![0], vec![2], vec![]),
                PetriTransition::new(vec![1], vec![0], vec![]),
                PetriTransition::new(vec![2], vec![0], vec![]),
            ],
        };

        network.nodes[0] = 1;

        test_recurse(&network, vec![vec![0, 1, 0], vec![0, 0, 1]]);
    }

    #[test]
    fn test_recurse_join() {
        let mut network = PetriNetwork {
            nodes: vec![1, 1, 0],
            node_data: vec![Default::default(); 3],
            transitions: vec![PetriTransition::new(vec![0, 1], vec![2], vec![])],
        };

        test_recurse(&network, vec![vec![0, 0, 1]]);

        network.nodes[0] = 0;
        test_recurse(&network, vec![vec![0, 1, 0]]);

        network.nodes[0] = 1;
        network.nodes[1] = 0;
        test_recurse(&network, vec![vec![1, 0, 0]]);
    }

    #[test]
    fn test_recurse_conflict() {
        let mut network = PetriNetwork {
            nodes: vec![1, 1, 1, 0],
            node_data: vec![Default::default(); 4],
            transitions: vec![
                PetriTransition::new(vec![0, 1], vec![3], vec![]),
                PetriTransition::new(vec![1, 2], vec![3], vec![]),
            ],
        };

        test_recurse(&network, vec![vec![0, 0, 1, 1], vec![1, 0, 0, 1]]);

        network.transitions.reverse();

        test_recurse(&network, vec![vec![0, 0, 1, 1], vec![1, 0, 0, 1]]);
    }
}
