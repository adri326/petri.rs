use crate::graph::{EdgeMap, PetriGraph};
use crate::simulator::Simulator;
use std::collections::HashMap;

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

    pub fn is_active(&self, nodes: &[u8]) -> bool {
        let mut active = true;
        for &input in self.inputs.iter() {
            active = active && nodes[input] > 0;
        }
        active
    }

    pub fn apply(&self, state: &mut [u8]) {
        self.apply_inputs(state);
        self.apply_outputs(state);
    }

    pub fn apply_inputs(&self, state: &mut [u8]) {
        for &input in self.inputs.iter() {
            state[input] -= 1;
        }
    }

    pub fn apply_outputs(&self, state: &mut [u8]) {
        for &output in self.outputs.iter() {
            state[output] += 1;
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

    pub fn generate_graph<'b, S: Simulator<'b>>(&'b self) -> PetriGraph {
        let mut map: EdgeMap<()> = EdgeMap::default();
        let simulator = S::init(self);

        let mut stack = vec![self.nodes.clone()];
        while let Some(current_node) = stack.pop() {
            // TODO: use a separate 'close' set
            if map.count_of(&current_node) == 0 {
                let next_states = simulator.get_next_states(&current_node);

                for next_state in next_states {
                    stack.push(next_state.clone());
                    map.add(&current_node, next_state, ());
                }
            }
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
    use crate::simulator::RecursiveBrancher;

    use super::*;

    pub(crate) fn test_recurse(network: &PetriNetwork, expected: Vec<Vec<u8>>) {
        let simulator = RecursiveBrancher::init(network);
        assert_eq!(
            simulator.get_next_states(&network.nodes),
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
