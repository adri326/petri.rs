use crate::{PetriNetwork, PetriTransition};
use std::collections::HashMap;

pub mod structures;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Label {
    Node(usize),
    Transition(usize),
}

#[derive(Clone, Debug)]
pub struct PetriBuilder {
    pub nodes: Vec<u8>,
    pub transitions: Vec<PetriTransition>,
    pub labels: HashMap<String, Label>,
}

#[derive(Debug)]
pub struct PetriTransitionBuilder<'a> {
    pub builder: &'a mut PetriBuilder,
    pub inputs: Vec<usize>,
    pub outputs: Vec<usize>,
    pub labels: Vec<String>,
}

#[derive(Debug)]
pub struct PetriBranchBuilder<'a> {
    pub builder: &'a mut PetriBuilder,
    pub current_node: usize,
}

pub trait TransitionMod {
    fn apply(self, builder: &mut PetriTransitionBuilder);
}

impl PetriBuilder {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            transitions: Vec::new(),
            labels: HashMap::new(),
        }
    }

    pub fn node(&mut self, value: u8) -> usize {
        let res = self.nodes.len();
        self.nodes.push(value);
        res
    }

    pub fn node_with_label(&mut self, value: u8, name: impl Into<String>) -> usize {
        let res = self.nodes.len();
        self.nodes.push(value);
        self.labels.insert(name.into(), Label::Node(res));
        res
    }

    pub fn label(&mut self, index: Label, name: impl Into<String>) {
        self.labels.insert(name.into(), index);
    }

    pub fn get_label(&self, name: &str) -> Option<Label> {
        self.labels.get(name).copied()
    }

    pub fn transition(&mut self, inputs: Vec<usize>, outputs: Vec<usize>) -> usize {
        let res = self.transitions.len();
        self.transitions.push(PetriTransition::new(inputs, outputs));
        res
    }

    pub fn begin_transition<'b>(&'b mut self) -> PetriTransitionBuilder<'b> {
        PetriTransitionBuilder {
            builder: self,
            inputs: Vec::new(),
            outputs: Vec::new(),
            labels: Vec::new(),
        }
    }

    pub fn begin_branch<'b>(&'b mut self, start_index: usize) -> PetriBranchBuilder<'b> {
        PetriBranchBuilder {
            builder: self,
            current_node: start_index,
        }
    }

    pub fn build(self) -> PetriNetwork {
        PetriNetwork {
            nodes: self.nodes,
            transitions: self.transitions,
        }
    }

    pub fn build_clone(&self) -> PetriNetwork {
        PetriNetwork {
            nodes: self.nodes.clone(),
            transitions: self.transitions.clone(),
        }
    }
}

impl<'a> PetriTransitionBuilder<'a> {
    pub fn input(mut self, index: usize) -> Self {
        self.inputs.push(index);

        self
    }

    pub fn output(mut self, index: usize) -> Self {
        self.outputs.push(index);

        self
    }

    pub fn label(mut self, name: impl Into<String>) -> Self {
        self.labels.push(name.into());

        self
    }

    pub fn build(self) -> usize {
        let res = self.builder.transitions.len();
        self.builder
            .transitions
            .push(PetriTransition::new(self.inputs, self.outputs));
        for name in self.labels {
            self.builder.label(Label::Transition(res), name);
        }
        res
    }

    pub fn with_mod<M: TransitionMod>(mut self, modifier: M) -> Self {
        modifier.apply(&mut self);
        self
    }
}

impl<'a> PetriBranchBuilder<'a> {
    pub fn step(mut self) -> Self {
        let next_node = self.builder.node(0);
        self.builder
            .transition(vec![self.current_node], vec![next_node]);
        self.current_node = next_node;

        self
    }

    pub fn step_with_label(mut self, name: impl Into<String>) -> Self {
        let next_node = self.builder.node(0);
        let transition = self
            .builder
            .transition(vec![self.current_node], vec![next_node]);
        self.builder
            .label(Label::Transition(transition), name.into());
        self.current_node = next_node;

        self
    }

    pub fn label(self, name: impl Into<String>) -> Self {
        self.builder
            .label(Label::Node(self.current_node), name.into());

        self
    }

    pub fn step_with_mod<M: TransitionMod>(mut self, modifier: M) -> Self {
        let next_node = self.builder.node(0);
        self.builder
            .begin_transition()
            .input(self.current_node)
            .output(next_node)
            .with_mod(modifier)
            .build();
        self.current_node = next_node;

        self
    }

    pub fn step_with_branch<F>(mut self, handler: F) -> Self
    where
        F: for<'c> Fn(PetriBranchBuilder<'c>),
    {
        let next_node = self.builder.node(0);
        let branch_node = self.builder.node(0);
        self.builder
            .transition(vec![self.current_node], vec![next_node, branch_node]);
        handler(self.builder.begin_branch(branch_node));
        self.current_node = next_node;

        self
    }

    pub fn step_with_fork<F>(mut self, handler: F) -> Self
    where
        F: for<'c> Fn(PetriBranchBuilder<'c>),
    {
        let next_node = self.builder.node(0);
        let branch_node = self.builder.node(0);
        self.builder
            .transition(vec![self.current_node], vec![next_node]);
        self.builder
            .transition(vec![self.current_node], vec![branch_node]);
        handler(self.builder.begin_branch(branch_node));
        self.current_node = next_node;

        self
    }

    pub fn join<S: AsRef<str>>(mut self, join_handles: impl IntoIterator<Item = S>) -> Self {
        let next_node = self.builder.node(0);
        let mut input_nodes = Vec::new();
        for join_handle in join_handles.into_iter() {
            input_nodes.push(
                self.builder
                    .get_label(join_handle.as_ref())
                    .unwrap()
                    .as_node()
                    .unwrap(),
            );
        }
        let mut transition = self.builder.begin_transition().input(self.current_node);
        for input in input_nodes {
            transition = transition.input(input);
        }
        transition.output(next_node).build();

        self.current_node = next_node;

        self
    }

    pub fn join_any<S: AsRef<str>>(mut self, join_handles: impl IntoIterator<Item = S>) -> Self {
        let next_node = self.builder.node(0);
        self.builder
            .transition(vec![self.current_node], vec![next_node]);
        for join_handle in join_handles.into_iter() {
            let input_node = self
                .builder
                .get_label(join_handle.as_ref())
                .unwrap()
                .as_node()
                .unwrap();
            self.builder.transition(vec![input_node], vec![next_node]);
        }
        self.current_node = next_node;

        self
    }
}

impl Label {
    pub fn as_node(self) -> Option<usize> {
        match self {
            Label::Node(res) => Some(res),
            _ => None,
        }
    }

    pub fn as_transition(self) -> Option<usize> {
        match self {
            Label::Transition(res) => Some(res),
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn build_simple_network() {
        let expected = PetriNetwork {
            nodes: vec![1, 0, 0],
            transitions: vec![
                PetriTransition::new(vec![0], vec![1]),
                PetriTransition::new(vec![1], vec![0, 2]),
            ],
        };

        let mut builder = PetriBuilder::new();
        let a = builder.node(1);
        let b = builder.node(0);
        let c = builder.node(0);
        builder.begin_transition().input(a).output(b).build();
        builder
            .begin_transition()
            .input(b)
            .output(a)
            .output(c)
            .build();
        let actual = builder.build();

        assert_eq!(actual, expected);
    }

    #[test]
    fn build_semaphore() {
        let expected = PetriNetwork {
            nodes: vec![1, 1, 1, 0, 0, 0, 0], // [in_a, in_b, mutex, mid_a, mid_b, out_a, out_b]
            transitions: vec![
                PetriTransition::new(vec![0, 2], vec![3]),
                PetriTransition::new(vec![1, 2], vec![4]),
                PetriTransition::new(vec![3], vec![5, 2]),
                PetriTransition::new(vec![4], vec![6, 2]),
            ],
        };

        // Construct this graph using the builders
        let mut builder = PetriBuilder::new();
        let in_a = builder.node(1);
        let in_b = builder.node(1);
        let mutex = structures::Semaphore::new(&mut builder, 1);
        let mid_a = builder.node(0);
        let mid_b = builder.node(0);
        let out_a = builder.node(0);
        let out_b = builder.node(0);
        // in -> mid
        builder
            .begin_transition()
            .input(in_a)
            .output(mid_a)
            .with_mod(mutex.p())
            .build();
        builder
            .begin_transition()
            .input(in_b)
            .output(mid_b)
            .with_mod(mutex.p())
            .build();
        // mid -> out
        builder
            .begin_transition()
            .input(mid_a)
            .output(out_a)
            .with_mod(mutex.v())
            .build();
        builder
            .begin_transition()
            .input(mid_b)
            .output(out_b)
            .with_mod(mutex.v())
            .build();

        let actual = builder.build();

        assert_eq!(actual, expected);

        // Construct the graph using branch builders
        let mut builder = PetriBuilder::new();
        let s_mutex = structures::Semaphore::new(&mut builder, 1);
        let s_in_a = builder.node_with_label(1, "in_a");
        let s_in_b = builder.node_with_label(1, "in_b");
        builder
            .begin_branch(s_in_a)
            .step_with_mod(s_mutex.p())
            .label("mid_a")
            .step_with_mod(s_mutex.v())
            .label("out_a");
        builder
            .begin_branch(s_in_b)
            .step_with_mod(s_mutex.p())
            .label("mid_b")
            .step_with_mod(s_mutex.v())
            .label("out_b");
        let s_mid_a = builder.get_label("mid_a").unwrap().as_node().unwrap();
        let s_mid_b = builder.get_label("mid_b").unwrap().as_node().unwrap();
        let s_out_a = builder.get_label("out_a").unwrap().as_node().unwrap();
        let s_out_b = builder.get_label("out_b").unwrap().as_node().unwrap();

        let second = builder.build();

        let graph = actual.generate_graph();
        let second_graph = second.generate_graph();
        // Verify that mid_a + mid_b ≤ 1 at all point in time
        graph.assert_forall(|state| state[mid_a] + state[mid_b] <= 1);
        second_graph.assert_forall(|state| state[s_mid_a] + state[s_mid_b] <= 1);
        // Verify that * ->> [_, _, _, _, _, 1, 1]
        graph.assert_always_reaches(|state| state[out_a] == 1 && state[out_b] == 1);
        second_graph.assert_always_reaches(|state| state[s_out_a] == 1 && state[s_out_b] == 1);
    }

    #[test]
    fn test_branch_builder() {
        let expected = PetriNetwork {
            nodes: vec![1, 0, 0, 0, 0, 0, 0],
            transitions: vec![
                PetriTransition::new(vec![0], vec![1]),
                PetriTransition::new(vec![1], vec![2, 3]),
                PetriTransition::new(vec![3], vec![4]),
                PetriTransition::new(vec![4], vec![5]),
                PetriTransition::new(vec![2, 5], vec![6]),
            ],
        };

        let mut builder = PetriBuilder::new();

        let start = builder.node(1);
        builder
            .begin_branch(start) // 0
            .step() // 1
            .step_with_branch(|branch| {
                branch // 3
                    .step() // 4
                    .step() // 5
                    .label("b1");
            }) // 2
            .join(["b1"]) // 6
            .label("end");
        let end = builder.get_label("end").unwrap().as_node().unwrap();
        let actual = builder.build();

        assert_eq!(actual, expected);

        let graph = actual.generate_graph();
        graph.assert_always_reaches(|state| state[end] == 1);
    }

    #[test]
    fn test_branch_builder_fork() {
        let expected = PetriNetwork {
            nodes: vec![1, 0, 0, 0, 0, 0, 0],
            transitions: vec![
                PetriTransition::new(vec![0], vec![1]),
                PetriTransition::new(vec![1], vec![2]),
                PetriTransition::new(vec![1], vec![3]),
                PetriTransition::new(vec![3], vec![4]),
                PetriTransition::new(vec![4], vec![5]),
                PetriTransition::new(vec![2], vec![6]),
                PetriTransition::new(vec![5], vec![6]),
            ],
        };

        let mut builder = PetriBuilder::new();

        let start = builder.node(1);
        builder
            .begin_branch(start) // 0
            .step() // 1
            .step_with_fork(|branch| {
                branch // 3
                    .step() // 4
                    .step() // 5
                    .label("b1");
            }) // 2
            .join_any(["b1"]) // 6
            .label("end");
        let end = builder.get_label("end").unwrap().as_node().unwrap();
        let actual = builder.build();

        assert_eq!(actual, expected);

        let graph = actual.generate_graph();
        graph.assert_always_reaches(|state| state[end] == 1);
    }
}