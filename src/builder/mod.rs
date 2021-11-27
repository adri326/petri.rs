use crate::{PetriNetwork, PetriTransition, PetriNodeData};
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
    pub groups: Vec<Vec<String>>,
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
    pub modifiers: Vec<Box<dyn BuilderMod + 'static>>,
    pub builder: &'a mut PetriBuilder,
    pub current_node: usize,
}

/// A trait for defining shared, custom operations on nodes and transitions.
/// When implementing this trait, you should also implement the `Debug` and `Clone` traits.
/// You should then implement the `apply_transition` method, which will apply the modifications to the transition builder.
/// The `apply_node` method lets you customize what happens when a new node is created.
pub trait BuilderMod: std::fmt::Debug + BuilderModClone {
    fn apply_transition(&self, builder: &mut PetriTransitionBuilder);

    fn apply_node(&self, _builder: &mut PetriBuilder, _index: usize) {
        // noop
    }
}

pub trait BuilderModClone {
    fn clone_box(&self) -> Box<dyn BuilderMod + 'static>;
}

impl<T> BuilderModClone for T
where
    T: Clone + BuilderMod + 'static
{
    fn clone_box(&self) -> Box<dyn BuilderMod + 'static> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn BuilderMod + 'static> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

impl PetriBuilder {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            groups: Vec::new(),
            transitions: Vec::new(),
            labels: HashMap::new(),
        }
    }

    pub fn node(&mut self, value: u8) -> usize {
        let res = self.nodes.len();
        self.nodes.push(value);
        self.groups.push(Vec::new());
        res
    }

    pub fn node_with_label(&mut self, value: u8, name: impl Into<String>) -> usize {
        let res = self.nodes.len();
        self.nodes.push(value);
        self.groups.push(Vec::new());
        self.labels.insert(name.into(), Label::Node(res));
        res
    }

    pub fn node_with_groups(&mut self, value: u8, groups: Vec<String>) -> usize {
        let res = self.nodes.len();
        self.nodes.push(value);
        self.groups.push(groups);
        res
    }

    pub fn label(&mut self, index: Label, name: impl Into<String>) {
        self.labels.insert(name.into(), index);
    }

    pub fn get_label(&self, name: &str) -> Option<Label> {
        self.labels.get(name).copied()
    }

    pub fn add_group(&mut self, index: usize, name: impl Into<String>) {
        self.groups[index].push(name.into());
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
            modifiers: Vec::new(),
            current_node: start_index,
        }
    }

    fn build_data(&self) -> Vec<PetriNodeData> {
        self.nodes.iter().enumerate().map(|(index, _node)| {
            let mut label = None;
            for (name, i) in self.labels.iter() {
                if *i == Label::Node(index) {
                    label = Some(name.clone());
                }
            }

            let groups = self.groups.get(index).cloned().unwrap_or(Vec::new());
            let max_value = None;

            PetriNodeData {
                label,
                groups,
                max_value,
            }
        }).collect()
    }

    pub fn build(self) -> PetriNetwork {
        PetriNetwork {
            node_data: self.build_data(),
            nodes: self.nodes,
            transitions: self.transitions,
        }
    }

    pub fn build_clone(&self) -> PetriNetwork {
        PetriNetwork {
            nodes: self.nodes.clone(),
            node_data: self.build_data(),
            transitions: self.transitions.clone(),
        }
    }
}

impl<'a> PetriTransitionBuilder<'a> {
    pub fn input(mut self, index: usize) -> Self {
        self.inputs.push(index);

        self
    }

    pub fn inputs(mut self, mut indices: Vec<usize>) -> Self {
        self.inputs.append(&mut indices);

        self
    }

    pub fn output(mut self, index: usize) -> Self {
        self.outputs.push(index);

        self
    }

    pub fn outputs(mut self, mut indices: Vec<usize>) -> Self {
        self.outputs.append(&mut indices);

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

    pub fn with_mod<M: BuilderMod>(mut self, modifier: &M) -> Self {
        modifier.apply_transition(&mut self);
        self
    }

    pub fn with_mods(mut self, modifiers: &Vec<Box<dyn BuilderMod>>) -> Self {
        for modifier in modifiers.iter() {
            modifier.apply_transition(&mut self);
        }

        self
    }
}

impl<'a> PetriBranchBuilder<'a> {
    pub fn with_mod<M: BuilderMod + 'static>(mut self, modifier: M) -> Self {
        self.modifiers.push(Box::new(modifier));

        self
    }

    pub fn without_mod(mut self) -> Self {
        self.modifiers = Vec::new(); // drop all the modifiers

        self
    }

    fn node(&mut self, value: u8) -> usize {
        let res = self.builder.node(value);
        for modifier in self.modifiers.iter() {
            modifier.apply_node(&mut self.builder, res);
        }
        res
    }

    pub fn step(mut self) -> Self {
        let next_node = self.node(0);
        self.builder
            .begin_transition()
            .input(self.current_node)
            .output(next_node)
            .with_mods(&self.modifiers)
            .build();
        self.current_node = next_node;

        self
    }

    pub fn step_with_label(mut self, name: impl Into<String>) -> Self {
        let next_node = self.node(0);
        let transition = self
            .builder
            .begin_transition()
            .input(self.current_node)
            .output(next_node)
            .with_mods(&self.modifiers)
            .build();
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

    pub fn step_with_mod<M: BuilderMod>(mut self, modifier: M) -> Self {
        let next_node = self.node(0);
        modifier.apply_node(self.builder, next_node);
        self.builder
            .begin_transition()
            .input(self.current_node)
            .output(next_node)
            .with_mods(&self.modifiers)
            .with_mod(&modifier)
            .build();
        self.current_node = next_node;

        self
    }

    pub fn step_with_branch<F>(mut self, handler: F) -> Self
    where
        F: for<'c> Fn(PetriBranchBuilder<'c>),
    {
        let next_node = self.node(0);
        let branch_node = self.node(0);
        self.builder
            .begin_transition()
            .input(self.current_node)
            .output(next_node)
            .output(branch_node)
            .with_mods(&self.modifiers)
            .build();
        handler(self.builder.begin_branch(branch_node));
        self.current_node = next_node;

        self
    }

    pub fn step_with_fork<F>(mut self, handler: F) -> Self
    where
        F: for<'c> Fn(PetriBranchBuilder<'c>),
    {
        let next_node = self.node(0);
        let branch_node = self.node(0);
        self.builder
            .begin_transition()
            .input(self.current_node)
            .output(next_node)
            .with_mods(&self.modifiers)
            .build();
        self.builder
            .begin_transition()
            .input(self.current_node)
            .output(branch_node)
            .with_mods(&self.modifiers)
            .build();
        handler(self.builder.begin_branch(branch_node));
        self.current_node = next_node;

        self
    }

    pub fn join<S: AsRef<str>>(mut self, join_handles: impl IntoIterator<Item = S>) -> Self {
        let next_node = self.node(0);
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
        transition.output(next_node).with_mods(&self.modifiers).build();

        self.current_node = next_node;

        self
    }

    pub fn join_any<S: AsRef<str>>(mut self, join_handles: impl IntoIterator<Item = S>) -> Self {
        let next_node = self.node(0);
        self.builder
            .transition(vec![self.current_node], vec![next_node]);
        for join_handle in join_handles.into_iter() {
            let input_node = self
                .builder
                .get_label(join_handle.as_ref())
                .unwrap()
                .as_node()
                .unwrap();
            self.builder.begin_transition()
                .input(input_node)
                .output(next_node)
                .with_mods(&self.modifiers)
                .build();
        }
        self.current_node = next_node;

        self
    }

    pub fn step_with_goto(mut self, goto_handle: impl AsRef<str>) -> Self {
        let goto_node = self.builder.get_label(goto_handle.as_ref()).unwrap().as_node().unwrap();
        let next_node = self.node(0);
        self.builder.begin_transition()
            .input(self.current_node)
            .output(goto_node)
            .output(next_node)
            .with_mods(&self.modifiers)
            .build();
        self.current_node = next_node;

        self
    }

    pub fn goto(mut self, goto_handle: impl AsRef<str>) -> Self {
        let goto_node = self.builder.get_label(goto_handle.as_ref()).unwrap().as_node().unwrap();
        self.builder.begin_transition()
            .input(self.current_node)
            .output(goto_node)
            .with_mods(&self.modifiers)
            .build();
        self.current_node = goto_node;

        self
    }

    pub fn goto_branch<S: AsRef<str>>(mut self, goto_handles: impl IntoIterator<Item = S>) -> Self {
        let output_nodes = goto_handles.into_iter()
            .map(|label| self.builder.get_label(label.as_ref()).unwrap().as_node().unwrap()).collect::<Vec<_>>();
        assert!(output_nodes.len() > 0);
        let next_node = output_nodes[0];
        self.builder.begin_transition()
            .input(self.current_node)
            .outputs(output_nodes)
            .with_mods(&self.modifiers)
            .build();
        self.current_node = next_node;

        self
    }

    pub fn with_clone<F>(self, callback: F) -> Self
    where
        F: for<'c> Fn(PetriBranchBuilder<'c>),
    {
        {
            let mut clone = self.builder.begin_branch(self.current_node);
            clone.modifiers = self.modifiers.clone();
            (callback)(clone);
        }
        self
    }

    /// TODO: replace S with a Condition enum
    pub fn step_with_condition<S: AsRef<str>>(mut self, conditions: impl IntoIterator<Item = S>) -> Self {
        let conditions = conditions.into_iter()
            .map(|label| self.builder.get_label(label.as_ref()).unwrap().as_node().unwrap()).collect::<Vec<_>>();
        let mut input_nodes = conditions.clone();
        input_nodes.push(self.current_node);
        let next_node = self.node(0);
        let mut output_nodes = conditions;
        output_nodes.push(next_node);

        self.builder.begin_transition()
            .inputs(input_nodes)
            .outputs(output_nodes)
            .with_mods(&self.modifiers);
        self.current_node = next_node;

        self
    }

    pub fn consume(self) {
        self.builder.transition(vec![self.current_node], vec![]);
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
            node_data: vec![Default::default(); 3],
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
            node_data: vec![Default::default(); 7],
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
            .with_mod(&mutex.p())
            .build();
        builder
            .begin_transition()
            .input(in_b)
            .output(mid_b)
            .with_mod(&mutex.p())
            .build();
        // mid -> out
        builder
            .begin_transition()
            .input(mid_a)
            .output(out_a)
            .with_mod(&mutex.v())
            .build();
        builder
            .begin_transition()
            .input(mid_b)
            .output(out_b)
            .with_mod(&mutex.v())
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
            node_data: vec![Default::default(); 7],
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
            node_data: vec![Default::default(); 7],
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

    #[test]
    fn test_branch_modifiers() {
        let expected = PetriNetwork {
            nodes: vec![2, 0, 0, 0, 0],
            node_data: vec![
                Default::default(),
                PetriNodeData::default().label("M"),
                Default::default(),
                Default::default(),
                Default::default(),
            ],
            transitions: vec![
                PetriTransition::new(vec![0, 1], vec![2]),
                PetriTransition::new(vec![2], vec![3]),
                PetriTransition::new(vec![3], vec![4, 1]),
            ]
        };

        let mut builder = PetriBuilder::new();
        let start = builder.node(2);
        let mutex = structures::Mutex::new(&mut builder, 0, "M");

        builder.begin_branch(start)
            .step_with_mod(mutex.p())
            .with_mod(mutex.section())
            .step()
            .without_mod()
            .step_with_mod(mutex.v());

        println!("{:?}", builder.groups);

        assert_eq!(builder.groups[0], Vec::<String>::new());
        assert_eq!(builder.groups[1], vec!["M"]);
        assert_eq!(builder.groups[2], vec!["M"]);
        assert_eq!(builder.groups[3], vec!["M"]);
        assert_eq!(builder.groups[4], Vec::<String>::new());

        let actual = builder.build();

        assert_eq!(actual, expected);
        assert!(actual.get_node(1).is_some());
        assert_eq!(actual.get_node(1).unwrap().1.groups, vec!["M"]);
        assert!(actual.get_node(2).is_some());
        assert_eq!(actual.get_node(2).unwrap().1.groups, vec!["M"]);
        assert!(actual.get_node(3).is_some());
        assert_eq!(actual.get_node(3).unwrap().1.groups, vec!["M"]);
    }
}
