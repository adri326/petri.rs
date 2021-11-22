use crate::{PetriTransition, PetriNetwork};

pub mod structures;

pub struct PetriBuilder {
    pub nodes: Vec<u8>,
    pub transitions: Vec<PetriTransition>,
}

pub struct PetriTransitionBuilder<'a> {
    pub builder: &'a mut PetriBuilder,
    pub inputs: Vec<usize>,
    pub outputs: Vec<usize>,
}

pub trait TransitionMod {
    fn apply(self, builder: &mut PetriTransitionBuilder);
}

impl PetriBuilder {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            transitions: Vec::new(),
        }
    }

    pub fn node(&mut self, value: u8) -> usize {
        let res = self.nodes.len();
        self.nodes.push(value);
        res
    }

    pub fn transition(&mut self, inputs: Vec<usize>, outputs: Vec<usize>) -> usize {
        let res = self.transitions.len();
        self.transitions.push(PetriTransition::new(inputs, outputs));
        res
    }

    pub fn build_transition<'b>(&'b mut self) -> PetriTransitionBuilder<'b> {
        PetriTransitionBuilder {
            builder: self,
            inputs: Vec::new(),
            outputs: Vec::new(),
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

    pub fn build(self) -> usize {
        let res = self.builder.transitions.len();
        self.builder.transitions.push(PetriTransition::new(self.inputs, self.outputs));
        res
    }

    pub fn with_mod<M: TransitionMod>(mut self, modifier: M) -> Self {
        modifier.apply(&mut self);
        self
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
            ]
        };

        let mut builder = PetriBuilder::new();
        let a = builder.node(1);
        let b = builder.node(0);
        let c = builder.node(0);
        builder.build_transition()
            .input(a)
            .output(b)
            .build();
        builder.build_transition()
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
            ]
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
        builder.build_transition()
            .input(in_a)
            .output(mid_a)
            .with_mod(mutex.p())
            .build();
        builder.build_transition()
            .input(in_b)
            .output(mid_b)
            .with_mod(mutex.p())
            .build();
        // mid -> out
        builder.build_transition()
            .input(mid_a)
            .output(out_a)
            .with_mod(mutex.v())
            .build();
        builder.build_transition()
            .input(mid_b)
            .output(out_b)
            .with_mod(mutex.v())
            .build();

        let actual = builder.build();

        assert_eq!(actual, expected);

        let graph = actual.generate_graph();
        // Verify that mid_a + mid_b ≤ 1 at all point in time
        graph.assert_forall(|state| state[mid_a] + state[mid_b] <= 1);
        // Verify that * ->> [_, _, _, _, _, 1, 1]
        graph.assert_always_reaches(|state| state[out_a] == 1 && state[out_b] == 1);
    }
}
