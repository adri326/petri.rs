use std::collections::HashSet;

/* Invariants:
- `inputs` has no duplicates
- `outputs` has no duplicates
*/
#[derive(Clone, Debug)]
pub struct PetriTransition {
    inputs: HashSet<usize>,
    outputs: HashSet<usize>,
    pub groups: Vec<String>,
    pub label: Option<String>,
    pub probability: f64,
}

impl PetriTransition {
    pub fn new(inputs: Vec<usize>, outputs: Vec<usize>, groups: Vec<String>) -> Self {
        Self {
            inputs: inputs.into_iter().collect(),
            outputs: outputs.into_iter().collect(),
            groups,
            label: None,
            probability: 1.0,
        }
    }

    pub fn inputs(&self) -> &HashSet<usize> {
        &self.inputs
    }

    pub fn outputs(&self) -> &HashSet<usize> {
        &self.outputs
    }

    pub fn add_input(&mut self, input: usize) {
        if !self.inputs.contains(&input) {
            self.inputs.insert(input);
        }
    }

    pub fn add_output(&mut self, output: usize) {
        if !self.outputs.contains(&output) {
            self.outputs.insert(output);
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
