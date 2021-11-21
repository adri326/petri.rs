use std::collections::{HashMap, HashSet};
use std::cmp::Ordering;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PetriTransition {
    pub inputs: Vec<usize>,
    pub outputs: Vec<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PetriNetwork {
    nodes: Vec<u8>,
    transitions: Vec<PetriTransition>,
}

#[derive(Clone, Debug)]
pub struct PetriGraph {
    map: HashMap<Vec<u8>, HashSet<Vec<u8>>>,
}

impl PetriTransition {
    pub fn new(inputs: Vec<usize>, outputs: Vec<usize>) -> Self {
        Self {
            inputs,
            outputs,
        }
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
        Self {
            nodes,
            transitions
        }
    }

    pub fn step(&self) -> Vec<Vec<u8>> {
        let max_index = self.max_index();

        let mut nodes = Vec::with_capacity(max_index + 1);
        nodes.extend(self.nodes.iter());
        while nodes.len() <= max_index {
            nodes.push(0);
        }

        self.recurse(0, nodes.clone(), nodes)
    }

    pub fn generate_graph(&self) -> PetriGraph {
        let mut map: HashMap<Vec<u8>, HashSet<Vec<u8>>> = HashMap::new();

        let mut stack = vec![self.nodes.clone()];
        while let Some(current_node) = stack.pop() {
            for next_state in self.recurse(0, current_node.clone(), current_node.clone()) {
                if let Some(ref mut node) = map.get_mut(&next_state) {
                    // Entry in graph already exists, add the next state (does nothing if the hashset already has it)
                    node.insert(next_state);
                } else {
                    // Entry in graph doesn't already exist, add it with `next_state` as only value
                    stack.push(next_state.clone());
                    let mut hashset = HashSet::new();
                    hashset.insert(next_state);
                    map.insert(current_node.clone(), hashset);
                }
            }
        }

        // Shrink result hashmap and hashsets
        for (_, node) in map.iter_mut() {
            node.shrink_to_fit();
        }

        map.shrink_to_fit();

        PetriGraph {
            map
        }
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

    fn recurse(&self, mut index: usize, state_in: Vec<u8>, state_out: Vec<u8>) -> Vec<Vec<u8>> {
        while state_out.get(index) == Some(&0) {
            index += 1;
        }
        if index >= self.nodes.len() {
            return vec![state_out];
        }

        let mut transition_candidates = Vec::new();
        for (n, transition) in self.transitions.iter().enumerate() {
            if transition.inputs.iter().any(|x| *x == index) && transition.is_active(&state_in) {
                transition_candidates.push(n);
            }
        }

        if transition_candidates.len() == 0 {
            return self.recurse(index + 1, state_in, state_out);
        }

        let mut res = Vec::new();

        for transition in transition_candidates {
            let mut new_state_in = state_in.clone();
            let mut new_state_out = state_out.clone();

            self.transitions[transition].apply(&mut new_state_in, &mut new_state_out);

            res.append(&mut self.recurse(index + 1, new_state_in, new_state_out));
        }

        res
    }
}

impl PetriGraph {
    pub fn get<'b>(&'b self, state: &Vec<u8>) -> Option<&'b HashSet<Vec<u8>>> {
        self.map.get(state)
    }

    pub fn union<'b>(&'b self, other: Self) -> Self {
        let mut map = HashMap::with_capacity(self.map.len() + other.map.len());

        map.extend(other.map.into_iter());

        // Merge the elements of `other` into `map`
        for (node, next_states) in self.map.iter() {
            match map.get_mut(node) {
                Some(ref mut res_states) => {
                    res_states.extend(next_states.iter().cloned());
                }
                None => {
                    map.insert(node.clone(), next_states.clone());
                }
            }
        }

        // Shrink result hashmap and hashsets
        for (_, node) in map.iter_mut() {
            node.shrink_to_fit();
        }

        map.shrink_to_fit();

        Self {
            map
        }
    }
}

impl<T: IntoIterator, U: IntoIterator<Item=Vec<u8>>> From<T> for PetriGraph
where
    T: IntoIterator<Item = (Vec<u8>, U)>,
{
    fn from(iter: T) -> Self {
        let iter = iter.into_iter().map(|(k, n)| (k, n.into_iter().collect()));
        Self {
            map: iter.collect(),
        }
    }
}

impl PartialEq for PetriGraph {
    fn eq(&self, other: &Self) -> bool {
        if self.map.len() != other.map.len() {
            return false;
        }

        for (node, self_states) in self.map.iter() {
            if let Some(other_states) = other.map.get(node) {
                if self_states != other_states {
                    return false;
                }
            } else {
                return false;
            }
        }

        true
    }
}

impl Eq for PetriGraph {}

impl PartialOrd for PetriGraph {
    /// Tests inclusion between self and `other`: if self is a sub-graph of `other` (ie `∀ node ∈ self`, `node ⊂ other[node]`), then
    /// `Ordering::Less` is returned and vice-versa (with `Ordering::Greater`).
    /// If neither are included in each other, then returns `None` and if both are equal, returns `Ordering::Greater`
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let mut self_inner = true;
        let mut other_inner = true;
        match self.map.len().partial_cmp(&other.map.len()) {
            Some(Ordering::Less) => {
                other_inner = false; // `self` is smaller than `other`
            }
            Some(Ordering::Greater) => {
                self_inner = false; // `other` is smaller than `self`
            }
            _ => {}
        }

        if !self_inner && !other_inner {
            return None;
        }

        // Test for self ⊂ other, ie. ∀node∈self, node ⊂ other[node]
        for (node, self_states) in self.map.iter() {
            if let Some(other_states) = other.map.get(node) {
                if self_states.is_subset(other_states) {
                    continue;
                }
            }
            self_inner = false;
            break;
        }

        if !self_inner && !other_inner {
            return None;
        }

        // Test for other ⊂ self, ie. ∀node∈other, node ⊂ self[node]
        for (node, other_states) in other.map.iter() {
            if let Some(self_states) = self.map.get(node) {
                if other_states.is_subset(self_states) {
                    continue;
                }
            }
            other_inner = false;
            break;
        }

        match (self_inner, other_inner) {
            (true, true) => Some(Ordering::Equal),
            (true, false) => Some(Ordering::Less), // self ⊂ other -> self <= other
            (false, true) => Some(Ordering::Greater), // other ⊂ self -> self >= other
            (false, false) => None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn test_recurse(network: &PetriNetwork, expected: Vec<Vec<u8>>) {
        assert_eq!(network.recurse(0, network.nodes.clone(), network.nodes.clone()), expected);
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
    fn test_generate_graph() {
        let mut network = PetriNetwork {
            nodes: vec![1, 0],
            transitions: vec![
                PetriTransition::new(vec![0], vec![1]),
            ]
        };

        test_recurse(&network, vec![vec![0, 1]]);

        let graph = network.generate_graph();

        let mut expected = PetriGraph {
            map: HashMap::new(),
        };
        expected.map.insert(vec![1, 0], [vec![0, 1]].into());
        expected.map.insert(vec![0, 1], [vec![0, 1]].into()); // Identity

        assert_eq!(graph, expected);

        expected.map.insert(vec![0, 0], [vec![0, 0]].into());
        assert_eq!(graph.partial_cmp(&expected), Some(Ordering::Less));
        assert_eq!(expected.partial_cmp(&graph), Some(Ordering::Greater));

        network.nodes[0] = 0;

        assert_eq!(graph.union(network.generate_graph()), expected);
    }

    #[test]
    fn test_generate_graph_transitivity() {
        // Construct three graphs A, B, C such that A ⊂ B and B ⊂ C
        let mut network = PetriNetwork {
            nodes: vec![0, 0, 0],
            transitions: vec![
                PetriTransition::new(vec![0], vec![1]),
                PetriTransition::new(vec![1], vec![2]),
            ]
        };

        network.nodes[2] = 1;
        let a = network.generate_graph();

        network.nodes[2] = 0;
        network.nodes[1] = 1;
        let b = network.generate_graph();

        network.nodes[1] = 0;
        network.nodes[0] = 1;
        let c = network.generate_graph();

        // Check reflexivity as well
        assert!(a == a);
        assert!(b == b);
        assert!(c == c);

        assert!(a < b);
        assert!(b > a);

        assert!(b < c);
        assert!(c > b);

        assert!(a < c);
        assert!(c > a);
    }

    #[test]
    fn test_graph_from() {
        let network = PetriNetwork::new(
            vec![1, 0],
            vec![
                PetriTransition::new(vec![0], vec![1]),
            ]
        );

        assert_eq!(network.step(), vec![vec![0, 1]]);

        assert_eq!(network.generate_graph(), PetriGraph::from([
            (vec![1, 0], [vec![0, 1]]),
            (vec![0, 1], [vec![0, 1]]),
        ]));
    }
}
