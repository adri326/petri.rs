use std::collections::{HashMap, HashSet};
use std::cmp::Ordering;
use std::cell::RefCell;
use std::borrow::Cow;

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
    transpose: RefCell<Option<HashMap<Vec<u8>, HashSet<Vec<u8>>>>>,
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

    pub fn set_node(&mut self, index: usize, value: u8) {
        while self.nodes.len() <= index {
            self.nodes.push(0);
        }
        self.nodes[index] = value;
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
            map,
            transpose: RefCell::new(None),
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

/// A graph of states of a petri network. The following notation is used in this document:
/// - `node ∈ G`, iff `node` is a state reached by the petri network while generating the graph `G`
/// - `node_a -> node_b [G]`, iff `node_a` transitions to `node_b` within the network as the graph `G` was generated.
///    We will omit the `[G]` indication when there is only one G
/// - `node_a ⊂ node_b [G]`, iff `∀ next ∈ G`, `(node_a -> next [G]) => (node_b -> next [G])`
/// - `node ⊂ B[node] [A]` iff `node ∈ A`, `node ∈ B` and `∀ next ∈ A`, `(node -> next [A]) => (node -> next [B])`
/// - `G = A ∪ B` is the smallest graph such that `∀ node ∈ A, node ⊂ C[node]`, `∀ node ∈ B, node ⊂ C[node]` and `∀ node ∈ C, node ∈ A or node ∈ B`
/// - `G = A ∩ B` is the greatest graph such that `∀ node ∈ C, node ∈ A and node ∈ B` and `∀ node ∈ C, node ⊂ A[node] and node ⊂ B[node]`
/// - `node_a ->> node_b [G]` if `∃ k_n ∈ ℕ→G, N ∈ ℕ` such that `∀ n < N, k_n -> k_n+1 [G]`, `k_0 = node_a` and `k_N = node_b`.
///    In other words, there exists a path from `node_a` to `node_b`.
impl PetriGraph {
    pub fn get<'b>(&'b self, state: &Vec<u8>) -> Option<&'b HashSet<Vec<u8>>> {
        self.map.get(state)
    }

    /// Returns the union graph of `self` and `other`, where if `res = self ∪ other`,
    /// `∀ node ∈ self, node ⊂ res[node]` and `∀ node ∈ other, node ⊂ res[node]`
    ///
    /// We thus have the relations `self ⊂ res` and `other ⊂ res`.
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
            map,
            transpose: RefCell::new(None), // TODO: formally prove that `G = A ∪ B => G^T = A^T ∪ B^T`
        }
    }

    pub fn calculate_transpose(&self) {
        if self.transpose.borrow().is_none() {
            let mut res: HashMap<Vec<u8>, HashSet<Vec<u8>>> = HashMap::new();

            for (node, next_states) in self.map.iter() {
                for next_state in next_states.iter() {
                    match res.get_mut(next_state) {
                        Some(ref mut hashset) => {
                            hashset.insert(node.clone());
                        }
                        None => {
                            res.insert(next_state.clone(), [node.clone()].into());
                        }
                    }
                }
            }

            *self.transpose.borrow_mut() = Some(res);
        }
    }

    /// Returns the greatest set `S ⊂ self` such that `∀ node ∈ S`, `∃ state ∈ self` such that `condition(state)` and `node ->> state`
    pub fn reaches<'b, F>(&'b self, condition: F) -> HashSet<Cow<'b, Vec<u8>>>
    where
        F: for<'c> Fn(&'c Vec<u8>) -> bool
    {
        let mut hash_set = HashSet::with_capacity(self.map.len());
        let mut stack = Vec::new();
        let mut condition_always_true = true;

        for (node, _) in self.map.iter() {
            if (condition)(node) {
                hash_set.insert(Cow::Borrowed(node));
                stack.push(node);
            } else {
                condition_always_true = false;
            }
        }

        if condition_always_true {
            return hash_set;
        }

        self.calculate_transpose();

        if let Ok(ref transpose) = self.transpose.try_borrow() {
            let transpose = transpose.as_ref().expect("PetriGraph::calculate_transpose did not set PetriGraph::transpose to Some(...)");
            while let Some(current_node) = stack.pop() {
                for antecedent in transpose.get(current_node).iter().map(|x| x.iter()).flatten() {
                    if !hash_set.contains(antecedent) {
                        hash_set.insert(Cow::Owned(antecedent.clone()));
                        stack.push(antecedent);
                    }
                }
            }
        } else {
            panic!("Couldn't borrow() PetriGraph::tranpose");
        }

        hash_set
    }

    /// Returns true if `∀ node ∈ self`, `∃ state ∈ self` such that `condition(state)` and `node ->> state`
    pub fn always_reaches<'b, F>(&'b self, condition: F) -> bool
    where
        F: for<'c> Fn(&'c Vec<u8>) -> bool
    {
        let actual = self.reaches(condition);
        let expected = self.map.iter().map(|(k, _)| Cow::Borrowed(k)).collect::<HashSet<_>>();

        actual == expected
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
            transpose: RefCell::new(None),
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
            transpose: RefCell::new(None),
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

    #[test]
    fn test_graph_transpose() {
        let network = PetriNetwork::new(
            vec![1, 1, 0, 0, 0, 0],
            vec![
                PetriTransition::new(vec![0], vec![2]),
                PetriTransition::new(vec![1], vec![3]),
                PetriTransition::new(vec![3], vec![4]),
                PetriTransition::new(vec![2, 4], vec![5]),
            ]
        );

        let graph = network.generate_graph();

        let mut graph_transpose = network.generate_graph();
        graph_transpose.calculate_transpose();

        assert!(graph_transpose.transpose.borrow().is_some()); // G^T was filled in

        graph_transpose.map = graph_transpose.transpose.borrow().as_ref().unwrap().clone();
        *graph_transpose.transpose.borrow_mut() = None;

        graph_transpose.calculate_transpose();

        let graph_double_transpose = PetriGraph {
            map: graph_transpose.transpose.borrow().as_ref().unwrap().clone(),
            transpose: RefCell::new(None),
        };

        assert_eq!(graph, graph_double_transpose); // G^T^T = G
    }

    #[test]
    fn test_graph_always_reaches() {
        let mut network = PetriNetwork::new(
            vec![1, 1, 0, 0, 0, 0],
            vec![
                PetriTransition::new(vec![0], vec![2]),
                PetriTransition::new(vec![1], vec![3]),
                PetriTransition::new(vec![3], vec![4]),
                PetriTransition::new(vec![2, 4], vec![5]),
            ]
        );

        let graph = network.generate_graph();

        assert!(graph.always_reaches(|state| state[5] == 1));

        for a in 0..=1 {
            for b in 0..=1 {
                network.set_node(0, a);
                network.set_node(1, b);
                let graph = network.generate_graph();
                let reaches_c = graph.always_reaches(|state| state[5] == 1); // reaches_c = [A, B, 0, 0, 0, 0] ->> [0, 0, 0, 0, 0, 1]
                assert_eq!(reaches_c, a == 1 && b == 1); // reaches_c <=> (A, B) = (1, 1)
            }
        }
    }
}
