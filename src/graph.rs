use std::collections::{HashMap, HashSet};
use std::cmp::Ordering;
use std::cell::RefCell;
use std::borrow::Cow;

#[cfg(feature = "export_dot")]
use super::network::export_dot;
// use crate::{
//     PetriTransition,
//     PetriNetwork,
// };

#[derive(Clone)]
pub struct PetriGraph {
    map: HashMap<Vec<u8>, HashSet<Vec<u8>>>,
    transpose: RefCell<Option<HashMap<Vec<u8>, HashSet<Vec<u8>>>>>,
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
    pub fn new(map: HashMap<Vec<u8>, HashSet<Vec<u8>>>) -> Self {
        Self {
            map,
            transpose: RefCell::new(None),
        }
    }

    pub fn get<'b>(&'b self, state: &Vec<u8>) -> Option<&'b HashSet<Vec<u8>>> {
        self.map.get(state)
    }

    #[inline]
    pub fn with_transpose<F, T>(&self, state: &Vec<u8>, fn_with: F) -> Option<T>
    where
        F: for<'c> FnOnce(&'c HashSet<Vec<u8>>) -> T
    {
        self.calculate_transpose();

        if let Some(ref inverse) = self.transpose.borrow().as_ref().unwrap().get(state) {
            Some((fn_with)(inverse))
        } else {
            None
        }
    }

    pub fn iter<'b>(&'b self) -> std::collections::hash_map::Iter<'b, Vec<u8>, HashSet<Vec<u8>>> {
        self.map.iter()
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

    /// Does the same as `assert!(graph.always_reaches(condition))`, but prints a more helpful error if the assertion fails.
    pub fn assert_always_reaches<'b, F>(&'b self, condition: F)
    where
        F: for<'c> Fn(&'c Vec<u8>) -> bool + Copy
    {
        let actual = self.reaches(condition);
        let expected = self.map.iter().map(|(k, _)| Cow::Borrowed(k)).collect::<HashSet<_>>();

        if actual != expected {
            let expected = expected.iter().map(|cow| cow.as_ref()).collect::<HashSet<_>>();
            let actual = actual.iter().map(|cow| cow.as_ref()).collect::<HashSet<_>>();
            let base = self.map.iter().map(|(k, _)| k).filter(|k| (condition)(k)).collect::<HashSet<_>>();

            eprintln!("Info: Expected set of nodes:");
            for node in expected.iter() {
                eprintln!(" - {:?}", node);
            }
            eprintln!("Info: Actual set of nodes:");
            for node in actual.iter() {
                eprintln!(" - {:?}", node);
            }
            eprintln!("Info: Set of nodes for which the condition is true:");
            for node in base.iter() {
                eprintln!(" - {:?}", node);
            }
            eprintln!("Info: Set difference (- is missing, * is reaching, ^ is base):");
            for node in expected.iter() {
                if actual.contains(node) {
                    if base.contains(node) {
                        eprintln!(" ^ {:?}", node);
                    } else {
                        eprintln!(" * {:?}", node);
                    }
                } else {
                    eprintln!(" - {:?}", node);
                }
            }
            panic!("Assertion error: expected all nodes of graph to reach the given node(s).");
        }
    }

    pub fn forall<'b, F>(&'b self, condition: F) -> bool
    where
        F: for<'c> Fn(&'c Vec<u8>) -> bool + Copy
    {
        for (node, _) in self.map.iter() {
            if !(condition)(node) {
                return false;
            }
        }

        true
    }

    pub fn assert_forall<'b, F>(&'b self, condition: F)
    where
        F: for<'c> Fn(&'c Vec<u8>) -> bool + Copy
    {
        let mut errors = HashSet::new();

        for (node, _) in self.map.iter() {
            if !(condition)(node) {
                errors.insert(node);
            }
        }

        if errors.len() > 0 {
            eprintln!("Info: Nodes analyzed: (- yielded false, + yielded true)");
            for (node, _) in self.map.iter() {
                if errors.contains(node) {
                    eprintln!(" - {:?}", node);
                } else {
                    eprintln!(" + {:?}", node);
                }
            }
            panic!("Assertion error: not every node fulfills the condition");
        }
    }

    #[cfg(feature = "export_dot")]
    pub fn export_dot<W: std::io::Write>(&self, writer: &mut W) {
        let mut writer = dot_writer::DotWriter::from(writer);

        export_dot::export_graph(self, &mut writer);
    }

    #[cfg(feature = "export_dot")]
    pub fn get_dot_string(&self) -> String {
        let mut vec = Vec::new();
        let mut writer = dot_writer::DotWriter::from(&mut vec);

        export_dot::export_graph(self, &mut writer);

        String::from_utf8_lossy(&vec).into_owned()
    }
}

impl<T: IntoIterator<Item=Vec<u8>>, const N: usize> From<[(Vec<u8>, T); N]> for PetriGraph {
    fn from(iter: [(Vec<u8>, T); N]) -> Self {
        let iter = iter.into_iter().map(|(k, n)| (k, n.into_iter().collect()));
        Self {
            map: iter.collect(),
            transpose: RefCell::new(None),
        }
    }
}

impl<T: IntoIterator<Item=Vec<u8>>> FromIterator<(Vec<u8>, T)> for PetriGraph {
    fn from_iter<U: IntoIterator<Item=(Vec<u8>, T)>>(iter: U) -> Self {
        let iter = iter.into_iter().map(|(k, n)| (k, n.into_iter().collect()));
        Self {
            map: iter.collect(),
            transpose: RefCell::new(None),
        }
    }
}

impl std::ops::Index<&Vec<u8>> for PetriGraph {
    type Output = HashSet<Vec<u8>>;

    fn index<'b>(&'b self, key: &Vec<u8>) -> &'b HashSet<Vec<u8>> {
        &self.map[key]
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

impl IntoIterator for PetriGraph {
    type IntoIter = std::collections::hash_map::IntoIter<Vec<u8>, HashSet<Vec<u8>>>;
    type Item = (Vec<u8>, HashSet<Vec<u8>>);

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}

impl<'a> IntoIterator for &'a PetriGraph {
    type IntoIter = std::collections::hash_map::Iter<'a, Vec<u8>, HashSet<Vec<u8>>>;
    type Item = (&'a Vec<u8>, &'a HashSet<Vec<u8>>);

    fn into_iter(self) -> Self::IntoIter {
        self.map.iter()
    }
}

impl std::fmt::Debug for PetriGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "PetriGraph {{")?;
        for (node, next_states) in self.map.iter() {
            write!(f, "  {:?} => ", node)?;
            if next_states.len() == 0 {
                writeln!(f, "{{}},")?;
            } else if next_states.len() == 1 {
                writeln!(f, "{{{:?}}},", next_states.iter().next().unwrap())?;
            } else {
                writeln!(f, "{{")?;
                for next_state in next_states {
                    writeln!(f, "    {:?},", next_state)?;
                }
                writeln!(f, "  }},")?;
            }
        }
        writeln!(f, "}}")
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        PetriTransition,
        PetriNetwork,
        network::test::test_recurse,
    };

    #[test]
    fn test_generate_graph() {
        let mut network = PetriNetwork {
            nodes: vec![1, 0],
            node_data: vec![Default::default(); 2],
            transitions: vec![
                PetriTransition::new(vec![0], vec![1], vec![]),
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
            node_data: vec![Default::default(); 3],
            transitions: vec![
                PetriTransition::new(vec![0], vec![1], vec![]),
                PetriTransition::new(vec![1], vec![2], vec![]),
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
            vec![Default::default(); 2],
            vec![
                PetriTransition::new(vec![0], vec![1], vec![]),
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
            vec![Default::default(); 6],
            vec![
                PetriTransition::new(vec![0], vec![2], vec![]),
                PetriTransition::new(vec![1], vec![3], vec![]),
                PetriTransition::new(vec![3], vec![4], vec![]),
                PetriTransition::new(vec![2, 4], vec![5], vec![]),
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
            vec![Default::default(); 6],
            vec![
                PetriTransition::new(vec![0], vec![2], vec![]),
                PetriTransition::new(vec![1], vec![3], vec![]),
                PetriTransition::new(vec![3], vec![4], vec![]),
                PetriTransition::new(vec![2, 4], vec![5], vec![]),
            ]
        );

        let graph = network.generate_graph();

        graph.assert_always_reaches(|state| state[5] == 1);

        for a in 0..=1 {
            for b in 0..=1 {
                network.set_node(0, a, Default::default());
                network.set_node(1, b, Default::default());
                let graph = network.generate_graph();
                let reaches_c = graph.always_reaches(|state| state[5] == 1); // reaches_c = [A, B, 0, 0, 0, 0] ->> [0, 0, 0, 0, 0, 1]
                assert_eq!(reaches_c, a == 1 && b == 1); // reaches_c <=> (A, B) = (1, 1)
            }
        }
    }

    #[test]
    fn test_step_fork() {
        let mut network = PetriNetwork::new(
            vec![0; 4],
            vec![Default::default(); 4],
            vec![
                PetriTransition::new(vec![0], vec![1], vec![]),
                PetriTransition::new(vec![1], vec![2], vec![]),
                PetriTransition::new(vec![1], vec![3], vec![]),
            ]
        );

        test_recurse(&network, vec![vec![0; 4]]);

        network.set_node(0, 1, Default::default());

        test_recurse(&network, vec![vec![0, 1, 0, 0]]);

        network.set_node(0, 0, Default::default());
        network.set_node(1, 1, Default::default());

        test_recurse(&network, vec![vec![0, 0, 1, 0], vec![0, 0, 0, 1]]);

        network.set_node(1, 0, Default::default());
        network.set_node(0, 1, Default::default());
        let graph = network.generate_graph();

        assert_eq!(graph, PetriGraph::from([
            (vec![1, 0, 0, 0], vec![vec![0, 1, 0, 0]]),
            (vec![0, 1, 0, 0], vec![vec![0, 0, 1, 0], vec![0, 0, 0, 1]]),
            (vec![0, 0, 1, 0], vec![vec![0, 0, 1, 0]]),
            (vec![0, 0, 0, 1], vec![vec![0, 0, 0, 1]]),
        ]));
    }
}
