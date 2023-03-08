use std::{cmp::Ordering, collections::HashMap};

#[derive(Debug, Clone)]
pub struct EdgeMap<T>(HashMap<Vec<u8>, HashMap<Vec<u8>, T>>);

impl<T> Default for EdgeMap<T> {
    fn default() -> Self {
        Self(HashMap::new())
    }
}

impl<T> EdgeMap<T> {
    pub fn new(map: HashMap<Vec<u8>, HashMap<Vec<u8>, T>>) -> Self {
        Self(map)
    }

    pub fn add(&mut self, from: &[u8], to: Vec<u8>, attribute: T) {
        if let Some(ref mut edges) = self.0.get_mut(from) {
            edges.insert(to, attribute);
        } else {
            self.0.insert(
                from.iter().copied().collect(),
                HashMap::from([(to, attribute)]),
            );
        }
    }

    pub fn remove(&mut self, from: &[u8], to: &[u8]) -> Option<(Vec<u8>, T)> {
        if let Some(ref mut edges) = self.0.get_mut(from) {
            edges.remove_entry(to)
        } else {
            None
        }
    }

    pub fn iter_nodes(&self) -> impl Iterator<Item = &Vec<u8>> {
        self.0.keys()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn has(&self, source: &[u8], target: &[u8]) -> bool {
        if let Some(edges) = self.0.get(source) {
            edges.contains_key(target)
        } else {
            false
        }
    }

    pub fn get_edge_attribute<'b>(&'b self, source: &[u8], target: &[u8]) -> Option<&'b T> {
        if let Some(edges) = self.0.get(source) {
            edges.get(target)
        } else {
            None
        }
    }

    pub fn get_edges_of<'b>(&'b self, source: &[u8]) -> Option<&'b HashMap<Vec<u8>, T>> {
        self.0.get(source)
    }

    pub fn iter_edges_of(&self, state: &[u8]) -> impl Iterator<Item = &Vec<u8>> {
        self.0.get(state).into_iter().flat_map(|edges| edges.keys())
    }

    pub fn count_of(&self, state: &[u8]) -> usize {
        self.0.get(state).map(|edges| edges.len()).unwrap_or(0)
    }

    pub fn iter_of(&self, state: &[u8]) -> impl Iterator<Item = (&Vec<u8>, &T)> {
        self.0.get(state).into_iter().flat_map(|edges| edges.iter())
    }

    pub fn shrink_to_fit(&mut self) {
        // Shrink result hashmap and hashsets
        for (_, edges) in self.0.iter_mut() {
            edges.shrink_to_fit();
        }

        self.0.shrink_to_fit();
    }

    pub fn reverse(&self) -> EdgeMap<T>
    where
        T: Clone,
    {
        let mut res: HashMap<Vec<u8>, HashMap<Vec<u8>, T>> = HashMap::new();

        for (node, edges) in self.0.iter() {
            for (edge, attribute) in edges.iter() {
                match res.get_mut(edge) {
                    Some(ref mut existing) => {
                        existing.insert(node.clone(), attribute.clone());
                    }
                    None => {
                        res.insert(edge.clone(), [(node.clone(), attribute.clone())].into());
                    }
                }
            }
        }

        Self::new(res)
    }

    pub fn union(&self, other: &Self) -> Self
    where
        T: Clone,
    {
        let mut res = self.clone();

        for node in other.iter_nodes() {
            for (edge, attribute) in other.iter_of(node) {
                res.add(node, edge.clone(), attribute.clone());
            }
        }

        res
    }

    pub fn is_subgraph(&self, other: &Self) -> bool {
        if self.0.len() > other.0.len() {
            return false;
        }

        for (node, edges) in self.0.iter() {
            if let Some(other_edges) = other.0.get(node) {
                for edge in edges.keys() {
                    if !other_edges.contains_key(edge) {
                        return false;
                    }
                }
            } else {
                return false;
            }
        }

        true
    }
}

impl<T> PartialEq for EdgeMap<T> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }

        self.is_subgraph(other) && other.is_subgraph(&self)
    }
}

impl<T> PartialOrd<EdgeMap<T>> for EdgeMap<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let self_subgraph = self.is_subgraph(other);
        let other_subgraph = other.is_subgraph(&self);

        match (self_subgraph, other_subgraph) {
            (true, true) => Some(Ordering::Equal),
            (true, false) => Some(Ordering::Less),
            (false, true) => Some(Ordering::Greater),
            (false, false) => None,
        }
    }
}
