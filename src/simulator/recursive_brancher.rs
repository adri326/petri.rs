use super::*;

/// A simulator that attempts to apply as many transitions as possible in each step,
/// by prioritizing transitions that may conflict with each other.
///
/// This simulator gives satisfying results in case of conflicts, without generating
/// huge graphs by only progressing one transition at once.
pub struct RecursiveBrancher<'a> {
    network: &'a PetriNetwork,
    node_order: Vec<usize>,
}

impl<'a> RecursiveBrancher<'a> {
    fn recursive_search(
        &self,
        mut remaining_values: Vec<u8>,
        mut current_state: Vec<u8>,
        mut brownout: Vec<bool>,
        skip: usize,
    ) -> Vec<Vec<u8>> {
        for (current_index, &node_index) in self.node_order.iter().enumerate().skip(skip) {
            if remaining_values[node_index] == 0 {
                continue;
            }

            let transitions = self
                .network
                .transitions()
                .iter()
                .enumerate()
                .filter(|(index, _)| !brownout[*index])
                .filter(|(_, transition)| transition.inputs.contains(&node_index))
                .filter(|(_, transition)| transition.is_active(&remaining_values))
                .collect::<Vec<_>>();

            match transitions.len() {
                0 => continue,
                1 => {
                    // Apply the only transition and keep looping
                    let (index, transition) = transitions[0];
                    brownout[index] = true;
                    transition.apply_inputs(&mut remaining_values);
                    transition.apply(&mut current_state);
                }
                _ => {
                    // Branch out and call recursive_search recursively,
                    // skipping the first few nodes until the current one
                    return transitions
                        .into_iter()
                        .flat_map(|(index, transition)| {
                            let mut brownout = brownout.clone();
                            brownout[index] = true;
                            let mut remaining_values = remaining_values.clone();
                            let mut current_state = current_state.clone();

                            transition.apply_inputs(&mut remaining_values);
                            transition.apply(&mut current_state);

                            self.recursive_search(
                                remaining_values,
                                current_state,
                                brownout,
                                current_index,
                            )
                        })
                        .collect();
                }
            }
        }

        vec![current_state]
    }
}

impl<'a> Simulator<'a> for RecursiveBrancher<'a> {
    fn init(network: &'a PetriNetwork) -> Self {
        RecursiveBrancher {
            network,
            node_order: get_node_order(network),
        }
    }

    fn get_next_states(&self, state: &[u8]) -> HashSet<Vec<u8>> {
        self.recursive_search(
            Vec::from(state),
            Vec::from(state),
            vec![false; self.network.transitions.len()],
            0,
        )
        .into_iter()
        .collect()
    }
}

fn get_node_order(network: &PetriNetwork) -> Vec<usize> {
    let mut order = (0..network.nodes.len()).collect::<Vec<_>>();
    let mut weights = vec![0usize; network.nodes.len()];
    for transition in network.transitions.iter() {
        for input in transition.inputs.iter() {
            weights[*input] += 1;
        }
    }

    order.sort_unstable_by_key(|&i| weights[i]);
    order.reverse();

    order
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_simple() {
        let network = PetriNetwork {
            nodes: vec![0, 1, 0],
            node_data: vec![Default::default(); 3],
            transitions: vec![
                PetriTransition::new(vec![0], vec![1], vec![]),
                PetriTransition::new(vec![1], vec![2], vec![]),
            ],
        };

        let brancher = RecursiveBrancher::init(&network);

        assert_eq!(
            brancher.get_next_states(&[0, 1, 0]),
            HashSet::from([vec![0, 0, 1]])
        );
    }

    #[test]
    fn test_two_calls() {
        let network = PetriNetwork {
            nodes: vec![1, 0, 0],
            node_data: vec![Default::default(); 3],
            transitions: vec![
                PetriTransition::new(vec![0], vec![1], vec![]),
                PetriTransition::new(vec![1], vec![2], vec![]),
            ],
        };

        let brancher = RecursiveBrancher::init(&network);

        assert_eq!(
            brancher.get_next_states(&[1, 0, 0]),
            HashSet::from([vec![0, 1, 0]])
        );

        assert_eq!(
            brancher.get_next_states(&[0, 1, 0]),
            HashSet::from([vec![0, 0, 1]])
        );
    }

    #[test]
    fn test_different_starting_set() {
        let network = PetriNetwork {
            nodes: vec![0, 0, 0],
            node_data: vec![Default::default(); 3],
            transitions: vec![
                PetriTransition::new(vec![0], vec![1], vec![]),
                PetriTransition::new(vec![1], vec![2], vec![]),
            ],
        };

        let brancher = RecursiveBrancher::init(&network);

        assert_eq!(
            brancher.get_next_states(&[1, 0, 0]),
            HashSet::from([vec![0, 1, 0]])
        );

        assert_eq!(
            brancher.get_next_states(&[0, 1, 0]),
            HashSet::from([vec![0, 0, 1]])
        );
    }
}
