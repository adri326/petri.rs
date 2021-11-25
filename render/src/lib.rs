#![feature(linked_list_cursors)]

use petri::{
    PetriNetwork,
    PetriTransition,
};
use std::collections::{VecDeque, HashSet, LinkedList};

#[derive(Clone, Debug)]
pub struct PetriRenderer<'a> {
    pub network: &'a PetriNetwork,
    pub nodes_y: Vec<Option<i32>>,
}

impl<'a> From<&'a PetriNetwork> for PetriRenderer<'a> {
    fn from(network: &'a PetriNetwork) -> PetriRenderer<'a> {
        Self {
            nodes_y: vec![None; network.nodes.len()],
            network,
        }
    }
}

impl<'a> PetriRenderer<'a> {
    pub fn calculate_nodes_y(&mut self, seed: usize) {
        let mut encountered = HashSet::new();
        encountered.insert(seed);
        self.nodes_y[seed] = Some(0);

        let mut iteration = 0;
        let mut transitions = LinkedList::from_iter(self.network.transitions.iter());

        // Simple implementation that works on small to medium-sized networks
        // TODO: more efficient version for bigger networks
        while self.nodes_y.iter().any(|x| x.is_none()) {
            iteration += 1;
            if iteration > self.network.nodes.len() {
                break;
            }

            let mut cursor = transitions.cursor_front_mut();
            while cursor.current().is_some() {
                let transition = cursor.current();
                if transition.is_none() {
                    break;
                }
                let transition = transition.unwrap();

                let mut input_max: Option<i32> = None;
                let mut input_any = false;
                for &input in transition.inputs.iter() {
                    if let Some(input) = self.nodes_y[input] {
                        input_max = Some(input_max.map(|x| x.max(input)).unwrap_or(input));
                    } else {
                        input_any = true;
                    }
                }

                let mut output_min: Option<i32> = None;
                let mut output_any = false;
                for &output in transition.outputs.iter() {
                    if let Some(output) = self.nodes_y[output] {
                        output_min = Some(output_min.map(|x| x.min(output)).unwrap_or(output));
                    } else {
                        output_any = true;
                    }
                }

                if !input_any && !output_any {
                    cursor.remove_current();
                    cursor.move_next();
                    continue;
                }

                if let Some(output_min) = output_min {
                    for &input in transition.inputs.iter() {
                        if !encountered.contains(&input) {
                            self.nodes_y[input] = Some(output_min - 1);
                            println!("{}", input);
                            encountered.insert(input);
                        }
                    }
                }

                if let Some(input_max) = input_max {
                    for &output in transition.outputs.iter() {
                        if !encountered.contains(&output) {
                            self.nodes_y[output] = Some(input_max + 1);
                            println!("{}", output);
                            encountered.insert(output);
                        }
                    }
                }

                cursor.move_next();
            }
        }
    }

    pub fn optimize_nodes_y(&mut self) {
        let mut nodes_y = vec![None; self.network.nodes.len()];
        std::mem::swap(&mut self.nodes_y, &mut nodes_y);

        for (index, node) in self.nodes_y.iter_mut().enumerate() {
            let mut inputs = Vec::new();
            let mut outputs = Vec::new();

            // Accumulate the y-values of the nodes that are connected to the current node
            for transition in self.network.transitions.iter() {
                if !transition.involves(index) {
                    continue;
                }

                let mut is_input = transition.inputs.iter().any(|x| *x == index);
                let mut is_output = transition.outputs.iter().any(|x| *x == index);

                if is_output {
                    for &input in transition.inputs.iter() {
                        if input != index {
                            inputs.push(nodes_y[input].unwrap_or(0));
                        } else {
                            is_input = true;
                        }
                    }
                }

                if is_input {
                    for &output in transition.outputs.iter() {
                        if output != index {
                            outputs.push(nodes_y[output].unwrap_or(0));
                        } else {
                            is_output = true;
                        }
                    }
                }
            }

            if inputs.len() + outputs.len() == 0 {
                // Not enough information to optimize
                *node = nodes_y[index];
                continue;
            } else {
                if inputs.len() > 0 && outputs.len() > 0 {
                    // Method 1: *node = avg(max(input), min(output))
                    let input_max = inputs.iter().reduce(|acc, act| acc.max(act)).unwrap();
                    let output_min = outputs.iter().reduce(|acc, act| acc.min(act)).unwrap();
                    if input_max + 1 >= output_min - 1 {
                        // This if prevents the graph from flattening itself out when we don't want it to
                        if inputs.len() >= outputs.len() {
                            *node = Some(input_max + 1);
                        } else {
                            *node = Some(output_min - 1);
                        }
                    } else {
                        *node = Some((input_max + output_min) / 2);
                    }
                } else if inputs.len() > 0 {
                    let input_max = inputs.iter().reduce(|acc, act| acc.max(act)).unwrap();
                    *node = Some(input_max + 1);
                } else {
                    let output_min = outputs.iter().reduce(|acc, act| acc.min(act)).unwrap();
                    *node = Some(output_min - 1);
                }
                println!("{:?}, {:?}, {:?}, {:?}", inputs, outputs, nodes_y[index], node);
            }
        }

        let mut y_min = 0;
        let mut y_max = 0;
        for y in self.nodes_y.iter() {
            if let Some(y) = y {
                y_min = y_min.min(*y);
                y_max = y_max.max(*y);
            }
        }

        println!("Positions: ");
        println!("===");
        for y in y_min..=y_max {
            print!("{} - ", y);
            for (index, node) in self.nodes_y.iter().enumerate() {
                if *node == Some(y) {
                    print!("{}  ", index);
                }
            }
            println!("");
        }
        println!("===");
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use crate::*;

    #[test]
    fn test_nodes_y() {
        let mut builder = PetriBuilder::new();
        let start = builder.node_with_label(1, "start");
        builder.begin_branch(start)
            .step()
            .step_with_branch(|branch| {
                branch
                    .step()
                    .join(["start"])
                    .label("p1");
            })
            .step()
            .step()
            .step()
            .step()
            .join(["p1"])
            .label("end");
        let end = builder.get_label("end").unwrap().as_node().unwrap();
        let p1 = builder.get_label("p1").unwrap().as_node().unwrap();
        builder.transition(vec![end], vec![start]);
        builder.transition(vec![p1], vec![start]);

        let network = builder.build();

        let mut renderer = PetriRenderer::from(&network);
        renderer.calculate_nodes_y(start);
        for y in renderer.nodes_y.iter() {
            assert!(y.is_some());
        }

        for n in 1..=10 {
            println!("=== Step {} ===", n);
            renderer.optimize_nodes_y();
            for y in renderer.nodes_y.iter() {
                assert!(y.is_some());
            }
        }

        panic!();
    }
}
