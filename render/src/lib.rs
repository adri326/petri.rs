#![feature(linked_list_cursors)]

const EPSILON: f32 = 0.00001;

use petri_network::{PetriNetwork, PetriTransition};
use rand::prelude::*;
use std::collections::{HashSet, LinkedList};

#[derive(Clone, Debug)]
pub struct PetriRenderer<'a> {
    pub network: &'a PetriNetwork,
    pub nodes_y: Vec<Option<i32>>,
    pub nodes_x: Vec<f32>,
}

impl<'a> From<&'a PetriNetwork> for PetriRenderer<'a> {
    fn from(network: &'a PetriNetwork) -> PetriRenderer<'a> {
        Self {
            nodes_y: vec![None; network.nodes().len()],
            nodes_x: (0..network.nodes().len()).map(|x| x as f32).collect(),
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
        let mut transitions = LinkedList::from_iter(self.network.transitions().iter());

        // Simple implementation that works on small to medium-sized networks
        // TODO: more efficient version for bigger networks
        while self.nodes_y.iter().any(|x| x.is_none()) {
            iteration += 1;
            if iteration > self.network.nodes().len() {
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
                            encountered.insert(input);
                        }
                    }
                }

                if let Some(input_max) = input_max {
                    for &output in transition.outputs.iter() {
                        if !encountered.contains(&output) {
                            self.nodes_y[output] = Some(input_max + 1);
                            encountered.insert(output);
                        }
                    }
                }

                cursor.move_next();
            }
        }
    }

    pub fn optimize_nodes_y(&mut self) {
        let mut nodes_y = vec![None; self.network.nodes().len()];
        std::mem::swap(&mut self.nodes_y, &mut nodes_y);

        for (index, node) in self.nodes_y.iter_mut().enumerate() {
            let mut inputs = Vec::new();
            let mut outputs = Vec::new();

            // Accumulate the y-values of the nodes that are connected to the current node
            for transition in self.network.transitions().iter() {
                if !transition.involves(index) {
                    continue;
                }

                let is_input = transition.inputs.iter().any(|x| *x == index);
                let is_output = transition.outputs.iter().any(|x| *x == index);

                if is_output {
                    for &input in transition.inputs.iter() {
                        if input != index {
                            inputs.push(nodes_y[input].unwrap_or(0));
                        }
                    }
                }

                if is_input {
                    for &output in transition.outputs.iter() {
                        if output != index {
                            outputs.push(nodes_y[output].unwrap_or(0));
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
            }
        }

        // Remove gaps
        let mut y_last = i32::MIN;
        let mut y_current = -1;
        let mut nodes_y = self.nodes_y.iter_mut().collect::<Vec<_>>();
        nodes_y.sort_by_key(|x| (**x).unwrap_or(0));

        for y in nodes_y.into_iter() {
            if let Some(ref mut y) = y {
                if *y > y_last {
                    y_current += 1;
                }
                y_last = *y;
                *y = y_current;
            }
        }
    }

    pub fn randomize_nodes_x(&mut self) {
        let mut rng = rand::thread_rng();
        self.nodes_x = (0..self.nodes_x.len()).map(|x| x as f32).collect();
        self.nodes_x.shuffle(&mut rng);
    }

    pub fn optimize_nodes_x(&mut self, attempts: usize, rounds: usize, step_size: f32) {
        if attempts > 0 {
            self.randomize_nodes_x();
        }
        for _ in 0..rounds {
            self.optimize_nodes_x_step(step_size);
        }
        if attempts > 1 {
            let mut best_attempt = (self.nodes_x.clone(), self.cost_x_all());

            for _attempt in 1..attempts {
                self.randomize_nodes_x();
                for _round in 0..rounds {
                    self.optimize_nodes_x_step(step_size);
                }
                let cost = self.cost_x_all();
                if cost < best_attempt.1 {
                    std::mem::swap(&mut best_attempt.0, &mut self.nodes_x);
                }
            }

            std::mem::swap(&mut best_attempt.0, &mut self.nodes_x);
        }
    }

    fn optimize_nodes_x_step(&mut self, step_size: f32) {
        let mut nodes_x = self.nodes_x.clone();
        for (index, current_x) in nodes_x.iter_mut().enumerate() {
            // ∂/∂x cost_x(index) ~= (cost_x(index, x+ε) - cost_x(index, x-ε)) / 2ε
            let dx = (self.cost_x(index, *current_x + EPSILON)
                - self.cost_x(index, *current_x - EPSILON))
                / (2.0 * EPSILON);

            // Gradient descent to find the minimum
            *current_x -= step_size * sigma(dx);
        }

        std::mem::swap(&mut nodes_x, &mut self.nodes_x);
    }

    fn cost_x(&self, index: usize, x: f32) -> f32 {
        let mut sum = 0.0;

        // Sum the distance to the current node within the transitions (closer transitions have a higher weight)
        for transition in self.network.transitions().iter() {
            if transition.involves(index) {
                for &input in transition.inputs.iter() {
                    let delta_y = self.nodes_y[input]
                        .map(|input_y| self.nodes_y[index].map(|y| input_y - y))
                        .flatten()
                        .unwrap_or(0);
                    let mult = if delta_y.abs() > 1 { 0.25 } else { 1.0 };
                    sum += mult * (self.nodes_x[input] - x) * (self.nodes_x[input] - x);
                }

                for &output in transition.outputs.iter() {
                    let delta_y = self.nodes_y[output]
                        .map(|output_y| self.nodes_y[index].map(|y| output_y - y))
                        .flatten()
                        .unwrap_or(0);
                    let mult = if delta_y.abs() > 1 { 0.25 } else { 1.0 };
                    sum += mult * (self.nodes_x[output] - x) * (self.nodes_x[output] - x);
                }
            }
        }

        let mut sum = sum.sqrt();

        // For every node that is on the same row as ourselves, sum a lennard-jones-like function
        for (i, node) in self.nodes_x.iter().enumerate() {
            if i == index || self.nodes_y[i] != self.nodes_y[index] {
                continue;
            }

            let d = 1.2;
            let r = (node - x).abs();
            let dr6 = (d / (r + EPSILON)).powi(6);
            let mult = 16.0;
            sum += mult * (dr6 * dr6 - dr6);
        }

        sum
    }

    fn cost_x_all(&self) -> f32 {
        let mut sum = 0.0;

        for (i, x) in self.nodes_x.iter().enumerate() {
            let cost = self.cost_x(i, *x);
            sum += cost * cost;
        }

        sum
    }

    pub fn discretize_x(&mut self, radix: f32) {
        for x in self.nodes_x.iter_mut() {
            *x = (*x / radix).round() * radix;
        }
    }
}

fn sigma(x: f32) -> f32 {
    2.0 / (1.0 + (-x).exp()) - 1.0
}

#[cfg(test)]
mod test {
    use super::*;
    use petri_network::*;

    #[test]
    fn test_nodes_y() {
        let mut builder = PetriBuilder::new();
        let start = builder.node_with_label(1, "start");
        builder
            .begin_branch(start)
            .step()
            .step_with_branch(|branch| {
                branch.step().join(["start"]).label("p1");
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

        for _n in 1..=3 {
            renderer.optimize_nodes_y();
            for y in renderer.nodes_y.iter() {
                assert!(y.is_some());
            }
        }

        renderer.randomize_nodes_x();
        renderer.optimize_nodes_x(1, 1, 0.01);
        for x in renderer.nodes_x.iter() {
            assert!(!x.is_nan() && !x.is_infinite());
        }
        renderer.optimize_nodes_x(100, 100, 0.01);
        for x in renderer.nodes_x.iter() {
            assert!(!x.is_nan() && !x.is_infinite());
        }
        renderer.optimize_nodes_x(0, 1000, 0.01);
        renderer.discretize_x(0.25);
        for (_i, x) in renderer.nodes_x.iter().enumerate() {
            assert!(!x.is_nan() && !x.is_infinite());
            println!("{}: ({}, {})", _i, x, renderer.nodes_y[_i].unwrap());
        }
    }
}
