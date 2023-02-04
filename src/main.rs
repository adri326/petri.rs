use std::{collections::HashMap};

use petri_network::{parser::parse, ExhaustiveBrancher, PetriGraph, PetriNetwork};

fn main() {
    let input = std::fs::read_to_string("examples/simple.petri").unwrap();
    let network = parse(&input).unwrap();

    let mut output = std::fs::File::create("target/simple.dot").unwrap();
    network.export_dot(&mut output);

    let graph = network.generate_graph::<ExhaustiveBrancher>();

    // let names = network.node_data().iter().enumerate().filter_map(|(index, data)| Some((data.label.clone()?, index))).collect::<HashMap<_, _>>();

    // let deadlocks = graph.final_nodes();

    let mut output = std::fs::File::create("target/graph.dot").unwrap();
    graph.export_dot(&mut output, |state, node| {

    }, |_, _, _| {});
}
