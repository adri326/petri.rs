use std::{collections::{HashMap, HashSet}};

use dot_writer::Attributes;
use petri_network::{parser::parse, ExhaustiveBrancher};

fn main() {
    let input = std::fs::read_to_string("examples/simple.petri").unwrap();
    let network = parse(&input).unwrap();

    let mut output = std::fs::File::create("target/simple.dot").unwrap();
    network.export_dot(&mut output);

    let graph = network.generate_graph::<ExhaustiveBrancher>();

    let names = network.node_data().iter().enumerate().filter_map(|(index, data)| Some((data.label.clone()?, index))).collect::<HashMap<_, _>>();

    let pool_size = network.nodes()[*names.get("Pool").unwrap()];
    let pool_end = *names.get("Pool_end").unwrap();
    let deadlocks: HashSet<Vec<u8>> = graph.final_nodes().filter(|&node| {
        node[pool_end] != pool_size
    }).cloned().collect();

    let success: HashSet<Vec<u8>> = graph.final_nodes().filter(|&node| {
        node[pool_end] == pool_size
    }).cloned().collect();

    let mut output = std::fs::File::create("target/graph.dot").unwrap();
    graph.export_dot(&mut output, |state, node| {
        node.set("shape", "point", true);
        node.set("width", "0.2", false);
        node.set("height", "0.2", false);
        let subgraph = graph.subgraph(state);
        if deadlocks.contains(state) {
            node.set("fillcolor", "red", true);
            node.set("color", "red", true);
        } else if success.contains(state) {
            node.set("fillcolor", "green", true);
            node.set("color", "green", true);
        } else if subgraph.always_reaches(|node| deadlocks.contains(node)) {
            node.set("fillcolor", "#f0b0b0", true);
            node.set("color", "#f0b0b0", true);
        } else if subgraph.always_reaches(|node| success.contains(node)) {
            node.set("fillcolor", "#b0f0b0", true);
            node.set("color", "#b0f0b0", true);
        } else {
            node.set("fillcolor", "#808080", true);
            node.set("color", "#808080", true);
        }
    }, |from, to, edge| {
        if from == to {
            edge.set("style", "invis", true);
        }
    });
}
