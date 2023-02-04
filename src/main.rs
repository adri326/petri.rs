use std::path::Path;

use petri_network::{parser::parse, PetriGraph, PetriNetwork};

fn main() {
    let input = std::fs::read_to_string("examples/simple.petri").unwrap();
    let network = parse(&input).unwrap();

    export(&network, "target/simple.dot");

    let graph = network.generate_graph();

    export_graph(&graph, "target/graph.dot");
}

fn export(network: &PetriNetwork, path: impl AsRef<Path>) {
    let mut output = std::fs::File::create(path).unwrap();
    network.export_dot(&mut output);
}

fn export_graph(graph: &PetriGraph, path: impl AsRef<Path>) {
    let mut output = std::fs::File::create(path).unwrap();
    graph.export_dot(&mut output);
}
