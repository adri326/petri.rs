use std::collections::{HashMap, HashSet};

use dot_writer::Attributes;
use petri_network::{parser::parse, ExhaustiveBrancher, MonteCarloSettings};

fn main() {
    let input = std::fs::read_to_string("examples/simple.petri").unwrap();
    let network = parse(&input).unwrap();

    let mut output = std::fs::File::create("target/simple.dot").unwrap();
    network.export_dot(&mut output);

    let graph = network.generate_graph::<ExhaustiveBrancher>();

    let names = network
        .node_data()
        .iter()
        .enumerate()
        .filter_map(|(index, data)| Some((data.label.clone()?, index)))
        .collect::<HashMap<_, _>>();

    let pool_size = network.nodes()[*names.get("Pool").unwrap()];
    let pool_end = *names.get("Pool_end").unwrap();
    let deadlocks: HashSet<Vec<u8>> = graph
        .final_nodes()
        .filter(|&node| node[pool_end] != pool_size)
        .cloned()
        .collect();

    let success: HashSet<Vec<u8>> = graph
        .final_nodes()
        .filter(|&node| node[pool_end] == pool_size)
        .cloned()
        .collect();

    let standing_distribution = graph
        .wrap_final_nodes(network.nodes(), 1.0)
        .get_standing_distribution(network.nodes(), Default::default());

    let maximum_distribution = standing_distribution.values().copied().reduce(f64::max).unwrap_or(1.0);

    let mut settings: MonteCarloSettings = Default::default();
    settings.iterations = 10000;
    settings.warmup_steps = 100;
    settings.steps = 101;

    let final_distribution = graph
        .get_standing_distribution(network.nodes(), settings);

    let mut output = std::fs::File::create("target/graph.dot").unwrap();
    graph.export_dot(
        &mut output,
        |state, dot_node| {
            let weight = *standing_distribution.get(state).unwrap() / maximum_distribution * 0.5;
            dot_node.set("shape", "point", true);
            dot_node.set("width", &format!("{}", weight), false);
            dot_node.set("height", &format!("{}", weight), false);
            let subgraph = graph.subgraph(state);
            if deadlocks.contains(state) {
                dot_node.set("fillcolor", "red", true);
                dot_node.set("color", "red", true);
                let weight = *final_distribution.get(state).unwrap();
                dot_node.set("xlabel", &format!("{:.3}", weight), true);
            } else if success.contains(state) {
                dot_node.set("fillcolor", "green", true);
                dot_node.set("color", "green", true);
                let weight = *final_distribution.get(state).unwrap();
                dot_node.set("xlabel", &format!("{:.3}", weight), true);
            } else if subgraph.always_reaches(|node| deadlocks.contains(node)) {
                dot_node.set("fillcolor", "#f0b0b0", true);
                dot_node.set("color", "#f0b0b0", true);
            } else if subgraph.always_reaches(|node| success.contains(node)) {
                dot_node.set("fillcolor", "#b0f0b0", true);
                dot_node.set("color", "#b0f0b0", true);
            } else {
                dot_node.set("fillcolor", "#808080", true);
                dot_node.set("color", "#808080", true);
            }
        },
        |from, to, dot_edge| {
            // let probability = *graph.get_edge_attribute(from, to).unwrap();
            // dot_edge.set("label", &format!("{}", probability), true);
            if from == to {
                dot_edge.set("style", "invis", true);
            }
        },
    );
}
