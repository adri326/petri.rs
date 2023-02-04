use super::*;
use dot_writer::*;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;

pub fn export_network(network: &PetriNetwork, writer: &mut DotWriter) {
    let mut digraph = writer.digraph();
    digraph.set_font_size(14.0);
    digraph.set("nodesep", "0.5", false);

    let mut groups: HashMap<String, (Vec<usize>, Vec<usize>)> = HashMap::new();

    for (index, data) in network.node_data.iter().enumerate() {
        for group in data.groups.iter() {
            match groups.get_mut(group) {
                Some(ref mut members) => members.0.push(index),
                None => {
                    groups.insert(group.clone(), (vec![index], vec![]));
                }
            }
        }
    }

    for (index, transition) in network.transitions.iter().enumerate() {
        for group in transition.groups.iter() {
            match groups.get_mut(group) {
                Some(ref mut members) => members.1.push(index),
                None => {
                    groups.insert(group.clone(), (vec![], vec![index]));
                }
            }
        }
    }

    // for (group_name, members) in groups {
    //     let mut cluster = digraph.cluster();
    //     cluster.set_label(&group_name);
    //     cluster.set_style(Style::Filled);
    //     for index in members.0 {
    //         cluster.node_named(&format!("N{}", index));
    //     }
    //     for index in members.1 {
    //         cluster.node_named(&format!("T{}", index));
    //     }
    // }

    for (index, &value) in network.nodes.iter().enumerate() {
        let data = &network.node_data[index];
        let mut node = digraph.node_named(&format!("N{}", index));
        node.set_shape(Shape::Circle);

        if value <= 3 {
            node.set_label(&format!("{}", "&#9679;".repeat(value as usize)));
            // if value == 1 {
            //     node.set_font_size(28.0);
            //     node.set("width", "0.5", false);
            //     node.set("height", "0.5", false);
            //     node.set("fixedsize", "true", false);
            // }
        } else {
            node.set_label(&format!("{}", value));
        }
        if data.label.is_some() {
            node.set("xlabel", &format!("{}", data.label.as_ref().unwrap()), true);
        }
    }

    for (index, transition) in network.transitions.iter().enumerate() {
        let name = format!("T{}", index);
        {
            let mut transition_node = digraph.node_named(&name);
            transition_node.set_shape(Shape::Rectangle);
            transition_node.set("height", "0.4", false);
            transition_node.set("width", "0.4", false);
            transition_node.set("margin", "0.01", false);
            transition_node.set_style(Style::Filled);

            if let Some(label) = &transition.label {
                transition_node.set_label(label);
            }
        }

        for input in transition.inputs.iter() {
            digraph.edge(&format!("N{}", input), &name);
        }

        for output in transition.outputs.iter() {
            digraph.edge(&name, &format!("N{}", output));
        }
    }
}

pub fn export_graph(
    graph: &PetriGraph,
    writer: &mut DotWriter,
    format_node: impl Fn(&[u8], &mut Node),
    format_edge: impl Fn(&[u8], &[u8], &mut AttributesList),
) {
    fn hash(state: &[u8]) -> u64 {
        let mut hasher = DefaultHasher::new();
        for &x in state {
            hasher.write_u8(x);
        }
        hasher.finish()
    }
    let mut digraph = writer.digraph();
    digraph.set_font_size(14.0);
    digraph.set("nodesep", "0.5", false);

    for (state, _) in graph.iter() {
        let mut node = digraph.node_named(&format!("S{:08x}", hash(state)));
        format_node(state, &mut node);
    }

    for (state, next_states) in graph.iter() {
        let name = format!("S{:08x}", hash(state));
        for next_state in next_states {
            let edge = digraph.edge(&name, &format!("S{:08x}", hash(next_state)));
            format_edge(state, next_state, &mut edge.attributes());
        }
    }
}
