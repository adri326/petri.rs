use super::*;
use dot_writer::*;

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

    for (group_name, members) in groups {
        let mut cluster = digraph.cluster();
        cluster.set_label(&group_name);
        cluster.set_style(Style::Filled);
        for index in members.0 {
            cluster.node_named(&format!("N{}", index));
        }
        for index in members.1 {
            cluster.node_named(&format!("T{}", index));
        }
    }

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
