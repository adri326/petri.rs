use super::*;
use dot_writer::*;

pub fn export_network(network: &PetriNetwork, writer: &mut DotWriter) {
    let mut digraph = writer.digraph();
    digraph.set_font_size(14.0);
    digraph.set("nodesep", "0.5", false);

    for (index, &value) in network.nodes.iter().enumerate() {
        let mut node = digraph.node_named(&format!("N{}", index));
        node.set_shape(Shape::Circle);

        if value <= 3 {
            node.set_label(&format!("{}", "\u{25cf}".repeat(value as usize)));
            if value == 1 {
                node.set_font_size(28.0);
                node.set("width", "0.5", false);
                node.set("height", "0.5", false);
                node.set("fixedsize", "true", false);
            }
        } else {
            node.set_label(&format!("{}", value));
        }
    }

    for (index, transition) in network.transitions.iter().enumerate() {
        let name = format!("T{}", index);
        digraph.node_named(&name)
            .set_shape(Shape::Rectangle)
            .set("height", "0.4", false)
            .set("width", "0.4", false)
            .set("margin", "0.01", false)
            .set_style(Style::Filled);

        for input in transition.inputs.iter() {
            digraph.edge(&format!("N{}", input), &name);
        }

        for output in transition.outputs.iter() {
            digraph.edge(&name, &format!("N{}", output));
        }
    }
}
