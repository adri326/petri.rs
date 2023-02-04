use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    num::ParseIntError,
};

use crate::{network::PetriNetwork, PetriTransition, PetriNodeData};

#[derive(Debug)]
pub enum ParseError {
    SyntaxError(usize),
    ParseIntError(ParseIntError),
}

pub fn parse(raw: &str) -> Result<PetriNetwork, ParseError> {
    let rules = parse_rules(raw)?;

    let transitions = rules
        .iter()
        .filter_map(|rule| match rule {
            Rule::Transition(t) => Some(t),
            _ => None,
        })
        .collect::<Vec<_>>();

    let assigns = rules
        .iter()
        .filter_map(|rule| match rule {
            Rule::Assign(node, value) => Some((node, *value)),
            _ => None,
        })
        .collect::<Vec<_>>();

    let mut nodes = transitions
        .iter()
        .flat_map(|transition| transition.inputs.iter().chain(transition.outputs.iter()))
        .collect::<HashSet<_>>()
        .into_iter()
        .cloned()
        .collect::<Vec<_>>();

    nodes.sort();

    let indices = get_indices(&nodes);

    let node_data = get_node_data(&indices);

    let nodes = get_values(&assigns, &indices);

    let transitions = transitions
        .iter()
        .map(|transition| generate_edge(transition, &indices))
        .collect::<Vec<_>>();

    Ok(PetriNetwork::new(
        nodes,
        node_data,
        transitions,
    ))
}

#[derive(Debug, PartialEq)]
enum Rule {
    Transition(Transition),
    Assign(String, u8),
}

#[derive(Debug, PartialEq)]
struct Transition {
    inputs: HashSet<String>,
    outputs: HashSet<String>,
}

fn parse_rules(raw: &str) -> Result<Vec<Rule>, ParseError> {
    let mut res = Vec::new();

    for (index, line) in raw.lines().enumerate() {
        let line = line.trim();
        if line.len() == 0 {
            continue;
        }

        let split = line.split("->").collect::<Vec<_>>();

        match split[..] {
            [left, right] => {
                let inputs = parse_set(left, index)?;
                let outputs = parse_set(right, index)?;

                res.push(Rule::Transition(Transition { inputs, outputs }))
            }
            [_] => {
                if let [name, value] = line.split('=').collect::<Vec<_>>()[..] {
                    let name = name.trim();
                    let value = value
                        .trim()
                        .parse::<u8>()
                        .map_err(|e| ParseError::ParseIntError(e))?;

                    res.push(Rule::Assign(name.to_owned(), value));
                } else {
                    return Err(ParseError::SyntaxError(index));
                }
            }
            _ => {
                return Err(ParseError::SyntaxError(index));
            }
        }
    }

    Ok(res)
}

fn parse_set(raw: &str, line: usize) -> Result<HashSet<String>, ParseError> {
    let raw = raw.trim();
    if raw.starts_with('{') {
        if !raw.ends_with('}') {
            return Err(ParseError::SyntaxError(line));
        }

        let mut res = HashSet::new();
        for item in raw[1..(raw.len() - 1)].split(' ') {
            if item.trim() == "" {
                continue;
            }

            if item.ends_with(',') {
                res.insert(item[0..(item.len() - 1)].to_owned());
            } else {
                res.insert(item.to_owned());
            }
        }
        Ok(res)
    } else {
        let mut res = HashSet::with_capacity(1);
        res.insert(raw.trim().to_owned());
        Ok(res)
    }
}

fn generate_edge(rule: &Transition, indices: &HashMap<String, usize>) -> PetriTransition {
    let inputs = rule
        .inputs
        .iter()
        .map(|input| indices.get(input).copied())
        .collect::<Option<Vec<_>>>()
        .unwrap();

    let outputs = rule
        .outputs
        .iter()
        .map(|output| indices.get(output).copied())
        .collect::<Option<Vec<_>>>()
        .unwrap();

    PetriTransition::new(inputs, outputs, vec![])
}

fn get_indices<'a, T: Hash + Clone + Eq + 'a>(
    iter: impl IntoIterator<Item = &'a T>,
) -> HashMap<T, usize> {
    let mut res = HashMap::new();

    for (index, item) in iter.into_iter().enumerate() {
        res.insert(item.clone(), index);
    }

    res
}

fn get_values<'a, 'b: 'a>(assigns: impl IntoIterator<Item=&'a (impl AsRef<str> + 'a, u8)>, indices: &HashMap<String, usize>) -> Vec<u8> {
    let mut res = vec![0; indices.len()];

    for (name, value) in assigns.into_iter() {
        res[indices.get(name.as_ref()).copied().unwrap()] = *value;
    }

    res
}

fn get_node_data(indices: &HashMap<String, usize>) -> Vec<PetriNodeData> {
    let mut res = vec![Default::default(); indices.len()];

    for (name, index) in indices {
        res[*index] = PetriNodeData {
            label: Some(name.clone()),
            groups: vec![],
            max_value: None
        }
    }

    dbg!(res)
}

#[cfg(test)]
mod test {
    use crate::PetriBuilder;

    use super::*;

    macro_rules! transition {
        ( [ $( $input:expr ),* ] , [ $( $output:expr ),* ] ) => {
            Rule::Transition(Transition {
                inputs: [ $( $input ),* ].into_iter().map(|x| x.to_owned()).collect::<HashSet<String>>(),
                outputs: [ $( $output ),* ].into_iter().map(|x| x.to_owned()).collect::<HashSet<String>>()
            })
        }
    }

    #[test]
    fn should_parse_empty() {
        assert_eq!(parse_rules("").unwrap(), vec![]);
    }

    #[test]
    fn should_parse_simple() {
        assert_eq!(
            parse_rules("A -> B").unwrap(),
            vec![transition!(["A"], ["B"])]
        );
    }

    #[test]
    fn should_parse_output_set() {
        assert_eq!(
            parse_rules("A -> {B C, D,}").unwrap(),
            vec![transition!(["A"], ["B", "C", "D"])]
        );
    }

    #[test]
    fn should_parse_input_set() {
        assert_eq!(
            parse_rules("{A, B C,} -> D").unwrap(),
            vec![transition!(["A", "B", "C"], ["D"])]
        );
    }

    #[test]
    fn should_parse_multiple() {
        assert_eq!(
            parse_rules("A -> B\nC -> D").unwrap(),
            vec![transition!(["A"], ["B"]), transition!(["C"], ["D"])]
        );
    }

    #[test]
    fn should_fully_parse_simple() {
        let mut builder = PetriBuilder::new();
        let start = builder.node_with_label(0, "A");
        builder.begin_branch(start)
            .step_with_label("B");

        assert_eq!(
            parse("A -> B").unwrap(),
            builder.build()
        );
    }

    #[test]
    fn should_assign_values() {
        let mut builder = PetriBuilder::new();
        let start = builder.node_with_label(3, "A");
        let end = builder.node_with_label(2, "B");

        builder.begin_transition()
            .input(start)
            .output(end)
            .build();

        builder.begin_transition()
            .input(end)
            .output(start)
            .build();

        assert_eq!(
            parse("A = 3\nB = 2\n\nA -> B\nB -> A").unwrap(),
            builder.build()
        );
    }
}
