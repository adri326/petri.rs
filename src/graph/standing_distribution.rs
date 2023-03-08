use rand::Rng;
use std::collections::HashMap;

use super::EdgeMap;

#[non_exhaustive]
#[derive(Clone, Debug)]
pub struct Settings {
    pub steps: usize,
    pub iterations: usize,
    pub warmup_steps: usize,
}

impl Default for Settings {
    fn default() -> Self {
        Settings {
            steps: 1000,
            iterations: 1000,
            warmup_steps: 100,
        }
    }
}

pub fn monte_carlo(
    map: &EdgeMap<f64>,
    initial_state: &[u8],
    settings: Settings,
) -> HashMap<Vec<u8>, f64> {
    let mut res: HashMap<Vec<u8>, usize> = HashMap::new();
    let mut count = 0;
    let mut rng = rand::thread_rng();

    for node in map.iter_nodes() {
        res.insert(node.clone(), 0);
    }

    for _ in 0..settings.iterations {
        let mut state = initial_state.to_vec();
        for step in 0..settings.steps {
            if step >= settings.warmup_steps {
                *res.get_mut(&state).unwrap() += 1;
                count += 1;
            }

            let edges = map.get_edges_of(&state).unwrap();
            let random_value: f64 = rng.gen();
            let mut sum = 0.0;
            for (edge, &value) in edges {
                sum += value;
                if sum >= random_value {
                    state = edge.to_vec();
                    break;
                }
            }
        }
    }

    return res
        .into_iter()
        .map(|(k, v)| (k, v as f64 / count as f64))
        .collect();
}
