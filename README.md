# petri.rs

A small rust library to simulate, compare and display petri networks.

## Using this library

To start using this library, add the following to your `Cargo.toml`:

```toml
[dependencies]
petri = { git = "https://github.com/adri326/petri.rs" }
```

Then, you can start using the objects of `petri`:

```rs
use petri::{PetriNetwork, PetriTransition, PetriGraph};

let network = PetriNetwork::new(
    vec![1, 0],
    vec![
        PetriTransition::new(vec![0], vec![1]),
    ]
);

assert_eq!(network.step(), vec![vec![0, 1]]);

assert_eq!(network.generate_graph(), PetriGraph::from([
    (vec![1, 0], [vec![0, 1]]),
    (vec![0, 1], [vec![0, 1]]),
]));
```

Several functions are available for you to verify properties of petri networks.
The following example verifies a simple property on a petri network with different input states.

```rs
let mut network = PetriNetwork::new(
    vec![0; 6],
    vec![
        PetriTransition::new(vec![0], vec![2]),
        PetriTransition::new(vec![1], vec![3]),
        PetriTransition::new(vec![3], vec![4]),
        PetriTransition::new(vec![2, 4], vec![5]),
    ]
);

for a in 0..=1 {
    for b in 0..=1 {
        network.set_node(0, a);
        network.set_node(1, b);
        let graph = network.generate_graph();
        let reaches_c = graph.always_reaches(|state| state[5] == 1); // reaches_c = [A, B, 0, 0, 0, 0] ->> [0, 0, 0, 0, 0, 1]
        assert_eq!(reaches_c, a == 1 && b == 1); // reaches_c <=> (A, B) = (1, 1)
    }
}
```
