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
