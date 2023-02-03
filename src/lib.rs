mod graph;
pub use graph::PetriGraph;

mod network;
pub use network::{data::PetriNodeData, PetriNetwork, PetriTransition};

pub mod builder;
pub use builder::{PetriBuilder, PetriTransitionBuilder};

pub mod parser;

#[cfg(any(
    all(feature = "conflict_fast", feature = "conflict_normal"),
    all(feature = "conflict_fast", feature = "conflict_slow"),
    all(feature = "conflict_normal", feature = "conflict_slow"),
))]
compile_error!("More than one flag for conflict resolution was set!");
