use std::collections::HashMap;

use crate::*;

pub mod simple;
pub use simple::SimpleSimulator;

pub mod recursive_brancher;
pub use recursive_brancher::RecursiveBrancher;

pub mod exhaustive_brancher;
pub use exhaustive_brancher::ExhaustiveBrancher;

pub trait Simulator<'a> {
    fn init(network: &'a PetriNetwork) -> Self;

    fn get_next_states(&self, state: &[u8]) -> HashMap<Vec<u8>, f64>;
}
