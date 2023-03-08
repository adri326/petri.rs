use super::*;

/// A simple simulator, that attempts to progress each transition
pub struct SimpleSimulator<'a>(&'a PetriNetwork);

impl<'a> Simulator<'a> for SimpleSimulator<'a> {
    fn init(network: &'a PetriNetwork) -> Self {
        SimpleSimulator(network)
    }

    fn get_next_states(&self, state: &[u8]) -> HashMap<Vec<u8>, f64> {
        let network = self.0;

        let mut res = vec![(Vec::from(state), 0.0)];

        for transition in network.transitions() {
            if transition.is_active(state) {
                let mut new_state = Vec::from(state);
                transition.apply(&mut new_state);
                res.push((new_state, transition.probability));
            }
        }

        return res.into_iter().collect();
    }
}
