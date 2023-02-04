use super::*;

/// A simple simulator, that attempts to progress each transition
pub struct SimpleSimulator<'a>(&'a PetriNetwork);

impl<'a> Simulator<'a> for SimpleSimulator<'a> {
    fn init(network: &'a PetriNetwork) -> Self {
        SimpleSimulator(network)
    }

    fn get_next_states(&self, state: &[u8]) -> HashSet<Vec<u8>> {
        let network = self.0;

        let mut res = vec![Vec::from(state)];

        for transition in network.transitions() {
            if transition.is_active(state) {
                let mut new_state = Vec::from(state);
                transition.apply(&mut new_state);
                res.push(new_state);
            }
        }

        return res.into_iter().collect();
    }
}
