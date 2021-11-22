use super::*;

pub struct Semaphore(usize); // (semaphore_id)
pub struct SemaphoreP(usize);
pub struct SemaphoreV(usize);

impl Semaphore {
    pub fn new(builder: &mut PetriBuilder, value: u8) -> Self {
        Self(builder.node(value))
    }

    pub fn p(&self) -> SemaphoreP {
        SemaphoreP(self.0)
    }

    pub fn v(&self) -> SemaphoreV {
        SemaphoreV(self.0)
    }
}

impl TransitionMod for SemaphoreP {
    fn apply(self, builder: &mut PetriTransitionBuilder) {
        builder.inputs.push(self.0);
    }
}

impl TransitionMod for SemaphoreV {
    fn apply(self, builder: &mut PetriTransitionBuilder) {
        builder.outputs.push(self.0);
    }
}
