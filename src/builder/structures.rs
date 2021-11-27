use super::*;

pub struct Semaphore(usize); // (semaphore_id)
#[derive(Clone, Debug)]
pub struct SemaphoreP(usize);
#[derive(Clone, Debug)]
pub struct SemaphoreV(usize);

impl Semaphore {
    pub fn new(builder: &mut PetriBuilder, value: u8) -> Self {
        Self(builder.node(value))
    }

    pub fn index(&self) -> usize {
        self.0
    }

    pub fn p(&self) -> SemaphoreP {
        SemaphoreP(self.0)
    }

    pub fn v(&self) -> SemaphoreV {
        SemaphoreV(self.0)
    }
}

impl BuilderMod for SemaphoreP {
    fn apply_transition(&self, builder: &mut PetriTransitionBuilder) {
        builder.inputs.push(self.0);
    }
}

impl BuilderMod for SemaphoreV {
    fn apply_transition(&self, builder: &mut PetriTransitionBuilder) {
        builder.outputs.push(self.0);
    }
}

pub struct Mutex(usize, String); // (mutex_id, mutex_name)
#[derive(Clone, Debug)]
pub struct MutexP(usize, String);
#[derive(Clone, Debug)]
pub struct MutexV(usize, String);
#[derive(Clone, Debug)]
pub struct MutexSection(usize, String);

impl Mutex {
    pub fn new(builder: &mut PetriBuilder, value: u8, name: impl Into<String> + Clone) -> Self {
        let mut res = Self(builder.node_with_label(value, name.clone()), name.into());
        builder.add_group(res.0, &res.1);
        res
    }

    pub fn index(&self) -> usize {
        self.0
    }

    pub fn name(&self) -> &String {
        &self.1
    }

    pub fn p(&self) -> MutexP {
        MutexP(self.0, self.1.clone())
    }

    pub fn v(&self) -> MutexV {
        MutexV(self.0, self.1.clone())
    }

    pub fn section(&self) -> MutexSection {
        MutexSection(self.0, self.1.clone())
    }
}

impl BuilderMod for MutexP {
    fn apply_transition(&self, builder: &mut PetriTransitionBuilder) {
        builder.inputs.push(self.0);
        // TODO
        // builder.add_group(&*self.1)
    }

    fn apply_node(&self, builder: &mut PetriBuilder, index: usize) {
        builder.add_group(index, &*self.1);
    }
}

impl BuilderMod for MutexV {
    fn apply_transition(&self, builder: &mut PetriTransitionBuilder) {
        builder.outputs.push(self.0);
        // TODO
        // builder.add_group(&*self.1)
    }
}

impl BuilderMod for MutexSection {
    fn apply_transition(&self, builder: &mut PetriTransitionBuilder) {
        // TODO
        // builder.add_group(&*self.1)
    }

    fn apply_node(&self, builder: &mut PetriBuilder, index: usize) {
        builder.add_group(index, &*self.1);
    }
}
