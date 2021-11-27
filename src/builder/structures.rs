use super::*;
use std::borrow::Cow;

pub struct Semaphore(usize); // (semaphore_id)
#[derive(Debug)]
pub struct SemaphoreP(usize);
#[derive(Debug)]
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

impl<'a> BuilderMod<'a> for SemaphoreP {
    fn apply_transition(&self, builder: &mut PetriTransitionBuilder) {
        builder.inputs.push(self.0);
    }
}

impl<'a> BuilderMod<'a> for SemaphoreV {
    fn apply_transition(&self, builder: &mut PetriTransitionBuilder) {
        builder.outputs.push(self.0);
    }
}

pub struct Mutex(usize, String); // (mutex_id, mutex_name)
#[derive(Debug)]
pub struct MutexP<'a>(usize, Cow<'a, String>);
#[derive(Debug)]
pub struct MutexV<'a>(usize, Cow<'a, String>);

impl Mutex {
    pub fn new(builder: &mut PetriBuilder, value: u8, name: impl Into<String> + Clone) -> Self {
        Self(builder.node_with_label(value, name.clone()), name.into())
    }

    pub fn index(&self) -> usize {
        self.0
    }

    pub fn name(&self) -> &String {
        &self.1
    }

    pub fn p<'b>(&'b self) -> MutexP<'b> {
        MutexP(self.0, Cow::Borrowed(&self.1))
    }

    pub fn p_static(&self) -> MutexP<'static> {
        MutexP(self.0, Cow::Owned(self.1.clone()))
    }

    pub fn v<'b>(&'b self) -> MutexV<'b> {
        MutexV(self.0, Cow::Borrowed(&self.1))
    }

    pub fn v_static(&self) -> MutexV<'static> {
        MutexV(self.0, Cow::Owned(self.1.clone()))
    }
}

impl<'a, 'b> BuilderMod<'a> for MutexP<'b> {
    fn apply_transition(&self, builder: &mut PetriTransitionBuilder) {
        builder.inputs.push(self.0);
    }

    fn apply_node(&self, builder: &mut PetriBuilder, index: usize) {
        builder.add_group(index, &*self.1);
    }
}

impl<'a, 'b> BuilderMod<'a> for MutexV<'b> {
    fn apply_transition(&self, builder: &mut PetriTransitionBuilder) {
        builder.outputs.push(self.0);
    }

    fn apply_node(&self, builder: &mut PetriBuilder, index: usize) {
        builder.add_group(index, &*self.1);
    }
}
