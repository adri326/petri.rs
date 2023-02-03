#[derive(Clone, Debug)]
pub struct PetriNodeData {
    pub label: Option<String>,
    pub groups: Vec<String>,
    pub max_value: Option<u8>,
}

impl Default for PetriNodeData {
    fn default() -> Self {
        Self {
            label: None,
            groups: Vec::new(),
            max_value: None,
        }
    }
}

impl PartialEq for PetriNodeData {
    fn eq(&self, other: &Self) -> bool {
        self.max_value == other.max_value
    }
}

impl Eq for PetriNodeData {}

impl PetriNodeData {
    pub fn label(mut self, label: impl Into<String>) -> Self {
        self.label = Some(label.into());

        self
    }

    pub fn groups(mut self, groups: Vec<String>) -> Self {
        self.groups = groups;

        self
    }

    pub fn max_value(mut self, max_value: Option<u8>) -> Self {
        self.max_value = max_value;

        self
    }
}
