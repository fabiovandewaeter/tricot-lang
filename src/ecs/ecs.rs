use bitvec::prelude::*;
use std::collections::HashMap;

use crate::values::Value;

#[derive(Clone)]
pub struct EntityMeta {
    pub components: BitVec,
    pub alive: bool,
}

pub struct ComponentStore {
    sparse: Vec<Option<usize>>,
    dense: Vec<u64>,
    data: Vec<Value>,
}

impl ComponentStore {
    pub fn new() -> Self {
        Self {
            sparse: Vec::new(),
            dense: Vec::new(),
            data: Vec::new(),
        }
    }

    pub fn insert(&mut self, entity: u64, value: Value) {
        if entity as usize >= self.sparse.len() {
            self.sparse.resize(entity as usize + 1, None);
        }

        if let Some(idx) = self.sparse[entity as usize] {
            self.data[idx] = value;
        } else {
            let idx = self.dense.len();
            self.sparse[entity as usize] = Some(idx);
            self.dense.push(entity);
            self.data.push(value);
        }
    }

    pub fn get(&self, entity: u64) -> Option<&Value> {
        self.sparse
            .get(entity as usize)
            .and_then(|&idx| idx.and_then(|i| self.data.get(i)))
    }

    pub fn get_mut(&mut self, entity: u64) -> Option<&mut Value> {
        if let Some(Some(idx)) = self.sparse.get(entity as usize) {
            self.data.get_mut(*idx)
        } else {
            None
        }
    }

    pub fn contains(&self, entity: u64) -> bool {
        (entity as usize) < self.sparse.len() && self.sparse[entity as usize].is_some()
    }
}

pub struct World {
    pub next_entity_id: u64,
    pub entities: Vec<EntityMeta>,
    pub component_stores: HashMap<String, ComponentStore>,
    pub component_ids: HashMap<String, usize>,
    next_component_index: usize,
}

impl World {
    pub fn new() -> Self {
        World {
            next_entity_id: 0,
            entities: Vec::new(),
            component_stores: HashMap::new(),
            component_ids: HashMap::new(),
            next_component_index: 0,
        }
    }

    pub fn spawn(&mut self) -> u64 {
        let id = self.next_entity_id;
        self.next_entity_id += 1;

        let components = BitVec::repeat(false, self.next_component_index);

        if id as usize >= self.entities.len() {
            self.entities.resize(
                id as usize + 1,
                EntityMeta {
                    components: components.clone(),
                    alive: false,
                },
            );
        }

        self.entities[id as usize] = EntityMeta {
            components,
            alive: true,
        };

        id
    }

    pub fn add_component(&mut self, entity: u64, comp_name: String, value: Value) {
        let bit_index = *self
            .component_ids
            .entry(comp_name.clone())
            .or_insert_with(|| {
                let idx = self.next_component_index;
                self.next_component_index += 1;

                for meta in &mut self.entities {
                    meta.components.resize(self.next_component_index, false);
                }

                self.component_stores
                    .insert(comp_name.clone(), ComponentStore::new());
                idx
            });

        if let Some(meta) = self.entities.get_mut(entity as usize) {
            meta.components.set(bit_index, true);
        }

        let store = self.component_stores.get_mut(&comp_name).unwrap();
        store.insert(entity, value);
    }

    pub fn get_component_mut(&mut self, entity: u64, component_type: &str) -> Option<&mut Value> {
        self.component_stores
            .get_mut(component_type)?
            .get_mut(entity)
    }

    pub fn query_entities(&self, component_names: &[&str]) -> Vec<u64> {
        let mut mask: BitVec<usize, Lsb0> = BitVec::repeat(false, self.next_component_index);

        for name in component_names {
            if let Some(idx) = self.component_ids.get(*name) {
                mask.set(*idx, true);
            } else {
                return Vec::new();
            }
        }

        let mut results = Vec::new();
        for (entity_id, meta) in self.entities.iter().enumerate() {
            let entity_id = entity_id as u64;
            if entity_id >= self.next_entity_id || !meta.alive {
                continue;
            }

            let mut has_all = true;
            for idx in mask.iter_ones() {
                if !meta.components[idx] {
                    has_all = false;
                    break;
                }
            }

            if has_all {
                results.push(entity_id);
            }
        }

        results
    }
}
