use std::collections::HashMap;

use crate::values::Value;

// Structures supplémentaires pour gérer les entités et composants
pub struct Entity {
    pub id: u64,
    pub components: HashMap<String, Value>,
}

pub struct EcsRegistry {
    pub next_entity_id: u64,
    pub entities: HashMap<u64, Entity>,
    pub components: HashMap<String, HashMap<u64, Value>>, // Composant -> (Entité -> Valeur)
}

impl EcsRegistry {
    pub fn new() -> Self {
        EcsRegistry {
            next_entity_id: 0,
            entities: HashMap::new(),
            components: HashMap::new(),
        }
    }

    // Crée une nouvelle entité et retourne son ID
    pub fn spawn_entity(&mut self, components: Vec<(String, Value)>) -> u64 {
        let id = self.next_entity_id;
        self.next_entity_id += 1;

        let mut entity_components = HashMap::new();
        for (comp_type, value) in components {
            entity_components.insert(comp_type.clone(), value.clone());
            self.components
                .entry(comp_type)
                .or_insert_with(HashMap::new)
                .insert(id, value);
        }

        self.entities.insert(
            id,
            Entity {
                id,
                components: entity_components,
            },
        );

        id
    }

    // Récupère un composant mutable pour une entité
    pub fn get_component_mut(
        &mut self,
        entity_id: u64,
        component_type: &str,
    ) -> Option<&mut Value> {
        self.components.get_mut(component_type)?.get_mut(&entity_id)
    }
}
