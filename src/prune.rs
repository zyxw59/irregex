use std::collections::hash_map::RandomState;
use std::hash::{BuildHasher, Hash, Hasher};

pub struct PruneList<S = RandomState> {
    last_seen: Vec<Option<ThreadStateHash>>,
    hasher: S,
}

impl PruneList<RandomState> {
    pub fn new(len: usize) -> Self {
        PruneList {
            // add one spot to allow for "one-past-the-end jumps"
            last_seen: vec![None; len + 1],
            hasher: RandomState::new(),
        }
    }
}

impl<S: BuildHasher> PruneList<S> {
    /// Inserts the thread state into the list, and returns `true` if the state has been seen
    /// before.
    pub fn insert<E: Hash>(&mut self, pc: usize, state: &E, input_idx: usize) -> bool {
        let engine_hash = {
            let mut hasher = self.hasher.build_hasher();
            state.hash(&mut hasher);
            hasher.finish()
        };
        let new_state = ThreadStateHash {
            engine_hash,
            input_idx,
        };
        if let Some(old_state) = self.last_seen[pc] {
            if new_state == old_state {
                return true;
            }
        }
        self.last_seen[pc] = Some(new_state);
        false
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct ThreadStateHash {
    engine_hash: u64,
    input_idx: usize,
}
