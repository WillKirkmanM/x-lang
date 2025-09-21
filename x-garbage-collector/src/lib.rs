use std::collections::{HashMap, HashSet};
use std::mem::size_of;
use std::sync::{Mutex, OnceLock};

/// Runtime-visible type identifier for GC-traceable layouts.
/// Codegen should assign stable ids per struct/array layout and register
/// their interior pointer offsets via `gc_register_descriptor`.
pub type TypeId = u32;

#[derive(Default)]
struct TypeDescriptor {
    // Offsets (in bytes) from the object base where a pointer field lives.
    // Tracer will read a pointer-sised value at each offset and follow it if tracked.
    pointer_offsets: Vec<usize>,
}

struct GcObject {
    data: Box<[u8]>,
    type_id: TypeId,
    marked: bool,
}

impl GcObject {
    fn ptr(&mut self) -> *mut u8 {
        self.data.as_mut_ptr()
    }
    fn addr(&mut self) -> usize {
        self.ptr() as usize
    }
}

#[derive(Default)]
struct GcState {
    objects: HashMap<usize, GcObject>, // addr -> object
    roots: HashSet<usize>,             // root addresses
    descriptors: HashMap<TypeId, TypeDescriptor>,
    bytes_allocated: usize,
}

impl GcState {
    fn new() -> Self {
        Self::default()
    }

    fn register_descriptor(&mut self, type_id: TypeId, pointer_offsets: &[usize]) {
        self.descriptors.insert(
            type_id,
            TypeDescriptor {
                pointer_offsets: pointer_offsets.to_vec(),
            },
        );
    }

    fn alloc(&mut self, size: usize, type_id: TypeId) -> *mut u8 {
        let data = vec![0u8; size].into_boxed_slice();
        let mut obj = GcObject {
            data,
            type_id,
            marked: false,
        };
        let addr = obj.addr();
        self.bytes_allocated += size;
        let ptr = obj.ptr();
        self.objects.insert(addr, obj);
        ptr
    }

    fn add_root(&mut self, ptr: *mut u8) {
        if !ptr.is_null() {
            self.roots.insert(ptr as usize);
        }
    }

    fn remove_root(&mut self, ptr: *mut u8) {
        self.roots.remove(&(ptr as usize));
    }

    fn collect(&mut self) {
        // 1) Mark
        let roots: Vec<usize> = self.roots.iter().copied().collect();
        for root in roots {
            self.mark_from(root);
        }

        // 2) Sweep
        self.sweep();
    }

    fn mark_from(&mut self, addr: usize) {
        if let Some(obj) = self.objects.get_mut(&addr) {
            if obj.marked {
                return;
            }
            obj.marked = true;

            // Collect interior pointers while we hold the mutable borrow.
            let mut children = Vec::new();
            if let Some(desc) = self.descriptors.get(&obj.type_id) {
                for &off in &desc.pointer_offsets {
                    // Safety: we read a machine pointer from object data at the given offset.
                    // Offsets should be codegen-provided and valid for the layout.
                    if off + size_of::<usize>() <= obj.data.len() {
                        let child = unsafe {
                            let base = obj.data.as_ptr().add(off) as *const *mut u8;
                            (*base) as usize
                        };
                        if child != 0 {
                            children.push(child);
                        }
                    }
                }
            }

            // Drop the mutable borrow to allow recursive mutable borrows of `self`.
            let _ = obj;

            // Recurse on collected children without holding the previous borrow.
            for child in children {
                self.mark_from(child);
            }
        }
    }

    fn sweep(&mut self) {
        let mut to_remove = Vec::new();
        for (addr, obj) in self.objects.iter() {
            if !obj.marked {
                to_remove.push(*addr);
            }
        }
        for addr in to_remove {
            if let Some(mut obj) = self.objects.remove(&addr) {
                self.bytes_allocated = self.bytes_allocated.saturating_sub(obj.data.len());

                // drop obj to free memory
                obj.marked = false;
            }
        }
        // clear marks for next cycle
        for obj in self.objects.values_mut() {
            obj.marked = false;
        }
    }
}

static GC: OnceLock<Mutex<GcState>> = OnceLock::new();

fn with_gc<R>(f: impl FnOnce(&mut GcState) -> R) -> R {
    let m = GC
        .get_or_init(|| Mutex::new(GcState::new()))
        .lock()
        .expect("GC mutex poisoned");

    // Workaround to satisfy borrow checker: re-lock inside to pass &mut
    drop(m);
    let mut m = GC.get().unwrap().lock().unwrap();
    f(&mut m)
}

/// Initialise GC state (idempotent).
pub fn gc_init() {
    GC.get_or_init(|| Mutex::new(GcState::new()));
}

/// Allocate a GC-managed object of `size` bytes and associate a `type_id` (descriptor).
pub fn gc_alloc(size: usize, type_id: TypeId) -> *mut u8 {
    with_gc(|gc| gc.alloc(size, type_id))
}

/// Manually trigger collection (mark from roots and sweep).
pub fn gc_collect() {
    with_gc(|gc| gc.collect());
}

/// Register a type descriptor (interior pointer offsets) for a type_id.
pub fn gc_register_descriptor(type_id: TypeId, pointer_offsets: &[usize]) {
    with_gc(|gc| gc.register_descriptor(type_id, pointer_offsets));
}

/// Register/unregister a root pointer.
pub fn gc_add_root(ptr: *mut u8) {
    with_gc(|gc| gc.add_root(ptr));
}
pub fn gc_remove_root(ptr: *mut u8) {
    with_gc(|gc| gc.remove_root(ptr));
}

// C ABI for easy linking from codegen/runtime

#[no_mangle]
pub extern "C" fn gc_runtime_init() {
    gc_init();
}

#[no_mangle]
pub extern "C" fn gc_runtime_collect() {
    gc_collect();
}

#[no_mangle]
pub extern "C" fn gc_runtime_register_descriptor(
    type_id: u32,
    offsets_ptr: *const usize,
    len: usize,
) {
    if !offsets_ptr.is_null() && len > 0 {
        // Safety: caller promises `offsets_ptr` points to `len` valid usize entries
        let slice = unsafe { std::slice::from_raw_parts(offsets_ptr, len) };
        gc_register_descriptor(type_id, slice);
    } else {
        gc_register_descriptor(type_id, &[]);
    }
}

#[no_mangle]
pub extern "C" fn gc_runtime_alloc(size: usize, type_id: u32) -> *mut u8 {
    gc_alloc(size, type_id)
}

#[no_mangle]
pub extern "C" fn gc_runtime_add_root(ptr: *mut u8) {
    gc_add_root(ptr);
}

#[no_mangle]
pub extern "C" fn gc_runtime_remove_root(ptr: *mut u8) {
    gc_remove_root(ptr);
}
