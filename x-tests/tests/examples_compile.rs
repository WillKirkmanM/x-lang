mod helpers;
use helpers::*;

#[test]
fn hello_compiles() { compile_example("hello.x"); }

#[test]
fn fibonacci_compiles() { compile_example("fibonacci.x"); }

#[test]
fn pure_compiles() { compile_example("pure.x"); }

#[test]
fn memoise_compiles() { compile_example("memoise.x"); }

#[test]
fn types_compiles() { compile_example("types.x"); }

#[test]
fn struct_compiles() { compile_example("struct.x"); }

#[test]
fn features_compiles() { compile_example("features.x"); }

#[test]
fn selection_sort_compiles() { compile_example("selection_sort.x"); }

#[test]
fn bubble_sort_compiles() { compile_example("bubble_sort.x"); }

#[test]
fn binary_search_compiles() { compile_example("binary_search.x"); }

#[test]
fn generics_compiles() { compile_example("generics.x"); }

#[test]
fn hanoi_compiles() { compile_example("hanoi.x"); }

#[test]
fn math_compiles() { compile_example("math.x"); }

#[test]
fn module_utils_compiles() { compile_example("module_utils.x"); }

#[test]
fn newton_sqrt_compiles() { compile_example("newton_sqrt.x"); }

#[test]
fn import_compiles() { compile_example("import.x"); }

#[test]
fn lifetime_compiles() { compile_example("lifetime.x"); }

// Added missing example tests
#[test]
fn become_compiles() { compile_example("become.x"); }

#[test]
fn move_error_compiles() { compile_example("move_error.x"); }

#[test]
fn traits_compiles() { compile_example("traits.x"); }

// Throws should compile with default (call) lowering.
#[test]
fn throws_compiles() { compile_example("throws.x"); }

#[test]
fn multi_compiles() { compile_example("multi.x"); }

// Raylib requires external library/tooling; keep ignored in CI.
// #[test]
// fn raylib_compiles() { compile_example("raylib.x"); }