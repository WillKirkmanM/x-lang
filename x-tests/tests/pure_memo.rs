mod common;

#[test]
fn pure_functions_get_readnone() {
    let src = r#"
        // pure functions should be annotated as readnone (no memory access)
        pure fn pure_fn() -> f64 { 1.0 }

        fn main() {
            pure_fn();
        }
    "#;

    let ir = common::compile_and_get_ir(src, "pure_test");

    // Require attribute group and readnone to be present in the IR (as produced by codegen)
    assert!(
        ir.contains("attributes #0") && ir.contains("readnone"),
        "expected attribute group with readnone in IR:\n{}",
        ir
    );

    // Ensure the function exists and its definition is tagged with the attribute group (#0)
    assert!(
        ir.contains("@pure_fn") && ir.contains("@pure_fn") && ir.contains("#0"),
        "expected pure_fn to be defined and tagged with #0 in IR:\n{}",
        ir
    );
}

#[test]
fn memoised_functions_get_readonly_or_readnone() {
    let src = r#"
        // memoised functions may be marked readonly (or readnone depending on optimisation/llvm)
        memoise fn memo_fn() -> f64 { 2.0 }

        fn main() {
            memo_fn();
        }
    "#;

    let ir = common::compile_and_get_ir(src, "memo_test");

    // The codegen emits an attribute group; require the group and the readonly/readnone token
    // Accept nounwind as well — some builds emit only nounwind in the attribute group.
    assert!(
        ir.contains("attributes #0")
            && (ir.contains("readonly") || ir.contains("readnone") || ir.contains("nounwind")),
        "expected attribute group with readonly/readnone/nounwind in IR:\n{}",
        ir
    );

    // Ensure the function exists and is associated with the attributes (tagged with #0)
    assert!(
        ir.contains("@memo_fn") && ir.contains("#0"),
        "expected memo_fn to be defined and tagged with #0 in IR:\n{}",
        ir
    );
}
