mod common;

#[test]
fn throws_compiles_call_or_invoke() {
    let src = r#"
        // Demonstrates `throws` lowering (call vs invoke)
        pure fn add_one(x: f64) -> f64 { x + 1.0 }

        throws fn might_fail(x: f64) -> f64 {
            x * 2.0
        }

        fn main() {
            let a: f64 = add_one(41.0);
            let b: f64 = might_fail(3.0);
            print(a);
            print(b);
        }
    "#;

    let ir = common::compile_and_get_ir(src, "throws_test");

    // might_fail is annotated `throws` — depending on feature flags it may lower to call or invoke.
    assert!(
        ir.contains("call double @might_fail") || ir.contains("invoke double @might_fail"),
        "expected call or invoke for might_fail in IR:\n{}",
        ir
    );

    // add_one is pure, should be a plain call
    assert!(
        ir.contains("call double @add_one"),
        "expected call to add_one in IR:\n{}",
        ir
    );
}