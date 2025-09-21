mod common;

#[test]
fn multi_dispatch_generates_resolver_and_variants() {
    let src = r#"
        struct Circle { r: f64 }
        struct Rect { w: f64, h: f64 }

        multi fn collide(a: &Circle, b: &Circle) -> bool { true }
        multi fn collide(a: &Circle, b: &Rect) -> bool { false }
        multi fn collide(a: &Rect,   b: &Circle) -> bool { false }
        multi fn collide(a: &Rect,   b: &Rect) -> bool { true }

        fn main() {
            let c1: Circle = Circle { r: 1.0 };
            let c2: Circle = Circle { r: 2.0 };
            let re: Rect = Rect { w: 2.0, h: 3.0 };

            let cc: bool = collide(&c1, &c2);
            let cr: bool = collide(&c1, &re);
            let rc: bool = collide(&re, &c2);
            let rr: bool = collide(&re, &re);
        }
    "#;

    let ir = common::compile_and_get_ir(src, "multi_test");

    // Resolver symbol should be present
    assert!(
        ir.contains("define i1 @collide("),
        "expected multi resolver 'collide' to be defined in IR:\n{}",
        ir
    );

    // Calls in `main` should call the resolver (i.e. use @collide)
    assert!(
        ir.contains("call i1 @collide("),
        "expected calls to resolver 'collide' in IR:\n{}",
        ir
    );

    // Type-id helper used by resolver should be declared
    assert!(
        ir.contains("__xlang_type_id"),
        "expected type id helper declaration in IR:\n{}",
        ir
    );
}