mod common;

#[test]
fn traits_and_impls_generate_methods_and_defaults() {
    let src = r#"
        trait Printable {
            fn print(&self);
            fn describe(&self) {
                self.print();
            }
        }

        struct Point { x: f64, y: f64 }

        impl Printable for Point {
            fn print(&self) { }
        }

        fn show_it(p: &Point) {
            p.print();
            p.describe();
        }

        fn main() {
            let pt: Point = Point { x: 1.0, y: 2.0 };
            show_it(&pt);
        }
    "#;

    let ir = common::compile_and_get_ir(src, "traits_test");

    // Impl methods should be declared with mangled names
    assert!(
        ir.contains("@Point_print"),
        "expected Point_print in IR:\n{}",
        ir
    );
    assert!(
        ir.contains("@Point_describe"),
        "expected Point_describe in IR:\n{}",
        ir
    );

    // show_it should call the synthesised methods
    assert!(
        ir.contains("call void @Point_print"),
        "expected call to Point_print in IR:\n{}",
        ir
    );
    assert!(
        ir.contains("call void @Point_describe"),
        "expected call to Point_describe in IR:\n{}",
        ir
    );
}
