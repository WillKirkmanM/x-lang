// Demonstrates the `throws` keyword lowering to call (default) or invoke (when XLANG_ENABLE_INVOKE=1)

pure fn add_one(x: f64) -> f64 { x + 1.0 }

throws fn might_fail(x: f64) -> f64 {
    // No actual throw statement yet; the annotation marks the call as potentially throwing.
    x * 2.0
}

fn main() {
    let a: f64 = add_one(41.0);   // nothrow -> call
    let b: f64 = might_fail(3.0); // throws  -> call by default, invoke if XLANG_ENABLE_INVOKE=1
    print(a);
    print(b);
}