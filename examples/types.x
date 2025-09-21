// 1. Struct definition with typed fields
struct Point {
    x: f64,
    y: f64,
}

// 2. Function with typed parameters and a return type
fn add(a: i32, b: i32) -> i32 {
    a + b
}

// 3. A function that uses a struct and returns void
fn print_point(p: Point) -> void {
    print_str("Point is:");
    print(p.x);
    print(p.y);
}   


fn main() {
    print_str("--- Demonstrating Types ---");

    // 4. Variable declarations with explicit type annotations
    let an_integer: i32 = 100;
    let another_integer: i32 = 250;
    let a_string: str = "Hello, typed world!";

    // 5. Variable declaration without a type annotation (for type inference)
    let inferred_float = 99.5;

    // 6. Calling a typed function
    let result: i32 = add(an_integer, another_integer);

    print_str("Result of add(100, 250):");
    print(result); // Expected: 350

    print_str("String variable:");
    print_str(a_string);

    // 7. Struct instantiation and usage
    let p1: Point = Point { x: 3.0, y: 4.0 };
    print_point(p1); // Expected: Point is:, 3, 4
}