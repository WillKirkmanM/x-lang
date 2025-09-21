import "./examples/module_utils.x";

fn add(a: f64, b: f64) -> f64 {
    a + b
}

fn main() {
    print_str("Imported functions demo:");

    let a: f64 = 10.0;
    let b: f64 = 5.0;

    print_str("Addition: {a} + {b} = ");
    let sum = add(a, b);
    print(sum);

    print_str("Subtraction: {a} - {b} = ");
    print(subtract(a, b));

    print_str("Multiplication: {a} * {b} = ");
    print(multiply(a, b));

    print_str("Division: {a} / {b} = ");
    print(divide(a, b));

    // Test division by zero handling
    print_str("Division by zero test:");
    divide(10.0, 0.0);
}

main();