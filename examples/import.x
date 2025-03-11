import std::print;
import "./examples/module_utils.x";

fn main() {
    print("Imported functions demo:");
    
    let a = 10;
    let b = 5;
    
    print("Addition: {a} + {b} = ");
    print(add(a, b));
    
    print("Subtraction: {a} - {b} = ");
    print(subtract(a, b));
    
    print("Multiplication: {a} * {b} = ");
    print(multiply(a, b));
    
    print("Division: {a} / {b} = ");
    print(divide(a, b));
    
    // Test division by zero handling
    print("Division by zero test:");
    divide(10, 0);
}

main();