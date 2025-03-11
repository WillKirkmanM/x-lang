fn add(a, b) {
    return a + b;
}

fn subtract(a, b) {
    return a - b;
}

fn multiply(a, b) {
    return a * b;
}

fn divide(a, b) {
    if b == 0 {
        print("Error: Division by zero");
        return 0;
    }
    return a / b;
}