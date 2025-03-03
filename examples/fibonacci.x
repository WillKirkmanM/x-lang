import std::print;

fn fibonacci_recursive(n) {
    if n <= 1 {
        n;
    } else {
        fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2);
    }
}

fn fibonacci_iterative(n) {
    if n <= 1 {
        return n;
    }
    
    let a = 0;
    let b = 1;
    let i = 2;
    
    while i <= n {
        let temp = a + b;
        a = b;
        b = temp;
        i = i + 1;
    }
    
    return b;
}

print("Recursive Fibonacci:");
for i in 0..10 {
    print(fibonacci_recursive(i));
}

print("Iterative Fibonacci:");
for i in 0..10 {
    print(fibonacci_iterative(i));
}