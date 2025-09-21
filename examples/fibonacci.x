fn fibonacci_recursive(n: f64) -> f64 {
    if n <= 1.0 {
        return n;
    } else {
        return fibonacci_recursive(n - 1.0) + fibonacci_recursive(n - 2.0);
    }
}

fn fibonacci_iterative(n: f64) -> f64 {
    if n <= 1.0 {
        return n;
    }
    
    let mut a: f64 = 0.0;
    let mut b: f64 = 1.0;
    let mut i: f64 = 2.0;
    
    while i <= n {
        let temp: f64 = a + b;
        a = b;
        b = temp;
        i = i + 1.0;
    }
    
    return b;
}

fn main() {
    print_str("Recursive Fibonacci:");
    for i in 0.0..10.0 {
        print(fibonacci_recursive(i));
    }

    print_str("Iterative Fibonacci:");
    for i in 0.0..10.0 {
        print(fibonacci_iterative(i));
    }
}