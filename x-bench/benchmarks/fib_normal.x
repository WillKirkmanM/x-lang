// Calculates the nth Fibonacci number using slow, simple recursion.
pure fn fib(n: f64) -> f64 {
    if (n <= 2.0) { return 1.0; }
    fib(n - 1.0) + fib(n - 2.0)
}

// Calculating the 40th number will be noticeably slow.
print(fib(40.0));
print(fib(40.0));
print(fib(40.0));
print(fib(40.0));
print(fib(40.0));
print(fib(40.0));
print(fib(40.0));
print(fib(40.0));