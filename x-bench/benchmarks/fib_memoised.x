// Calculates the nth Fibonacci number using a memoised function.
memoised pure fn fib(n: f64) -> f64 {
    if (n <= 2.0) { return 1.0; }
    fib(n - 1.0) + fib(n - 2.0)
}

// The memoised version can handle a much larger number instantly.
print(fib(40.0));
print(fib(40.0));
print(fib(40.0));
print(fib(40.0));
print(fib(40.0));
print(fib(40.0));
print(fib(40.0));
print(fib(40.0));