// Demonstrates the 'become' keyword for guaranteed tail-call optimization (TCO).
// This example calculates the sum of all integers from 1 to N.

// A public-facing function that provides a clean interface.
// It kicks off the tail-recursive process with an initial accumulator of 0.
pure fn sum(n) {
    sum_helper(n, 0)
}

// The core helper function that is actually tail-recursive.
// An "accumulator" pattern is used, where the result of the current
// step is passed as a parameter to the next recursive step.
pure fn sum_helper(n, accumulator) {
    if (n == 0) {
        // Base case: If there are no more numbers to add,
        // the result is the value stored in the accumulator.
        accumulator
    } else {
        // This is a tail call. The addition (n + accumulator) happens *before*
        // the recursive call. The 'become' keyword ensures this is compiled
        // as a jump, not a new function call, preventing stack growth.
        become sum_helper(n - 1, n + accumulator);
    }
}

print_str("Calculating sum(100000) using tail recursion...");

// This call creates 100,000 "recursive" steps.
// Without TCO from `become`, this would immediately crash with a stack overflow.
// With TCO, it runs efficiently with constant stack space.
let result = sum(100000);

print_str("Result:");
print(result); // Expected output: 5000050000