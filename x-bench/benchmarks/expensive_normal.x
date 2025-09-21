// A standard recursive function to find the number of paths in a grid.
// Its complexity is exponential, so it will be very slow for large grids.

// Time complexity: exponential — roughly O(2^(m + n))
// (more precisely Θ(binomial(m + n, m)) for the number of recursive calls)
// Space complexity: O(m + n) stack depth due to recursion.
pure fn grid_traveler(m: f64, n: f64) -> f64 {
    if (m == 1.0 && n == 1.0) { return 1.0; }
    if (m == 0.0 || n == 0.0) { return 0.0; }

    grid_traveler(m - 1.0, n) + grid_traveler(m, n - 1.0)
}

// With a 14x14 grid, this will take a significant amount of time.
print(grid_traveler(14.0, 14.0));