// The exact same algorithm, but with the `memoised` keyword.
// The compiler will automatically cache the results of subproblems.
memoised pure fn grid_traveler(m: f64, n: f64) -> f64 {
    if (m == 1.0 && n == 1.0) { return 1.0; }
    if (m == 0.0 || n == 0.0) { return 0.0; }

    grid_traveler(m - 1.0, n) + grid_traveler(m, n - 1.0)
}

// This will be orders of magnitude faster than the normal version.
print(grid_traveler(14.0, 14.0));