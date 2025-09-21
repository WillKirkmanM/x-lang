// Checks if the target sum can be achieved using the numbers.
// This version is inefficient due to re-calculating the same branches.
pure fn can_sum(target_sum: f64, numbers: [f64]) -> bool {
    if (target_sum == 0.0) { return true; }
    if (target_sum < 0.0) { return false; }

    for num in numbers {
        let remainder = target_sum - num;
        if (can_sum(remainder, numbers) == true) {
            return true;
        }
    }

    return false;
}

// A moderately difficult problem that will take time without memoisation.
print(can_sum(7.0, [2.0, 3.0, 5.0, 7.0]));
print(can_sum(7.0, [2.0, 3.0, 5.0, 7.0]));
print(can_sum(7.0, [2.0, 3.0, 5.0, 7.0]));
print(can_sum(7.0, [2.0, 3.0, 5.0, 7.0]));
print(can_sum(7.0, [2.0, 3.0, 5.0, 7.0]));