// A memoised version of the can_sum function.
// It avoids re-computing results for the same target_sum.
memoised pure fn can_sum(target_sum: f64, numbers: [f64]) -> bool {
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

// The result is computed almost instantly with the cache.
print(can_sum(7.0, [2.0, 3.0, 5.0, 7.0]));
print(can_sum(7.0, [2.0, 3.0, 5.0, 7.0]));
print(can_sum(7.0, [2.0, 3.0, 5.0, 7.0]));
print(can_sum(7.0, [2.0, 3.0, 5.0, 7.0]));
print(can_sum(7.0, [2.0, 3.0, 5.0, 7.0]));