// Memoised pure function `six`.
// In LLVM IR, this becomes: 
// - a memoizing wrapper @six(double), implementing cache checks/miss/set
// - the actual function body is in @six.impl(double)
// - the cache is stored in the @six_cache global variable.
memoised pure fn six(x) {
    x - 1   // Implemented in @six.impl(double): loads x, subtracts 1.0, returns result
}

// Result variable initialised by calling six(8).
// In LLVM: 
// - calls @six(8.0), which does cache lookup and only computes if not already cached.
let result = six(8);
print(result);

// Print the result of six(8) again. 
// In LLVM: 
// - calls @six(8.0), this time hits the cache and loads cached value, then prints it.
print(six(8));

// Memoised pure function with no arguments.
// In LLVM IR, this maps to:
// - a memoizing wrapper @how_many() with same caching logic as `six`, backed by @how_many.impl()
// - the cache is in @how_many_cache
memoized pure fn how_many() {
    3      // Implemented in @how_many.impl(): just returns 3.0
}

// Print the result of 3) is passed to @print(double) for printing
print(how_many());
print(how_many());

// The main routine in LLVM (see @main) creates the caches for "six" and "how_many", 
// then proceeds to call, store, and print as above.
