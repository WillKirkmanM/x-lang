fn main() {
    let mut data: i32[1024];

    // The 'parallel' keyword is a hint to the compiler that each iteration
    // of this for-loop is independent and can be executed concurrently
    // on different CPU cores.
    parallel for i in 0..1024 {
        // This calculation is "embarrassingly parallel" because the result
        // for data[i] does not depend on the result of any other iteration.
        data[i] = i * 2;
    }

    // A standard, sequential loop to print the first 10 results
    // to verify that the parallel computation was successful.
    for i in 0..10 {
        print(data[i]);
    }
}