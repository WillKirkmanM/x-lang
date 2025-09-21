// 1. Define a struct. Since this is not a primitive,
//    the borrow checker will treat it as a "move" type.
struct Vector {
    x: f64,
    y: f64
}

// 2. This function takes ownership of its argument `v`.
//    When called, the argument passed to it will be moved.
fn calculate_length(v: Vector) -> f64 {
    // (calculation would go here)
    return 0.0;
}

fn main() {
    // 3. Create an instance of the non-copy type.
    let v1: Vector = Vector { x: 3.0, y: 4.0 };

    // 4. Pass `v1` to the function.
    //    This MOVES ownership of the data from `v1` into the function.
    calculate_length(v1);

    // 5. Attempt to use `v1` again after it has been moved.
    //    This line will cause the borrow checker to produce an error.
    print(v1.x); // ERROR: Use-after-move of variable 'v1'
}