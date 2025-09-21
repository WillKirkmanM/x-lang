// A generic struct definition
struct Pair<T, U> {
    first: T,
    second: U,
}

// A generic function definition
fn get_first<T, U>(p: Pair<T, U>) -> T {
    return p.first;
}

// Using the generic types
fn main() {
    // A pair of an integer and a boolean
    let int_bool_pair: Pair<i32, bool> = Pair{ first: 10, second: true };

    // get_first is instantiated with <i32, bool>
    let first_val: i32 = get_first(int_bool_pair);
    print(first_val);
}