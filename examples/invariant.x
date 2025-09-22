struct MyVec {
    len: i32,
    capacity: i32,

    // Guarantees this property holds between method calls.
    invariant {
        self.len <= self.capacity
    }
}

impl MyVec {
    // This method contains a check that is redundant due to the invariant.
    fn push(&mut self) {
        // The compiler's InvariantPass will see this condition,
        // compare it to the struct's invariant, prove it's always true,
        // and eliminate the conditional branch and the `else` block.
        if self.len <= self.capacity {
            self.len = self.len + 1;
        } else {
            // This code is now provably dead and will be removed.
            print_str("Error: Invariant violated!");
        }
    }
}

fn main() {
    let mut v = MyVec { len: 0, capacity: 10 };
    v.push(); // Call the method to trigger code generation.
}