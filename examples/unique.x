struct Point { x: i32, y: i32 }

// The `unique` keyword guarantees that `p` is the only pointer
// to this Point instance within the scope of this function.
fn move_point(p: &unique mut Point, dx: i32, dy: i32) {
    p.x = p.x + dx;
    p.y = p.y + dy;
}

fn main() {
    let mut my_point = Point { x: 10, y: 20 };
    move_point(&mut my_point, 5, -5); 
    // The call would pass a reference that the compiler can treat as `noalias`.
}