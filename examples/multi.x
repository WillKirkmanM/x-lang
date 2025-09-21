// Multiple dispatch example with two-argument multi functions.
// Note: the compiler currently requires the first two parameters of `multi`
// functions to be references (&T / &mut T) to obtain type IDs at runtime.

struct Circle { r: f64 }
struct Rect { w: f64, h: f64 }

multi fn collide(a: &Circle, b: &Circle) -> bool {
    true
}

multi fn collide(a: &Circle, b: &Rect) -> bool {
    false
}

multi fn collide(a: &Rect, b: &Circle) -> bool {
    false
}

multi fn collide(a: &Rect, b: &Rect) -> bool {
    true
}

fn main() {
    let c1: Circle = Circle { r: 1.0 };
    let c2: Circle = Circle { r: 2.0 };
    let re: Rect = Rect { w: 2.0, h: 3.0 };

    // Resolve different combinations via the multi resolver
    let cc: bool = collide(&c1, &c2);
    let cr: bool = collide(&c1, &re);
    let rc: bool = collide(&re, &c2);
    let rr: bool = collide(&re, &re);

    // print_bool(cc);
    // print_bool(cr);
    // print_bool(rc);
    // print_bool(rr);
}