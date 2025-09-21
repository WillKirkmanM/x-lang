// Lifetime/borrow tests: &, &mut, * (deref), and escaping references

struct Vec2 {
    x: f64,
    y: f64,
}

fn returns_ref_bad<'a>() -> &'a f64 {
    // ERROR: returns a reference to a local temporary (does not live long enough)
    let v: f64 = 42.0;
    let r: &'a f64 = &v;   // borrow of local
    return r;              // borrow-checker should reject
}

fn mixed_borrows_conflict() {
    let mut v: Vec2 = Vec2 { x: 1.0, y: 2.0 };

    let rx: &f64 = &v.x;        // immutable borrow
    // ERROR: cannot borrow `v` as mutable while `rx` is live
    let mx: &mut f64 = &mut v.x;
    // print(*rx);              // would deref rx (left commented; codegen may not support yet)
}

fn lifetime_escape() {
    let r: &f64;
    {
        let tmp: f64 = 3.14;
        r = &tmp;               // borrow limited to inner scope
    }
    // ERROR: use-after-scope (reference does not live long enough)
    // print(*r);
}

fn ok_block() {
    let mut v: Vec2 = Vec2 { x: 3.0, y: 4.0 };
    let px: &mut f64 = &mut v.x;  // take a mutable reference
    // *px = 5.0;                 // deref store (commented until codegen supports deref)
    // print(*px);                // deref load (commented until codegen supports deref)
}

fn main() {
    // Uncomment one at a time to test borrow checker diagnostics:

    // returns_ref_bad();       // ERROR: returning reference to local
    // mixed_borrows_conflict();// ERROR: conflicting borrows
    // lifetime_escape();       // ERROR: reference does not live long enough
    ok_block();                 // OK if deref lines remain commented
}