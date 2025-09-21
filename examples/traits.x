// Example: traits and impls

trait Printable {
    fn print(&self);
    fn describe(&self) {
        // default impl calls print
        self.print();
    }
}

struct Point {
    x: f64,
    y: f64,
}

impl Printable for Point {
    fn print(&self) {
        // This function exists so method dispatch can resolve.
    }
}

fn show_it(p: &Point) {
    p.print();
    p.describe(); // resolved via trait default synthesised for Point
}

fn main() {
    let pt: Point = Point { x: 1.0, y: 2.0 };
    show_it(&pt);
}