fn add(x: f64, y: f64) -> f64 {
    x + y
}

fn multiply(x: f64, y: f64) -> f64 {
    x * y // Return the Product of x and y
}

fn greet() {
    let a: f64 = 3.0;
    let b: f64 = 4.0;
    let c: f64 = a + b; // Return the Sum of a and b

    print(c);

    print_str("1");
    print_str("2");
    print_str("3");

    print_str("Hello, World!");
}

greet();

for n in 7..19 {
    print(n);
}

fn test_comparisons(x: f64, y: f64) {
    if x < y {
        print_str("x is less than y");
    }
    if x <= y {
        print_str("x is less than or equal to y");
    }
    if x > y {
        print_str("x is greater than y");
    }
    if x >= y {
        print_str("x is greater than or equal to y");
    }
    if x == y {
        print_str("x is equal to y");
    }
    if x != y {
        print_str("x is not equal to y");
    }
}

test_comparisons(5.0, 10.0);
test_comparisons(10.0, 10.0);
test_comparisons(15.0, 10.0);

fn say_hello(name: str) {
    print_str("Hi {name}!");
}

say_hello("world");
say_hello("moon");

let result1: f64 = add(3.0, 4.0);
print_str("The result of 3 + 4 is: {result1}.");
print(result1);

let result2: f64 = multiply(3.0, 4.0);
print_str("The result of 3 * 4 is:");
print(result2);

let mul = |x: f64, y: f64| -> f64 { x * y };
let div = |x: f64, y: f64| -> f64 { x / y };

print_str("The result of the multiply closure is:");
let mul_result: f64 = mul(4.0, 1.0);
print(mul_result);

print_str("The result of the divide closure is:");
let div_result: f64 = div(7.0, 1.0);
print(div_result);

let arr: [f64] = [1.0, 2.0, 3.0, 4.0, 5.0];

let two: f64 = arr[1];
print(two);

let mut i: i32 = 0;
while i < 5 {
    print(i);
    i = i + 1;
}

struct Point {
    x: f64,
    y: f64,
}

let mut p = Point { x: 10.0, y: 20.0 };
print_str("Point coordinates:");
print(p.x);
print(p.y);

p.x = 30.0;
print_str("Updated X coordinate:");
print(p.x);
