import std::print;

fn add(x, y) {
    x + y;
}

fn multiply(x, y) {
    x * y; // Return the Product of x and y
}

fn greet() {
    let a = 3;
    let b = 4;
    let c = a + b; // Return the Sum of a and b

    print(c);

    print("1");
    print("2");
    print("3");

    print("Hello, World!");
}

greet();

for n in 7..19 {
    print(n);
}

fn test_comparisons(x, y) {
    if x < y {
        print("x is less than y");
    }
    if x <= y {
        print("x is less than or equal to y");
    }
    if x > y {
        print("x is greater than y");
    }
    if x >= y {
        print("x is greater than or equal to y");
    }
    if x == y {
        print("x is equal to y");
    }
    if x != y {
        print("x is not equal to y");
    }
}

test_comparisons(5, 10);
test_comparisons(10, 10);
test_comparisons(15, 10);

fn say_hello(name) {
    print("Hi {name}!");
}

say_hello("world");
say_hello("moon");

let result1 = add(3, 4);
print("The result of 3 + 4 is: {result1}.");
print(result1);

let result2 = multiply(3, 4);
print("The result of 3 * 4 is:");
print(result2);

let mul = |x, y| { x * y };
let div = |x, y| { x / y };

print("The result of the multiply closure is:");
print(mul(4, 1));

print("The result of the divide closure is:");
print(div(7, 1));

let arr = [1, 2, 3, 4, 5];

let two = arr[1];
print(two);

let i = 0;
while i < 5 {
    print(i);
    i = i + 1;
}

struct Point {
    x,
    y
}

let p = Point{ x: 10.0, y: 20.0 };
print("Point coordinates:");
print(p.x);
print(p.y);

p.x = 30.0;
print("Updated X coordinate:");
print(p.x);
