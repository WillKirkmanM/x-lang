fn abs(x: f64) -> f64 {
    if x < 0.0 {
        return -x;
    }
    x
}

fn sqrt_newton(n: f64) -> f64 {
    if n < 0.0 {
        return -1.0;
    }

    if n == 0.0 || n == 1.0 {
        return n;
    }

    let mut x: f64 = n / 2.0;
    let epsilon: f64 = 0.00001;

    while abs(x * x - n) > epsilon {
        let temp: f64 = x + n / x;
        x = temp / 2.0;
    }

    x
}

print_str("Square root approximations using Newton's method:");
let two = sqrt_newton(4.0);
print_str("sqrt(4) ≈ ");
print(two);

let three = sqrt_newton(10.0);
print_str("sqrt(10) ≈ ");
print(three);

let ten = sqrt_newton(100.0);
print_str("sqrt(100) ≈ ");
print(ten);