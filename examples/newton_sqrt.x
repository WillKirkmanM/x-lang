import std::print;

fn abs(x) {
    if x < 0 {
        return -x;
    }
    return x;
}

fn sqrt_newton(n) {
    if n < 0 {
        return -1;
    }
    
    if n == 0 || n == 1 {
        return n;
    }
    
    let x = n / 2;
    let epsilon = 0.00001; 
    
    while abs(x * x - n) > epsilon {
        let temp = x + n / x;
        x = (temp / 2);
    }
    
    x;
}

print("Square root approximations using Newton's method:");
let two = sqrt_newton(4);
print("sqrt(4) ≈ {two}");

let three = sqrt_newton(10);
print("sqrt(10) ≈ {three}");

let ten = sqrt_newton(100);
print("sqrt(100) ≈ {ten}");