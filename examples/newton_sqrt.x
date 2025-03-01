import std::print;

fn abs(x) {
    if x < 0 {
        return -x;
    }
    x;
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
        x = (x + n / x) / 2;
    }
    
    x;
}

print("Square root approximations using Newton's method:");
print("sqrt(4) ≈");
print(sqrt_newton(4));

print("sqrt(10) ≈");
print(sqrt_newton(10));

print("sqrt(100) ≈");
print(sqrt_newton(100));