let mul = |x: f64, y: f64| -> f64 { x * y };
let div = |x: f64, y: f64| -> f64 { x / y };

print_str("The result of the multiply closure is:");
let mul_result: f64 = mul(4.0, 1.0);
print(mul_result);

let div_result: f64 = div(7.0, 1.0);
print_str("The result of the divide closure is:");
print(div_result);