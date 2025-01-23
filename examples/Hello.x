let x = 10
let y = 5

print("Arithmetic:")
print(x + y)    // Output: 15
print(x - y)    // Output: 5 
print(x * y)    // Output: 50
print(x / y)    // Output: 2

// Variables
print("Variable Assignment:")
print(test)     // Output: 5
test = 10       // Reassignment
print(test)     // Output: 10

// Control Flow - Loops
print("For Loop Example:")
let sum = 0
for i in 0..5 {
    print(i)
    if i == 3 {
        print("i is 3")
    }
    sum = sum + i
}
print("Sum:")   
print(sum)      // Output: 15

// Control Flow - Conditionals
print("Conditional Examples:")
let a = 25
let b = 10

if a < b {
    print("a is less than b")
} else if a > b {
    print("a is greater than b")    // Output: a is greater than b
}

// Operators
print("Compound Assignments:")
print("Initial:")
print(value)                       // Output: 10
value += 5                         // Add 5
print("After +=:")
print(value)                       // Output: 15
value *= 2                         // Multiply by 2
print("After *=:")
print(value)                       // Output: 30
value -= 10                        // Subtract 10
print("After -=:")
print(value)                       // Output: 20
value /= 2                         // Divide by 2
print("After /=:")
print(value)                       // Output: 10

// Modulo
print("Modulo Operation:") 
let mod_result = a % 6
print("25 % 6 =")
print(mod_result)                  // Output: 1