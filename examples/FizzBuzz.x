for num in 1..100 {
    if num % 3 == 0 && num % 5 == 0 {
        print("FizzBuzz")
        continue
    } else if num % 3 == 0 {
        print("Fizz")
    } else if num % 5 == 0 {
        print("Buzz")
    } else {
        print(num)
    }
}