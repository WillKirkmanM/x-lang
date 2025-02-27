use std::process::Command;

use inkwell::context::Context;
use x_codegen::CodeGen;
use x_parser::parse;

fn main() {
    let input = r#"
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

    // hi
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

    "#;
    let program = match parse(input) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            return;
        }
    };

    println!("AST: {:?}", program);

    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "main");

    match codegen.generate(program.clone()) {
        Ok(_) => {
            println!("Generated LLVM IR:");
            println!("{}", codegen.get_ir());

            match codegen.jit_execute() {
                Ok(_result) => (),
                Err(e) => {
                    eprintln!("Execution error: {}", e);
                }
            }
        }
        Err(e) => eprintln!("Codegen error: {}", e),
    }

    // match codegen.generate(program) {
    //     Ok(_) => {
    //         println!("Generated LLVM IR:");
    //         println!("{}", codegen.get_ir());

    //         std::fs::write("output.ll", codegen.get_ir()).expect("Failed to write IR");

    //         Command::new("llc")
    //             .args([
    //                 "-opaque-pointers",
    //                 "-filetype=obj",
    //                 "-relocation-model=pic",
    //                 "output.ll",
    //                 "-o",
    //                 "output.o"
    //             ])
    //             .status()
    //             .expect("Failed to run llc");

    //         Command::new("clang")
    //             .args([
    //                 "output.o",
    //                 "-o",
    //                 "program",
    //                 "-fPIE",
    //                 "-pie",
    //                 "-lm"
    //             ])
    //             .status()
    //             .expect("Failed to link");

    //         std::fs::remove_file("output.ll").ok();
    //         std::fs::remove_file("output.o").ok();
    //     },
    //     Err(e) => eprintln!("Codegen error: {}", e)
    // }
}
