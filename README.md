# lion

## Syntax

With type inference, must of the explicitly declared type information
in the syntax will become completely optional.

```rust
// main entry point of the program
fn main() {
    // Byte literal with type [u8; 12]
    println!(b"Hello world!");

    // Arbitrarily nested scopes
    {
        println!({
            // all declared variables must have types (for now)
            let x: i32 = f(32, 3) + 14 / 4;
            x / 4
        });
    }

    // Lists
    {
        // variables are immutable by default
        let mut stuff: [i32; 3] = [1, 2, 3];
        // Closures do not require types
        // Closures with a declared return type must have braces
        // Closures can capture from their surroundings
        // This feature may be subject to adequate type inference
        stuff = stuff.map(|a| a * 2 + stuff[0]).collect();
        println!(stuff);
    }
}

// Basic function declaration
// Top-level functions require types
// Functions declared with fn are available even to lines
// that appear above their declaration.
fn g(a: i32, b: i32) -> i32 {
    // conditional
    if a > 33 {
        // explicit early return
        return -1;
    }
    // else if and else are optional if the type is ()
    else if b == 2 {
        println!(b * 2);
    }
    else {
        println!(b"yahoo!");
    }

    // conditionals are expressions
    let q: i32 = if a > 2 { 1 } else { 3 };

    // Need for explicit types here will be removed
    // by type inference
    let k: i32 = 2;
    // Lack of semicolon means that this
    // expression returns its type instead of ()
    a * b / k
};

// Functions can be single expressions too
fn f(a: i32, b: i32, c: i32) -> i32 {
    a + b + c
};
```
