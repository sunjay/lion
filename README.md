# lion [![Build Status](https://travis-ci.org/sunjay/lion.svg?branch=master)](https://travis-ci.org/sunjay/lion)

A programming language and interactive REPL for performing calculations
involving units.

```rust
% // Lines starting with `%` are input, `//` lines are comments
% 2 + 2
4
% // Values starting with ' are units
% // 'in = inches
% 2 'in + 2 'in
4 'in
% // Conversions are automatically performed
% 2 'cm + 4 'in
12.16 'cm
% 4 'in + 2 'cm
4.78740157 'in
% // Units can be operated on to create "compound units"
% 9.81 'm / 's ^ 2 // acceleration due to gravity
9.81 'm / 's ^ 2
% 9.81 'm*'s^-2
9.81 'm*'s^-2
% // Just the unit is the same as multiplying by the unit with value 1
% 9.81'm * 1's^-2
9.81 'm*'s^-2
% 1 'm as 'cm
100 'cm
```

We support loading in lion declaration files. For example, here is part of the
default set of declarations loaded each time you start the interpreter:

```rust
//Filename: units.lion

// Units must be declared like this before they can be used. This is useful
// for finding which file actually declared a given unit

// Units are defined by declaring an anonymous function that returns that unit
// converted from a value that explicitly has *no* unit
#[unit]
fn(x '_) -> 'deg { x }
#[unit]
fn(x '_) -> 'rad { x }

// To avoid this boilerplate, this could also have been defined:
//unit!('deg);
//unit!('rad);
// This is a macro that expands to the same thing as above

// Unit conversions must be defined explicitly. Each declaration defines the
// conversion one way.
// pi is provided as a constant
// fn(x) { foo } defines an anonymous function that takes x and returns foo
// fn(x 'deg) { foo } asserts that x has unit 'deg
// lion doesn't do much type checking since it mostly has numbers. This lets
// you have some basic assurances about the unit of data being passed to your
// function. The unit can be left off in which case any number (including
// numbers with no units) can be passed in.
// If a number with a different unit is passed in, a conversion will be
// automatically attempted using the known conversion functions.

// Convert from degrees to radians (x will be in degrees)
#[converter]
fn(x 'deg) -> 'rad { x * (pi 'rad) / (180 'deg) }
// Convert from radians to degrees (x will be in radians)
#[converter]
fn(x 'rad) -> 'deg { x * (180 'deg) / (pi 'rad) }

// Defined conversions form a DAG which is searched every time a conversion
// is needed. To avoid longer compile times, it is recommended that you keep
// the path from one unit to another as short as possible. The shortest possible
// path will always be used to perform the conversion.

// Units can take advantage of predefined prefix systems. For example, rather
// than defining m, cm, mm, nm, etc., you can just use the `#[prefix(SI)]`
// annotation. This automatically defines conversions from 'm to 'cm, 'mm, etc.
// The conversions are defined in both directions. That means that from this
// point onward, you can just use 'm to define conversions to other units and it
// will automatically work for 'cm, 'mm, 'km, etc. etc. etc.
// This annotation will warn you if you overwrite an another unit. You can
// silence this warning by using the #[overwrite('unitname)] annotation.
// The `#[overwrite('unitname)]` annotation can be used in any case where this
// warning occurs.
#[unit]
#[prefix(SI)]
//#[overwrite('mm)] // Use this to resolve conflicts
fn(x '_) -> 'm { x }
// Equivalent: unit!('m, prefix = SI)

#[unit]
// Instead of mL, this will define ml
#[prefix(SI, transform=[lowercase])]
fn(x '_) -> 'L { x }

#[unit]
// Instead of mmeter and kmeter, this will produce millimeter and kilometer
#[prefix(SI, transform=[long])]
fn(x '_) -> 'meter { x }

// All units must be a SINGLE case-sensitive identifier. You cannot for example
// define a compound unit:
// #[unit]
// fn(x '_) -> 'rad / 'sec { x } // ERROR: Cannot define a compound unit. Use conversions instead.
// You can define conversions for compound units using the individual units that
// you have defined:
#[converter]
fn(x 'm^3) -> 'L { x * 1000 }
#[converter]
fn(x 'L) -> 'm^3 { x / 1000 }

// ALIASING
// Example of creating a unit which is just an alias for another unit
alias!('kph, 'km / 'hour)
// This will generate the following code:
// #[unit]
// fn(x '_) -> 'kph {
//     // Runs the original conversion function
//     (x 'km / 'hour) 'kph
// }
// #[converter]
// fn(x 'kph) -> 'km / 'hour { x }
// fn(x 'km / 'hour) -> 'kph { x }
```

Defining custom prefix systems will **eventually** be supported. Here's an idea
of what that might look like:
```rust
prefix_system! {
    name: SI,
    conversions: {
        // prefix => conversions
        // Each conversion function will be transformed to include the full unit
        // e.g. fn(x) -> 'k { ... } will become fn(x) -> 'km { ... } when the
        //      `#[prefix(SI)]` annotation is applied to the unit 'm
        E => {
            // Convert from base unit to prefixed base unit
            // The unit listed in this type must match the one before `=>`
            fn(x) -> 'E { x / 10^18 }
            // Convert from prefixed base unit to base unit
            fn(x 'E) { x * 10^18 }
        },
        P => {
            fn(x) -> 'P { x / 10^15 }
            fn(x 'P) { x * 10^15 }
        },
        T => {
            fn(x) -> 'T { x / 10^12 }
            fn(x 'T) { x * 10^12 }
        },
        G => {
            fn(x) -> 'G { x / 10^9 }
            fn(x 'G) { x * 10^9 }
        },
        M => {
            fn(x) -> 'M { x / 10^6 }
            fn(x 'M) { x * 10^6 }
        },
        k => {
            fn(x) -> 'k { x / 10^3 }
            fn(x 'k) { x * 10^3 }
        },
        h => {
            fn(x) -> 'h { x / 10^2 }
            fn(x 'h) { x * 10^2 }
        },
        da => {
            fn(x) -> 'da { x / 10^1 }
            fn(x 'da) { x * 10^1 }
        },
        d => {
            fn(x) -> 'd { x / 10^-1 }
            fn(x 'd) { x * 10^-1 }
        },
        c => {
            fn(x) -> 'c { x / 10^-2 }
            fn(x 'c) { x * 10^-2 }
        },
        m => {
            fn(x) -> 'm { x / 10^-3 }
            fn(x 'm) { x * 10^-3 }
        },
        u => {
            fn(x) -> 'u { x / 10^-6 }
            fn(x 'u) { x * 10^-6 }
        },
        n => {
            fn(x) -> 'n { x / 10^-9 }
            fn(x 'n) { x * 10^-9 }
        },
        p => {
            fn(x) -> 'p { x / 10^-12 }
            fn(x 'p) { x * 10^-12 }
        },
        f => {
            fn(x) -> 'f { x / 10^-15 }
            fn(x 'f) { x * 10^-15 }
        },
        a => {
            fn(x) -> 'a { x / 10^-18 }
            fn(x 'a) { x * 10^-18 }
        },
    },
    // This is equivalent to using `#[prefix(SI)]` on each of these units when
    // they were defined. Having this is important because we want to be able
    // to use custom prefix systems with existing units that may have been
    // defined elsewhere.
    //TODO: Figure out how this works with `#[overwrite(...)]` and how conflicts
    // will be resolved
    units: ['m, 'L],
    //TODO: Figure out how transforms will work (e.g. lowercase, long, etc.)
    transform: {
        long: {
            E{x} => exa{x},
            P{x} => peta{x},
            T{x} => tera{x},
            G{x} => giga{x},
            M{x} => mega{x},
            k{x} => kilo{x},
            h{x} => hecto{x},
            da{x} => deca{x},
            d{x} => deci{x},
            c{x} => centi{x},
            m{x} => milli{x},
            u{x} => micro{x},
            n{x} => nano{x},
            p{x} => pico{x},
            f{x} => femto{x},
            a{x} => atto{x},
        },
        lowercase: {
            {u}{x} => {u}{lowercase(x)}
        },
    },
}
```

TODO:
* No orphan units or conversions (see Rust orphan rule)
* Port [remaining units](https://github.com/sunjay/lion/blob/d90dd3089f16e091a8c8ef4735a1bd2ff5e330cd/src/prelude/units.lion)
