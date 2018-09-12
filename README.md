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
% 9.81 'm / 's^2 // acceleration due to gravity
9.81 'm / 's ^ 2
% 9.81 'm * 's^-2 // Equivalent to 9.81 'm / 's ^ 2, but printed as written
9.81 'm 's ^ -2
% // Just the unit is the same as multiplying by the unit with value 1
% (9.81'm) * (1's^-2) // Same as 9.81 'm * 's ^ -2
9.81 'm 's ^ -2
% // Adjacent units are automatically grouped together as one compound unit
% 9.81 'm 's^-2
9.81 'm 's ^ -2
% // Explicit conversions are possible with `as`
% 1 'm as 'cm
100 'cm
% // Commutative multiplication is recognized and unified in units
% 10 'm * 's + 20 's * 'm
30 'm * 's
% // Division is not commutative, so the following will not do the same thing
% 10 'm / 's + 20 's / 'm
Error: Cannot convert from 'm / 's to 's / 'm
```

## Features

* Units in variables and functions are inferred when necessary and checked to
  make sure each operation is mathematically sound
  ```rust
  // Here we are annotating the unit of the parameter radius and the unit
  // of the return type
  fn circle_area(radius 'm) -> 'm^2 {
      // If this unit was not provided, it would be inferred from the simplified
      // unit of the expression on the right hand side. Since it is provided,
      // lion will check to make sure the right hand side results in something
      // that is convertable to the declared unit.
      let square_radius 'm ^ 2 = radius ^ 2;
      pi * square_radius
  }
  ```
* Conversions between units are performed automatically and implicitly
  ```rust
  % fn foo(x 'km) {}
  % // This is automatically converted into 'km so it can be passed to the function
  % foo(10 'm);
  % // This function only accepts values without a unit
  % fn bar(x) {}
  % bar(10); // OK
  % bar(10 'm);
  Error: Cannot convert from 'm to '_
  % // This also works in expressions: 2 'km is converted to 'm
  % 10 'm * 2 'km
  20000 'm ^ 2
  ```
* Units are automatically simplified, or coerced into the unit you provide if
  you provide one explicitly
  ```rust
  % 10 'km / 'm
  10000
  % 10 as 'm / 'm
  10 'm / 'm
  % let x 'm / 'm = 10;
  % x
  10 'm / 'm
  ```
* Unit conversions are mathematically valid
  ```rust
  % // This is invalid because you cannot convert between unit dimensions
  % 10 as 'm / 'm ^ 2
  Error: Cannot convert from '_ to 'm / 'm ^ 2
  % // This is still okay because we aren't converting anything
  % // Notice how the unit gets simplified since we haven't explicitly specified
  % // the target unit
  % 10 'm / 'm^2
  10 'm ^ -1
  ```

## Declaration Files

Lion declaration files allow you to declare units, conversions, functions and
useful constants. For example, here is part of the default set of declarations
loaded each time you start the interpreter:

```rust
//Filename: units.lion

// Import this file using a use statement:
//     use units;
// This will import the file `units.lion` from the current directory
//
// Specify the full path using:
//     use "./units.lion";
//
// Specify explicit unit overrides with:
//     use units override ('mm, 'cm);
// This would suppress any warnings about duplicate declarations for 'mm and 'cm

// Declaring a public (usable outside of this module) constant value called `pi`
// that has the unit '_
//
// The unit '_ is special in that it represents a quantity that is unitless
pub const pi '_ = 3.1415926535897932384626433832;
// We could have also written this as follows since '_ is optional
//pub const pi = 3.1415926535897932384626433832;

// Units are defined using the `unit` keyword
unit 'deg;
unit 'rad;

// Units do not need to appear in the file before they are used, but in order to
// use a unit, it must be declared *somewhere*. Units don't even need to be
// declared in the same file they are used in. As long as the units appear in
// some imported file or in some imported package, you are good to go.

// All units belong to the same global scope. Multiple declarations of the same
// unit are NOT allowed because someone else defining the same unit may make
// different assumptions about which conversions are defined than someone else.
// If someone defines the same unit as someone else, you *cannot* use both of
// their packages (or files if using path imports).

// Unit conversions are defined by explicitly declaring the conversion factor
// between two units. This representation permits lion to do dimensional
// analysis in order to determine how to convert between units.
//
// Note that this defines the conversion from degrees to radians AND the
// conversion from radians to degrees
conversion 180 'deg == pi 'rad;

// Duplicate conversions that result in multiple potential conversion factors
// between two units are not allowed.
//
// conversion 1000 'm == 1 'km; // (1)
// conversion 1000 'm == 1 'km; // Not allowed, duplicate of (1)
// conversion 1001 'm == 1 'km; // Also not allowed, conflicts with (1)
//
// Rules about conversions:
// * The units on both sides must be distinct
// * At least one of the units in a defined conversion factor must be
//   locally defined in the current package (or file if using path imports)
//   * You cannot define conversions for units you do not define
// * Within the same package (or file if using path imports), a conversion is
//   only allowed to be defined once for a given unordered pair of units
// * If more than one package defines a conversion between an unordered pair
//   of units, the defined conversion factors must be equivalent
//   Example:
//       conversion 1000 'm == 1 'km; // In package A that defines 'm
//       conversion 1 'km == 1000 'm; // In package B that defines 'km
//       // Order does not matter
//       conversion 1000 'm == 1 'km; // Package B could also do this
// * The conversion cannot be between two different dimensions of the same unit
//   * This is not a rigorous check. `'m == 'm^2` will be rejected, but it is
//     up to you to make sure that `'L == 'm^2` is not accidentally defined
// * The unit of the expression on either side cannot be '_ since a conversion
//   between dimensions of units is invalid and '_ has dimension zero
// * The expression on either side of the conversion cannot evaluate to zero
//   since this can result in divide by zero errors
// * Furthermore, if more than one conversion path exists between two units,
//   then the cumulative conversion factor must be the same across all possible
//   paths between those two units.
//
//   Example:
//       conversion 1 'a == 2 'b;
//       conversion 1 'b == 3 'c;
//       conversion 1 'a == 6 'c; // OK because 2 * 3 = 6

// Units can take advantage of predefined prefix systems. For example, rather
// than defining m, cm, mm, nm, etc., you can just use the `#[prefix(SI)]`
// annotation. This automatically defines conversion factors for 'm to 'cm,
// 'm to 'mm, etc. Since conversion factors allow for converting in both
// directions, you can just use 'm to define conversions to other units and all
// of those conversions will automatically work for 'cm, 'mm, 'km, etc.
//
// An error will be produced if one of the generated units conflicts with an
// already declared unit.

// This will define 'm, 'mm, 'cm, etc.
#[prefix(SI)]
unit 'm;

// Instead of 'mL, this will define 'ml, 'kl, etc.
// transform takes a list of filters to apply
#[prefix(SI, transform=[lowercase])]
unit 'L;

// Instead of mmeter and kmeter, this will produce millimeter and kilometer
#[prefix(SI, transform=[longprefix])]
unit 'meter;

// All units must be a SINGLE case-sensitive identifier. You cannot for example
// define a compound unit:
//
// unit 'rad / 'sec; // ERROR: Cannot define a compound unit. Use conversions instead.

// You can define conversions for compound units using the individual units that
// you have defined. Note that the rules about conversions above must be
// followed and are enforced when conversions are defined, not just when
// conversions are used.
conversion 1 'm^3 == 1000 'L;

// ALIASING

// This is syntactic sugar for both defining a unit and a 1:1 conversion between
// that unit and another (possibly compound) unit
//
// Any attributes like #[prefix(...)] will be passed on to the unit declaration
unit 'kph alias 'km / 'hour;

// This will generate the following code:
//
// unit 'kph;
// conversion 1 'kph == 1 'km / 'hour;
```

Defining custom prefix systems will **eventually** be supported. Here's an idea
of what that might look like:

```rust
prefix_system! {
    name: SI,
    conversions: {
        // If the prefix system was applied to 'm, this would generate:
        //
        // conversion 10^24 'm == 1 'Ym
        // conversion 10^21 'm == 1 'Zm
        // ...etc...

        // Format: prefix => conversion factor
        Y => 10^24,
        Z => 10^21,
        E => 10^18,
        P => 10^15,
        T => 10^12,
        G => 10^9,
        M => 10^6,
        k => 10^3,
        h => 10^2,
        da => 10^1,
        d => 10^-1,
        c => 10^-2,
        m => 10^-3,
        u => 10^-6,
        n => 10^-9,
        p => 10^-12,
        f => 10^-15,
        a => 10^-18,
        z => 10^-21,
        y => 10^-24,
    },
    // This is equivalent to using `#[prefix(SI)]` on each of these units when
    // they were defined. Having this is important because we want to be able
    // to use custom prefix systems with existing units that may have been
    // defined elsewhere.
    // This will fail to compile if these units already define a unit that
    // has the same name as a unit generated by this prefix system
    units: ['m, 'L],
    //TODO: Figure out how transforms will work
    transform: {
        // Defines the long prefixes, the actual transform is done by some
        // internal code
        longprefix: {
            // Format: prefix => long prefix
            Y => yotta,
            Z => zetta,
            E => exa,
            P => peta,
            T => tera,
            G => giga,
            M => mega,
            k => kilo,
            h => hecto,
            da => deca,
            d => deci,
            c => centi,
            m => milli,
            u => micro,
            n => nano,
            p => pico,
            f => femto,
            a => atto,
            z => zepto,
            y => yocto,
        },
    },
}
```

TODO:
* Port [remaining units](https://github.com/sunjay/lion/blob/d90dd3089f16e091a8c8ef4735a1bd2ff5e330cd/src/prelude/units.lion)
