# lion [![Build Status](https://travis-ci.org/sunjay/lion.svg?branch=master)](https://travis-ci.org/sunjay/lion)

An interactive REPL for performing calculations involving units.

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
#[unit] fn(x '_) -> 'deg { x }
#[unit] fn(x '_) -> 'rad { x }

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
#[converter] fn(x 'deg) -> 'rad { x * (pi 'rad) / (180 'deg) }
// Convert from radians to degrees (x will be in radians)
#[converter] fn(x 'rad) -> 'deg { x * (180 'deg) / (pi 'rad) }
```

TODO:
* Deal with unit prefixes (SI prefixes)
  * Idea: Way to declare prefix system
  * Automatically attempts to parse prefixes out from units and knows how to
    convert on the fly
  * Gives priority to units that are already defined before attempting to do
    inference like this
  * Units need to explicitly opt-in to prefix systems to be able to use them
  * Prefix systems need to be able to opt-in units
  * No orphans (see Rust orphan rule)
* Port [remaining units](https://github.com/sunjay/lion/blob/d90dd3089f16e091a8c8ef4735a1bd2ff5e330cd/src/prelude/units.lion)
