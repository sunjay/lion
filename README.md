# lion [![Build Status](https://travis-ci.org/sunjay/lion.svg?branch=master)](https://travis-ci.org/sunjay/lion)

A very tiny, dynamic, loosely-typed language for doing calculations in a more intuitive way.

- [x] scanner
- [x] lexer/tokenizer
- [x] parser
- [x] ast to evaluation tree
- [x] evaluator (&beta; reducer)
- [ ] prelude
    - [x] math
    - [ ] units
- [ ] simple cli (repl)
- [ ] version 1.0.0
- [ ] further enhancements

## Examples

```
$ 2 + 2
= 4
$ 20 cm + 2 m
= 220 cm
$ (20 cm + 2 m) m
= 2.2 m
$ add2 x = x + 2
$ add2 5
= 7
$ add2cm x = x + 2 cm
$ add2cm (5 cm)
= 7 cm
```

## Design Goals

* Define most things in the language itself rather than on a compiler level
* Support prefix, postfix and infix operators -- functions, units and operators
* Be lazy, not eager -- but still provide a way to force strict evaluation (still under consideration, since lazy probably requires immutable too)
* **Do not write another programming language.** Keep it simple.

## Implementation Goals:
* Be able to generate parse trees and abstract syntax trees given raw text source
* Be able to evaluate raw text source from the generated trees
    * Be able to generate good error messages with line and character numbers
* Be able to compile programs into bytecode
* Support basic scope / stack

## Version 1.0.0 Notes
* In order to speed up the process of making this, the first version will be very simple and only support numeric types
* Numbers will be represented by a value as well as a unit
* Vectors and other fancier types will not be included until future versions
* Only simple expressions as found in the examples will be included in this initial release

## Possible Future Extensions:
* Pattern matching on functions
  * Unwrapping
  * Supporting variable numbers of arguments
  * Matching units
* Supporting default values and multiple function signatures
* Defining the supported input and output types of a function (useful for adding Vectors and stuff while still keeping addition operators, etc.)
  * Adding types should be an all or nothing process, either you have them on every parameter and the output or you do not have them at all
* Language extensions:
  * Defined outside of the runtime in order to add some feature or type to the language using some sort of plugin or middleware API
  * Vectors
  * Arrays, operations on arrays
  * Strings, operations on strings as arrays of characters
  * I/O
  * Graphing
* Importing and modules - being able to add defintions defined in a separate module somewhere (split circuits from basic math, etc.)
* Other numerical bases + numeric literals for those bases (base 16, etc.)

## Syntax

* Functions:
  * Basic syntax for defining a name and some parameters
  `<function_name> <param1> <param2> = <expression>`
  * Anonymous functions can omit the function name and use a backslash instead (when using a backslash, the space after the name is not necessary)
  `\<param1> <param2> = <expression>`
  * Function expressions return the defined function

* Expressions:
  * Composed of symbols and evaluated at runtime based on the definitions of those symbols (precedence, etc.)
  * [ADVANCED] Can be enclosed within a block `{ <expressions> }` where the result of the expression is the final line in the block

* Strings: (but no string operators...yet)
  * Double-quotes only for strings, backslash escapes for double quotes within strings

* Default operators:
  * Definitions are included in prelude for the following basic operators:
    * `+`
    * `-`
    * `*`
    * `/`
    * `**`
    * `==`
    * `!=`
    * `>=`
    * `<=`
    * `neg` - for negating numbers (since `-` is infix)
    * Math functions such as `max`, `min`, `sin`, `cos`, `tan`, etc.

## Custom Operators
All the basic operators will be defined in lion syntax in a special prelude module. 

Defining an operator is as simple as calling the `operator` function and passing in its three parameters:

1. The fixity of your operator, one of: `PREFIX`, `POSTFIX`, `INFIX`
2. The precedence, a number from 0 to 9 where 9 is the highest precedence
3. The name of the operator in double-quotes (example: `"$$"`)
4. A function (can be anonymous) representing the relationship between your operator's input and your operator's output
  * `PREFIX` and `POSTFIX` operators take one parameter
  * `INFIX` operators take two parameters
  * The names of the function parameters do not matter

Function declaration syntax is syntatic sugar for defining a `PREFIX` operator with high precedence.

TODO: Everything is left-associative right now, this may be changed in later versions to be configurable

### Examples
```
$ operator INFIX 6 "$$" (\x y = x * y + x)
$ 3 $$ 2
= 9
$ f x = 2 * x
$ operator POSTFIX 6 "timestwo" (f)
$ 2.5 timestwo
= 5
$ (2.5 cm) timestwo
= 5 cm
```

## Units
Units are simply `POSTFIX` operators with a very high precedence. This section describes some useful functions introduced to make working with units easier.

Units can have any casing, but to avoid confusion it is recommended to always use proper SI prefixes (or whatever other system you are following) whenever possible.

The `defineUnit` function takes a case-sensitive string (use double-quotes) representing the unit's name. This defines a tightly-bound (high precedence) `POSTFIX` function which when used, attempts to convert its argument to the now defined unit based on any conversions that are currently defined. 

```
$ defineUnit "kohm"
$ 3 kohm
= 3 kohm
```

All values with no unit are defined to have a special unit called `units`. The conversion to this unit simply removes whatever unit a value already has. To convert from this unit to another one, the new unit just replaces `units` as the unit for this value.

`defineUnit` takes care of defining this simple conversion for you and immediately makes your unit available for use in simple conversions to and from `units`.

The now defined unit function can be used as an argument in the `convert` function. The `convert` function is what is used behind the scenes to convert from the current unit of the quantity to a new unit.

```
# By wrapping the `cm` function in parentheses, it is not evaluated
$ convert (2 units) (cm)
```

Conversions are automatically performed during calculations. The units are combined such that the leftmost unit is preserved. Incompatible units (where no conversion can be resolved) are left untouched since they cannot be reduced any further.

```
$ 20 cm + 2 m
= 220 cm
$ 20 cm + 2 m + 2 kohm
= 220 cm + 2 kohm
```

You can explicitly convert to a unit by calling its function:

```
$ (20 cm + 2 m) m
= 2.2 m
```

This is equivalent to:

```
$ convert (20 cm + 2 m) "m"
= 2.2 m
```

This will only work if a conversion from `cm` to `m` is defined. The `prelude` module loaded with each program session has many useful units and conversions already built in.

### Defining Conversions
To define a conversion between two units, use the `conversion` function.

The `conversion` function takes the two units to convert from and to as well as a function to convert from the first argument to the second one. This conversion definition is **one way**. No equivalence is automatically defined.

```
$ conversion "cm" "m" (\x = x / 100)
```

## Implementation Notes
Essentially does beta reduction on any given expression. Since the language is lazy, many advanced expressions can be supported without explicitly requiring lambdas everywhere (see `if` below). Everything, including unit conversions must be lazy since many unit conversions can often be ruled out.

Certain functions will be defined on a "language level". Language level just means that these are immutable functions available at runtime.

Examples of lanaguage level functions:
* `operator` - for defining custom operators (See Custom Operators)
* `defineUnit` - for defining a new unit (See Units), does not error if the unit already exists, creates a `POSTFIX` function with the given name that attempts to resolve the given parameter to this unit
* `convert` - for giving some quantity a new unit, performs a conversion
* `unitFor` - retrieves the unit for some quantity
* `valueOf` - retrieves the raw value (without a unit) for a given quantity, technically just performs a conversion to the unit `units`
* `conversion` - defines a conversion from one unit to another, overwrites any existing conversion (See Defining Conversions)
* `if` - evaluates a given boolean expression and then returns one of its arguments based on the result
  * `if <expr> <if_true> <if_false>` results in one of the expressions based on the result of expr
  * Fairly flexible because of how blocks work
  * Both parameters are required


TODO: Update this file before release


