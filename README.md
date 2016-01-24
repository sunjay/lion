GOALS: Language Features
------------------------

* Basic Types:
    * Functions: (args) => {body; return result}
    * Variables: variable_name = value
    * Operators: prefix, postfix, infix
    * Boolean: true, false
* Split multiple statements with semicolon (;)
* Ability to define operators as functions with precedence
* Quantities can have a unit or be dimensionless (a unit of `unit`)
* Comments begin with `#`
* Functions with default arguments
* Valid characters for variable name/operator: `a-zA-Z$_^&*!@%+?<>.:/\|~'"\``
* Ability to create JavaScript modules that tap into the interpreter
* Basic definitions written in the language loaded initially (like Prelude)
* Functions define closures
* Numbers are simply symbols that can be reduced like anything else (same with true and false)
* Short circuit evaluation
* Exceptions thrown using special functions (`error(message)`, `assert(boolean)`, `in_range(value, start, end)`, etc.)

Supported Expressions:
----------------------

    $ # simple arithmetic
    $ 2 + 2 * 3
    = 8
    $ # parenthesis change operator precedence
    $ # fractions are automatically preserved
    $ 2 * (3 + 2) / 4
    = 5 / 2
    $ # force fraction evaluation with the ! postfix operator
    $ (5 / 2)!
    = 2.5
    $ # defining a function
    $ # functions automatically get the highest precedence as do most symbols or numbers
    $ f = (x) => 2 * x
    $ # functions are simply prefix operators
    $ # evaluating with a defined value
    $ f 3
    = 6
    $ # evaluating with an undefined value
    $ f x
    = (x) => 2 * x
    $ f y
    = (y) => 2 * y
    $ # defining an operator
    $ # POSTFIX is simply a variable
    $ operator POSTFIX 7 doubled f
    $ 7 doubled
    = 14
    $ # infix operators have to have two parameters
    $ # functions can be defined in the operator definition too
    $ # second parameter after operator type is precedence between 0 and 9
    $ operator INFIX 7 $$ ((x, y) => x + y * x)
    $ 5 $$ 8
    = 45
    $ # postfix operators and prefix operators can have any number > 0
    $ q = (x, y, z) => {
        w = x * y
        return w + z * w
    }
    $ q 1 2 3
    = 8
    $ operator POSTFIX 7 %^hi q
    $ 1 2 3 %^hi
    = 8
    $ # use `operator` to reassign an existing prefix operator's precedence
    $ # prefix operator and function are synonymous
    $ r = (t) => t * 7
    $ r 7 + 2
    = 51
    $ # this is usually not recommended unless you absolutely need it
    $ operator PREFIX 1 f f
    $ # now + will bind higher than f
    $ r 7 + 2
    = 63
    $ if/else are functions
    $ a = 2
    $ # always requires all arguments
    $ if (a == 2) (() => 3) (() => 4)
    3



