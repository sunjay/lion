# lion

A minimal and highly flexible language designed to make calculations
readable and concise.

Design Goals
------------
* Have a very minimal set of language features (functions, symbols,
    etc. -- that's it) that act as building blocks for other
    language features
* Be flexible enough so that most things can be defined in the language
    itself and then additional functionality can be implemented using
    extension modules that "hook in" using middleware
* Allow for prefix, postfix and infix function evaluation -- thus supporting
    functions, units and operators in a form that is familiar
* Make purity easy and natural without making it an absolute requirement
* Do not create another programming language. Write a language for evaluating
    expressions as collections of functions in a concise and dynamic manner.

Language Features
-----------------

* Basic Types:
    * Functions: (args) => {body; return result}
    * Variables: variableName = value
    * Operators: prefix, postfix, infix
    * Boolean: true = 1, false = 0
* Split multiple statements with semicolon (;)
* Ability to define operators as functions with precedence
* Quantities can have a unit or be dimensionless (a unit of `unit`)
* Comments begin with `#`
* Functions with default arguments
* Valid characters for variable name/operator:
    `a-zA-Z0-9$_^&*!@%+?<>.:/\|~'"\``
* Ability to create JavaScript modules that tap into the interpreter
    * `middleware` hooks into various things such as finding undefined variables (could be used to define numbers, constants, etc.)
    * Side-effects of this: you could potentially do things like
        define `2 = 9` if you want...might be dangerous (but also 
        extremely powerful)
    * If middleware is done right, it could enable extensions to provide
        additional functionality like `const` function or an `override` 
        function for storing whether certain symbols are constant 
        or variable and then overriding that information when necessary
* Basic definitions written in the language loaded initially (like Prelude)
* Functions define closures
* Numbers are simply symbols that can be reduced like anything else (same with true and false)
* Short circuit evaluation
* Exceptions thrown using special functions (`error(message)`, `assert(boolean)`, `assertInRange(value, start, end)`, etc.)
* Anything that isn't defined is automatically a special Symbol type (useful for isinstance checking)
* Lazy by default (kind of) (makes `if` easier to implement)
* `import` is just a function that hooks into the interpreter to dynamically
    load and evaluate some file (usually as JavaScript) -- there should be
    some middleware for this too.
* Possible extensions:
    * Varadic parameters (i.e. (...args) => args) - requires support for arrays
    * Basic array methods for acting on ranges: `map`, `foldl`, `foldr`, `range`, `flatten`

TODO:
* Some more work needs to be done to make things truly lazy. For example,
    in the evaluation model, function arguments are evaluated as soon as
    the function is invoked. Instead of that, function arguments should
    only be invoked when they are substituted for their place in the
    function body. Additionally, evaluation should not be entirely depth
    first. It should begin at the top, attempt to evaluate that, and then
    continuously evaluate whatever bare minimum number of symbols is
    necessary for that evaluation to occur. Tokens that are not symbols
    should always be evaluated (turned into a tree) right away as they
    have syntactic meaning, not semantic meaning.

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

Control Flow
------------

    $ if/else are functions
    $ a = 2
    $ # always requires all arguments
    $ if (a == 2) (() => 3) (() => 4)
    = 3
    $ # multiple lines
    $ if (a == 3) (() => {
        return 17
    }) (() => {
        return 21
    })
    = 21

Note that certain expressions aren't possible because the flexibility of
this syntax makes them ambiguous:

    4*x
    x*2

Cannot be parsed since it is impossible to know whether it is multiplication
or another operator of some kind.

Dimensionless Quantities & Units
--------------------------------

Units are subcategories of certain types. Operators using these units are
defined at the type level. For example, the Number type can have units like
`cm`, `inch`, `metre`, etc. Unit names can be any of the valid characters.
That means that units like `cm^2` are possible (note that there can be no
spaces between `cm` and `^2`).

Examples of usage:

    $ 4
    = 4
    $ # the unit for something is simply an arbitrary string constant defined in the JavaScript
    $ unitFor 4
    = units
    $ # by convention, this string is also usually a postfix function used to convert other values into that unit
    $ 4 units
    = 4
    $ 4 cm
    = 4 cm
    $ # valueOf returns the dimensionless version of a quantity
    $ # internally, this is a conversion from that unit to `units`
    $ valueOf (4 cm)
    = 4
    $ # unit functions should have the highest precedence binding so that expressions work as expected
    $ 4 cm + 8 cm
    = 12 cm
    $ # you can set a quantity to a specific unit using that unit's constant
    $ # by convention, this is just the uppercase name of that unit
    $ # internally, this is a conversion from `units` to that unit
    $ transform 4 CM
    = 4 cm
    $ # alternatively, you can use the special transform operator
    $ 4 -> cm
    = 4 cm
    $ # this applies to any unit transformation
    $ 3 m -> CM
    = 300 cm

When defining a type, care should be taken to account for both dimensionless
quantities and quantities with units. A dimensionless quantity is simple a
quantity with the unit `units`.

Custom transformations between units can be defined using the special
defineTransformation function.

    $ defineTransformation CM M ((x) => x * 100)

### Defining custom units
In order to expose enough so that units can be used as defined above,
the following minimal complete definition should be made for each custom
unit.

    $ # This automatically creates an uppercase constant for that unit
    $ # This will produce an error if the uppercase constant clashes with
    $ # the name of that unit (i.e. uppercase of $@ is still $@) 
    $ # So make there is at least one letter.
    $ # This will automatically create a transformation from UNITS to the
    $ # newly defined constant that simply sets the unit on the value 
    $ defineUnit cm

Note that this **does not** allow you to redefine existing units. To do that,
you must first explicitly remove them using `undefineUnit`.

    $ # automatically removes unit and constant from lookup table
    $ undefineUnit CM

Trying to redefine an existing unit without undefining it first will result
in an error.

Language Implementation
-----------------------
Internally, the language is implemented such that everything is tokenized
and then those tokens are dynamically interpreted at runtime.

For example, let's say you have this function:

    f = (x) => 23 + x * 4

This might be tokenized as follows:

    SYMBOL(f) EQUALS PARENOPEN SYMBOL(x) PARENCLOSE ARROW SYMBOL(23) SYMBOL(+) SYMBOL(x) SYMBOL(*) SYMBOL(4)

Note how mostly there are symbols. These are what will be dynamically evaluated at runtime.

When the interpreter reaches this line, it will attempt to create a 
evaluation tree. First it will attempt to create one using any non-symbols.
If that is not possible, the symbols themselves will be evaluated to create an evaluation tree based on their precedence.

Since there are non-symbols present, the above set of tokens is turned into the following:

    ASSIGNMENT
    --> SYMBOL(f)
    --> FUNCTION
        --> SYMBOL(x)
        --> SYMBOL(23) SYMBOL(+) SYMBOL(x) SYMBOL(*) SYMBOL(4)

This tree represents the assignment of the symbol `f` to the function with
one argument `x`. The body of that function is the remaining symbols.
All the symbols representing the function's body are now grouped together.
They are not evaluated any further at this point.

Evaluation of a tree like this stops when no further reductions are possible.
In this case, no further reductions are possible because the function is
simply being assigned to the symbol. The symbol will be evaluated as part
of the assignment. The reasons for that will be realized as you read on.

When this function gets evaluated, the same algorithm is applied. This time,
since there are only symbols and nothing else, the symbols are used to 
create a tree. This tree, once created, will be beta reduced as much as 
possible as part of the evaluation. 

It is necessary and important that this step be taken on every function 
evaluation since symbol precedence is completely dynamic in this language.

The symbols are looked at in order and their precedence is looked up. 
Symbols only containing numbers are looked up to find their numeric values.
Numbers have the highest precedence values and are defined by zero argument 
functions that evaluate to themselves. Zero arguments means that none of 
their sibling symbols are required for their evaluation. A lookup is enough.
Operators are functions that each have their own defined precedence and 
fixity. This precedence is used to create the evaluation tree that will be 
reduced.

Every time a symbol is encountered that exactly matches a symbol from the
function's arguments, the value provided to that function (or a default
value) is put in its place. For example, if `f 7` is the expression being
evaluated, `SYMBOL(x)` will be replaced with `SYMBOL(7)` whenever it is
encountered.

Since by default, the operator `*` has a higher precedence than the
operator `+`, the above symbols will produce the following tree when `f 7` 
is evaluated.

    ADD
    --> SYMBOL(23)
    --> MULTIPLY
        --> SYMBOL(7)
        --> SYMBOL(4)

Note that as described, `SYMBOL(x)` is replaced with `SYMBOL(7)`. 

When reducing, the interpreter will go to the deepest part of the tree and
attempt to reduce each part, one at a time. Each symbol that only contains
a number will evaluate to that number when looked up. `ADD` and `MULTIPLY`
simply represent the function bodies defined for those operators. These
will take the evaluated arguments and further evaluate them based on their
definitions.

Steps:

1. Symbols are evaluated (in reality this would happen one at a time)

    ADD
    --> SYMBOL(23)
    --> MULTIPLY
        --> 7
        --> 4

2. First operation is completed

    ADD
    --> SYMBOL(23)
    --> 28

3. Last symbol is evaluated. Note how nothing is evaluated until it is
absolutely needed. And note how symbol conversion happens exactly once. Values don't need to be looked up again mid-evaluation.

    ADD
    --> 23
    --> 28

4. Final operation is completed
    51

Evaluation stops because there is just a single value left that cannot be
evaulated any further. No further reductions can take place.

### Building evaluation trees from symbols
The algorithm for building evaluation trees from symbols is very simple.

1. Find the symbol with the highest precedence (for the same precedence, go left to right)
2. Attempt to look it up, find the number of arguments that function needs
    as well as its fixity
    * Numbers when looked up return a zero argument function returning that number's literal value
3. For each argument required, take one of the adjacent symbols and evaluate
    it starting at step 2
    * Use the fixity to determine the direction of this search
    * Infix functions can only have 2 arguments.
4. Replace that symbol with its evaluated version
5. Repeat at step 1 until there are no more symbols to evaluate

This algorithm only applies when there are only symbols within the expression. Before this can be run, any non-symbols (other tokens) must
be evaluated. Once non-symbols have been reduced as much as possible into
either symbols or evaluated components, this algorithm can finish the job.

Every reduction stops once the given tree cannot be reduced any further.

Parenthesis evaluate to a zero argument function returning the evaluation of the tokens within it. This is useful as a method for implementing symbol
grouping and for overriding function/operator precedence.

Example: Given the following set of tokens

    SYMBOL(4) SYMBOL(/) PARENOPEN SYMBOL(5) SYMBOL(+) SYMBOL(6) PARENCLOSE

The first step will be to evaluate the non-symbol tokens. This will result
in the following tree:

    (expr)
    --> SYMBOL(4) SYMBOL(/)
    --> ()
        --> SYMBOL(5) SYMBOL(+) SYMBOL(6)

Note how the items within the PAREN* tokens are now grouped. Each branch
of this tree will now be evaluated following the symbol-tree algorithm
above. Since the `()` create a deeper branch of the tree, that leaf-node
will be evaluated first. Thus forcing that expression to be evaluated
and effectively changing the operator precedence just as we desired.

In terms of scope/closures/etc., for now, functions will simply have
access to everything that can be looked up (read: is defined). In
the future they may only have access to things in their immediate closure
(i.e. on their branch or on a parent branch).

If a lookup fails, that symbol will simply be returned as is. This is
considered reasonable means for the stoppage of evaluation. None of the
immediate parent branches of that branch can be evaluated.

For a tree

    ADD
    --> SYMBOL(23)
    --> MULTIPLY
        --> SYMBOL(a)
        --> SUBTRACT
            --> SYMBOL(9)
            --> SYMBOL(3)

The branch with `SYMBOL(a)` cannot be evaluated. Its sibling can be, so evaluation stops once that sibling has been evaluated. The return value is thus a tree and that tree is printed as is.

The result of this tree is `23 + a * 6` since this is the furthest it could
be reduced.

Note that this returned tree **is not** a value type. If an expression being evaluated cannot be reduced to a single value, a function is returned with
the arguments being any symbols in its deepest branches that could not
be evaluated. Thus, a more accurate statement is that the return value of
this tree is `(a) => 23 + a * 6`.

This enables the expression to be futher evaluated down the line and also
creates the ability to make the interpreter generate pseudo functions on
the fly. The original function notation `(args) => expression` is simply
syntatictic sugar designed to avoid any naming conflicts with the outside
context. So even if you have `x` defined elsewhere, you can safely define
a function using `(x) => ...` without worry.

`SYMBOL`s will contain debugging information such as the line number and
character number of that symbol. This will help make error reporting clear.

### Implementing types
Types are implemented as extension modules that tap into the interpreter.
The number type is one of the defaults included in the interpreter initially.

Numbers can be represented minimally by just a single object:

    {
        value: <numeric value>
        category: <default: units>
    }

That type will then be responsible for implementing all the functionality
it desires by creating an evaluation middleware. Evaluation middleware
is a means for multiple types to define methods for the evaluation of a
single function. This is something only available in extension modules and
essentially means that a symbol can have multiple implementations based
on its context.

Each definition in the lookup table has a default definition. For example,
if a function is defined as follows:

    f = (x) => 2 * x

Then it will be placed in the lookup table as a simple one argument function
accepting all kinds of types. This is the default or catch-all version of
the symbol f.

With middleware, we can add a step before that default function where we
attempt to do something else. 

    function redirectMiddleware(info) {
        if (info.symbol.data === "f") {
            // instead of being a function, f is now a number
            info.finish(new Number(42, UNITS));
            return;
        }
    }

Here, we hook into the lookup for f and return a number instead of a 
function.

    function redirectMiddleware(info) {
        if (info.symbol.data === "f") {
            info.finish(new Symbol("g"));
            return;
        }
    }

Here, we check the symbol's data and then replace it with another symbol.
Then we explicitly stop the evaluation of any further middleware with
`finish()`. The symbol "g" will not be further evaluated until the next
reduction step. An attempt to look it up will be made. If we had called
`finish()` with another type instead (a non-symbol), no further attempts to
look anything up would be made and that symbol would now take the type
passed to `finish()`.

TODO: If info is just the symbol, how do types differentiate using the
type of each argument? Does each version of a function run one at a time?
Am I describing the wrong type of middleware here? This looks like a lookup
middleware when I really should have been describing an evaluation middleware.

Types can even define methods for converting them to strings which is
useful in the REPL.

### More Advanced Types
Let's say we wanted to implement a Vector type. To do that, we may store
a generic vector as follows:

    {
        values: [<array of Number objects>]
    }

We may choose to inject several functions into the namespace such as

    # could be nicer with varadic parameters
    $ v = Vector3 1 2 3
    $ v2 = Vector2 1 2
    $ vectorLength v
    = 3
    $ vectorLength v2
    = 2
    $ vectorMagnitude v
    = sqrt 14

We may also want to support several operations such as

    $ v + (Vector 3 2 1)
    (Vector 4 4 4)

These operations can be implemented using evaluation middleware as defined
above. Simply put, if one evaluation middleware fails to evaluate something,
the next one will attempt it. This will go on until something is evaluated
or until it can no longer be evaluated at all. At that point, evaluation
will stop.

Writing Middleware
------------------
The middleware implementation is inspired by koa.js where middleware
functions "yield downstream" and then control flows back "upstream" 
afterwards.

There are currently just a few main types of middleware:
1. Assignment middleware
2. Evaluation middleware

### Asignment middleware
Assignment middleware allows you to influence the behaviour of the
assignment operator from extension modules. 




