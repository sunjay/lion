# Semantics

The details of when exactly conversions occur are very important for
accomplishing the goals outlined in [Unification]. In particular, it is
important to acknowledge that we always abide by the commutative, associative,
and distributive laws of each operator. These laws come in to play for certain
**irreducible** expresions. An irreducible expression is an expression that
cannot be evaluated further because of some conflict that prevents its units
from combining. This is only currently possible with the `+` and `-` operators.

The `+` and `-` operators requires a conversion from the RHS unit to the LHS
unit. If this conversion is not possible, the operation does not take place and
the expression is marked unreducible at that point.

FIXME: Maybe irreducibility should just be an error?

Compound units (unit expressions) support multiplication, division and
exponentiation only. Thus, when using the `*`, `/` and `^` operators, no
conversion takes place. Normal mathematical rules apply (e.g. `'a * 'b / 'a`
produces `'b`).

At the end of the evaluation of an expression, one of two things can happen:

1. **simplification** - This is essentially where anything like `'m * 'km` is
   reconciled into `'m ^ 2`. This occurs when the variable has no target type
   or we are evaluating an expression in the REPL that doesn't get stored in any
   variable.
2. **conversion into a target unit** - If the result of the expr is `'m / 's`
   but the type of the target variable is `'km / 's`, we should be able to
   perform a conversion and get there.

These final two steps introduce quite a few complications. Consider if the unit
from the second case was applied to the first. That is, consider if the result
of the expression was `'m ^ 's` but the specified unit of the target variable
was `'m * 'km`. How would you resolve that?

The trick is to think about what the `k` in `'km` means. If we look at the
example from the first case, we can simplify `'m * 'km` to `'m ^ 's` because
`1000 'm = 1 'km`. Since these two units can be converted to one another, we
can determine the answer by dividing the value by 1000 and setting the unit to
`'m * 'm`.

This realization is great, but also extremely problematic as far as proof search
goes. Consider a more general case: `'a'b'c` being converted to `'d'e'f`. Since
any pair or triple of these units can convert to the others, we have a huge
multitude of solutions:

* `'a => 'd` and `'b => 'e` and `'c => 'f`
* `'a => 'd'e` and `'b'c => 'f`
* `'a => 'e'f` and `'b'c => 'd`
* `'a => 'd'f` and `'b'c => 'e`
* `'a'b => 'd` and `'c => 'e'f`
* `'a'b => 'd'e` and `'c => 'f`
* and so on...

Note: No unit can convert to `'_` and defining such a conversion is **illegal**,
so that case is not considered here.

This example just has unit multiplication in it, but the same complications
occur for units that mix any combination of multiplication, division, and
exponentiation.

Further complications: [Temperatures vs Temperature rates](http://www.warmtips.com/20060711.htm)

[Unification]: unification.md
