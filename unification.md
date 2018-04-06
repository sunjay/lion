# Unification Algorithm

The process of unifying two units will produce a derivation leading from
one unit to a target unit.

For example, let's say you declare:

```rust
let x 'm / 's = 100 'm / 5 's;
assert_eq!(x, 20 'm / 's);
```

The goal of this unification would be 'm / 's and the derivation would
say that you need to divide 100 by 5 to get 20 and the resulting unit
would be exactly equal to the goal of 'm / 's.

Now let's say you had the following:
```rust
let x 'm / 's ^ 2 = 100 'm / 5 's;
```

This would not unify because there is no way to get from 'm / 's to 'm / 's^2.

If you did the following, it would work because there is a valid conversion from
'm to 'km and 's to 'hour.

```rust
let x 'km / 'hour = 100 'm / 5 's;
assert_eq!(x, 72 'kph); // 'kph is an alias for 'km / 'hour
```

Cases like this are where unification comes in. We need to "unify" because
complex cases like this require a deeper analysis. We may not necessarily have
a direct mapping from 'km / 'hour to 'm / 's. Instead, we can use our knowledge
of 'm to 'km and 's to 'hour to figure it out.

Another case where this is useful is for equivalent units due to some algebraic
properties like commutative multiplication or how `a^-2 = 1/a^2`. Let's say we
want to express the gravitational constant G. This is approximately equal to
6.647e-11 m^3 kg^-1 s^-2. Here are just a few of the different ways this could
be expressed:

```rust
// No unit (auto conversion from unitless to unit)
let G 'm^3 * 'kg^-1 * 's^-2 = 6.647e-11;

// Commutative multiplication
G = 6.647e-11 'kg^-1 * 'm^3 * 's^-2;
G = 6.647e-11 'kg^-1 * 's^-2 * 'm^3;
G = 6.647e-11 's^-2 * 'kg^-1 * 'm^3;

// With division, parenthesis and commutative multiplication
G = 6.647e-11 'm^3 / ('kg^1 * 's^2);
G = 6.647e-11 'm^3 / ('s^2 * 'kg^1);

// Weird, but still technically correct
G = 6.647e-11 's^-2 / ('m^-3 * 'kg^1);
```

This code works because no matter how we express it, we are always assigning the
same unit. If we print out the value of G, we get its original unit back because
unification always favors the notation of the target unit.

For expressions, we always try to use the first unit we know about as the
unification target. This means that the following will produce two different but
equivalent results:

```rust
% 10 'm * 's + 20 's * 'm
30 'm * 's
% 10 's * 'm + 20 'm * 's
30 's * 'm
```

Expressions are special in that the unification target is allowed to change
during the unification in case two units cannot be converted to one another.
This is what allows expressions like `100 'm / 5 's` to work. When we see
`100 'm`, the target is set to `'m` and it immediately unifies because `100 'm`
has the same unit as the target. Then, when we see `'s`, we realize that there
is no defined conversion between them so we apply the `/` operator and change
the target to `'m / 's`.

This behaviour is common between all different kinds of units but its exact
semantics depends on the operator. We applied `/` to `100` and `5` above because
compound units support the `/` operator. However, if we had something like the
following:

```rust
% 10 'm + 5 's
10 'm + 5 's
```

We just get back the exact same expression with `10` and `5` unchanged because
there is no way to unify those units and so there is no way to simplify the
expression. Compound units only support `*`, `/`, and `^` with constants.

The convenient part about all of this is that it allows you to do stuff like
this:

```rust
% // Convert 20 CAD / kg to cents per g (CAD = Canadian Dollar)
% (20 'CAD / 'kg) * (100 'cent / 1 'CAD) * (1 'kg / 1000 'g)
2 'cent / 'g
% // Convert 40 m/s to km/h (the parenthesis above were optional)
% 40 'm/'s * 1'km/1000'm * 60's/'min * 60'min/'hour
144 'km / 'h
```

The units that are the same divide out as you would expect and you are only left
with the result that you want. This generalizes to exponents too:

```rust
% // 'N is a "Newton" (a unit of force) and it is an alias for
% // the type 'kg * 'm * 's^-2
% // We know that F = m a and a = v / t.
% let F = 5.2 'N;
% let m = 150 'lbs;
% let t = 30 'min;
% // Answer is automatically converted to the unit of the variable.
% // It is an error if the conversion is impossible.
% let v 'km / 'hour = F / m * t;
% v
495 'km / 'hour
% v 'm / 's
138 'm / 's
```

Here, the force starts out with the unit `'N` which is the same as
`'kg * 'm * 's^-2`. Multiplying `F` by `t`, which has the unit `'min`, results in
`t` first being converted to `'s` and then multiplied by the unit
`'kg * 'm * 's^-2`. This causes the unit to become `'kg * 'm * 's^-1`. When we
divide by `m` with unit `'lbs`, `'lbs` is converted to `'kg` and we end up with
`'m * 's^-1`. The final conversion is to the unit of `v` which is `'km / 'hour`.
This is a straightforward conversion from `'m` to `'km` and `'s` to `'hour`. We
see the result in `'m / 's` by printing `v 'm / 's`. This does the conversion
back from `'km` to `'m` and from `'hour` to `'s`.

We will always attempt to simply units as much as possible by either converting
to equivalent units, or when the units are the same, dividing them out or
joining them with an appropriate exponent.

```rust
% 10 's + 1500 'ms
11.5 's
% 2 'm / 2 'm
1
% 2 'm * 2 'm
4 'm ^ 2
% 16 's ^ 2 / 2 's
8 's
```

Note that unit conversions are a little different when using units with
exponents.

```rust
% // 'cm is NOT converted to 'cm^3 when using + or -
% 10 'cm + 1000 'cm^3 // Incompatible units
10 'cm + 1000 'cm^3
% // Though 1 cm is 10 mm, 1 cm^2 is 100 mm^2
% 1 'cm^2 + 100 'mm^2
2 'cm ^ 2
```

The first example does not work because the two units have **different**
dimensions and are thus incompatible. The second example works because units of
the **same** dimension can be added or subtracted. An important detail here is
that the conversion factor for units with exponents is different from when the
units have no exponent.

Alias units can be used to combine commonly used compound units into a single
unit which automatically gets converted into the original compound unit as
necessary. Alias units are regular units and are not a special case. When
simplifying unit expressions, we will attempt to use these alias relationships
to produce a simpler expression. This is only done in expressions where the
unit is not already declared as something else.

```rust
% // 1 Newton: 'N = 'kg * 'm * 's^-2
% // Expressions are always simplified because their unit is not
% // declared explicitly anywhere
% 5 'kg * 'm * 's^-2
5 'N
% // The unit of F is simplified to 'N since F declares no unit
% let F = 5 'kg * 'm * 's^-2;
% F
5 'N
% // Unit is unchanged because it is already in its simplest form
% let F = 5 'N;
% F
5 'N
% // Unit is unsimplified because variable explicitly declares its unit
% let F 'kg * 'm * 's^-2 = 5;
% // Expressions with only an identifier are not simplified because the
% // identifier has a unit already
% F
5 'kg * 'm * 's^-2
```

Note that this doesn't change the meaning of any of these values, only the way
the unit is represented. All of these are still equivalent to one another.
