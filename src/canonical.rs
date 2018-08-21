use std::ops::{Mul, Div, BitXor as Pow};

use smallvec::SmallVec;

use ast::{UnitExpr, UnitName, Span};
use unit_graph::{UnitGraph, UnitID};

pub struct UndeclaredUnit<'a> {
    pub name: UnitName<'a>,
    pub span: Span<'a>,
}

/// We assume that most compound units won't involve more than this many units. This allows us to
/// store most unit representations entirely on the stack while "spilling" over to the heap if our
/// assumption turns out to be false. This constant can be changed over time based on experience.
/// The value of this constant affects the memory overhead of the compiler significantly if most
/// units are much smaller than this constant and it affects the compiler's performance
/// significantly if most units are larger than this constant.
const MAX_ASSUMED_UNIT_SIZE: usize = 8;

/// A CanonicalUnit is a unit expression in canonical form
///
/// e.g. 'a^x 'b^y 'c^z
///      where 'a, 'b, 'c are units and x, y, z are integers
///
/// The elements are guaranteed to be sorted by UnitID (ascending) and not have any duplicate
/// UnitIDs. No exponent will be zero.
///
/// The number of terms in a CanonicalUnit can be zero. In that case, the CanonicalUnit represents
/// a "unitless" quantity (denoted '_)
#[derive(Debug, Clone, Default, Hash, PartialEq, Eq)]
pub struct CanonicalUnit(SmallVec<[(UnitID, i64); MAX_ASSUMED_UNIT_SIZE]>);

/// Ensure that the terms are sorted and without duplicates
fn ensure_unit_invariants(terms: &[(UnitID, i64)]) {
    let mut last = None;
    for (term, x) in terms {
        if *x == 0 {
            panic!("bug: exponent in canonical unit was zero: {:?}",
                CanonicalUnit(SmallVec::from_slice(terms)));
        }
        if let Some(last) = last {
            if term == last {
                panic!("bug: duplicate unit in canonical representation: {:?}",
                    CanonicalUnit(SmallVec::from_slice(terms)));
            }
            else if term < last {
                panic!("bug: unsorted unit in canonical representation: {:?}",
                    CanonicalUnit(SmallVec::from_slice(terms)));
            }
        }
        else {
            last = Some(term);
        }
    }
}

macro_rules! unit_invariants_debug {
    ($name:ident) => {
        #[cfg(debug)]
        ensure_unit_invariants(&$name);
    };
}

impl CanonicalUnit {
    /// Returns the canonical unit that represents unitless
    pub fn unitless() -> Self {
        CanonicalUnit(SmallVec::new())
    }

    /// Canonicalizes a unit
    pub fn from_unit_expr<'a>(expr: &UnitExpr<'a>, units: &UnitGraph) -> Result<Self, UndeclaredUnit<'a>> {
        use self::UnitExpr::*;
        //TODO: Make sure unit is sorted
        Ok(match expr {
            Unit(name, _) if name.is_unitless() => CanonicalUnit::unitless(),
            &Unit(name, span) => units.unit_id(name).map(Self::from).map_err(|_| UndeclaredUnit {
                name,
                span,
            })?,
            Mul(lhs, rhs, _) => {
                let lhs = CanonicalUnit::from_unit_expr(lhs, units)?;
                let rhs = CanonicalUnit::from_unit_expr(rhs, units)?;
                lhs * rhs
            },
            Div(lhs, rhs, _) => {
                let lhs = CanonicalUnit::from_unit_expr(lhs, units)?;
                let rhs = CanonicalUnit::from_unit_expr(rhs, units)?;
                lhs / rhs
            },
            Pow(lhs, exp, _) => {
                let lhs = CanonicalUnit::from_unit_expr(lhs, units)?;
                lhs ^ *exp
            },
        })
    }

    /// Returns true if this represents having no unit
    pub fn is_unitless(&self) -> bool {
        // "Unitless" means has no units
        self.0.is_empty()
    }
}

impl From<UnitID> for CanonicalUnit {
    fn from(id: UnitID) -> Self {
        CanonicalUnit(smallvec![(id, 1)])
    }
}

// Maybe not a good idea to do this, but we're using Bitwise XOR ^ for exponentiation
impl Pow<i64> for CanonicalUnit {
    type Output = CanonicalUnit;

    fn bitxor(self, rhs: i64) -> Self::Output {
        &self ^ rhs
    }
}

impl<'a> Pow<i64> for &'a CanonicalUnit {
    type Output = CanonicalUnit;

    fn bitxor(self, rhs: i64) -> Self::Output {
        CanonicalUnit(self.0.iter()
            .map(|&(x, e)| (x, e * rhs))
            .filter(|&(_, e)| e != 0).collect())
    }
}

impl<'a> Mul<CanonicalUnit> for &'a CanonicalUnit {
    type Output = CanonicalUnit;

    fn mul(self, other: CanonicalUnit) -> Self::Output {
        self * &other
    }
}

impl Mul<CanonicalUnit> for CanonicalUnit {
    type Output = CanonicalUnit;

    fn mul(self, other: CanonicalUnit) -> Self::Output {
        &self * &other
    }
}

impl<'a> Mul<&'a CanonicalUnit> for &'a CanonicalUnit {
    type Output = CanonicalUnit;

    fn mul<'b>(self, other: &'b CanonicalUnit) -> Self::Output {
        let mut terms = smallvec![];

        let CanonicalUnit(lhs) = self;
        let CanonicalUnit(rhs) = other;
        unit_invariants_debug!(lhs);
        unit_invariants_debug!(rhs);
        let mut lhs_i = 0;
        let mut rhs_i = 0;
        while lhs_i < lhs.len() && rhs_i < rhs.len() {
            let (left, x) = lhs[lhs_i];
            let (right, y) = rhs[rhs_i];
            // Left must go after right in the sorted order
            if left > right {
                terms.push((right, y));
                rhs_i += 1;
            }
            // Right must go after left in the sorted order
            else if right > left {
                terms.push((left, x));
                lhs_i += 1;
            }
            // Both are equal and their counts must be added because 'a^x * 'a^y = 'a^(x+y)
            else {
                debug_assert!(left == right);
                let exponent = x + y;
                if exponent != 0 {
                    terms.push((left, exponent));
                }
                lhs_i += 1;
                rhs_i += 1;
            }
        }
        // Only up to one of these loops will run
        while lhs_i < lhs.len() {
            let (left, x) = lhs[lhs_i];
            terms.push((left, x));
            lhs_i += 1;
        }
        while rhs_i < rhs.len() {
            let (right, y) = rhs[rhs_i];
            terms.push((right, y));
            rhs_i += 1;
        }

        ensure_unit_invariants(&terms);
        CanonicalUnit(terms)
    }
}

impl<'a> Div<CanonicalUnit> for &'a CanonicalUnit {
    type Output = CanonicalUnit;

    fn div(self, other: CanonicalUnit) -> Self::Output {
        self / &other
    }
}

impl Div<CanonicalUnit> for CanonicalUnit {
    type Output = CanonicalUnit;

    fn div(self, other: CanonicalUnit) -> Self::Output {
        &self / &other
    }
}

//FIXME: Implement Mul and invert and then make Div = left * invert(right)
impl<'a> Div<&'a CanonicalUnit> for &'a CanonicalUnit {
    type Output = CanonicalUnit;

    fn div<'b>(self, other: &'b CanonicalUnit) -> Self::Output {
        // self / other == self * other^-1
        self * (other ^ -1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Generate variables for some UnitIDs
    ///
    /// unit_ids!(a, b, c) generates:
    ///
    ///    let a: UnitID = 0;
    ///    let b: UnitID = 1;
    ///    let c: UnitID = 2;
    macro_rules! unit_ids {
        // Use default counter
        ($name:ident, $($rest:tt)*) => {
            unit_ids!(0, $name, $($rest)*);
        };
        // Need to make the comma optional
        ($name:ident) => {
            unit_ids!(0, $name);
        };
        // Increment a given counter
        ($count:expr, $name:ident, $($rest:tt)*) => {
            unit_ids!($count, $name);
            unit_ids!($count + 1, $($rest)*);
        };
        // Need to make the comma optional
        ($count:expr, $name:ident) => {
            let $name: UnitID = $count;
        };
        // Base case
        ($count:expr) => ();
    }

    macro_rules! unit {
        ($($unit_name:ident ^ $exp:expr)*) => {
            {
                let mut terms = smallvec![$(($unit_name, $exp)),*];
                terms.sort_unstable_by(|&(a, _): &(UnitID, i64), &(b, _): &(UnitID, i64)| a.cmp(&b));
                ensure_unit_invariants(&terms);
                CanonicalUnit(terms)
            }
        };
    }

    #[test]
    fn pow() {
        // These are intentionally out of order so that we can test if the unit is sorted properly
        unit_ids!(a, c, b);
        let unit1 = &unit!(a^3 b^2 c^-2);
        // Should multiply the exponents
        assert_eq!(unit1 ^ 2, unit!(a^6 b^4 c^-4));
        assert_eq!(unit1 ^ -1, unit!(a^-3 b^-2 c^2));
        assert_eq!(unit1 ^ -2, unit!(a^-6 b^-4 c^4));
        assert_eq!(unit1 ^ 0, CanonicalUnit::unitless());
    }

    #[test]
    fn multiplication() {
        // These are intentionally out of order so that we can test if the unit is sorted properly
        unit_ids!(a, c, d, b);
        let unit1 = unit!(a^3 b^2 c^-2);
        let unit2 = unit!(b^-1 c^2 d^3);
        let expected = unit!(a^3 b^1 d^3);
        assert_eq!(unit1 * unit2, expected);
    }

    #[test]
    fn division() {
        // These are intentionally out of order so that we can test if the unit is sorted properly
        unit_ids!(b, c, a);
        let unit1 = unit!(a^1 b^1);
        let unit2 = unit!(a^1 b^2 c^3);
        let expected = unit!(b^-1 c^-3);
        assert_eq!(unit1 / unit2, expected);
    }
}
