use std::ops::Div;

use smallvec::SmallVec;

use unit_graph::UnitID;

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

impl From<UnitID> for CanonicalUnit {
    fn from(id: UnitID) -> Self {
        CanonicalUnit(smallvec![(id, 1)])
    }
}

//FIXME: Implement Mul and invert and then make Div = left * invert(right)
impl<'a> Div<&'a CanonicalUnit> for &'a CanonicalUnit {
    type Output = CanonicalUnit;

    fn div<'b>(self, other: &'b CanonicalUnit) -> Self::Output {
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
            // Left must go after right
            if left > right {
                // Invert exponent since right hand side is on the bottom
                terms.push((right, -y));
                rhs_i += 1;
            }
            // Right must go after left
            else if right > left {
                terms.push((left, x));
                lhs_i += 1;
            }
            // Both are equal and their counts must be subtracted because 'a^x/'a^y = 'a^(x-y)
            else {
                debug_assert!(left == right);
                let exponent = x - y;
                if exponent != 0 {
                    terms.push((left, exponent));
                }
                lhs_i += 1;
                rhs_i += 1;
            }
        }
        // Only one of these loops will run
        while lhs_i < lhs.len() {
            let (left, x) = lhs[lhs_i];
            terms.push((left, x));
            lhs_i += 1;
        }
        while rhs_i < rhs.len() {
            let (right, y) = rhs[rhs_i];
            // Invert exponent since right hand side is on the bottom
            terms.push((right, -y));
            rhs_i += 1;
        }

        ensure_unit_invariants(&terms);
        CanonicalUnit(terms)
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
            CanonicalUnit(smallvec![$(($unit_name, $exp)),*])
        };
    }

    #[test]
    fn division() {
        unit_ids!(a, b, c);
        let unit1 = unit!(a^1 b^1);
        let unit2 = unit!(a^1 b^2 c^3);
        let expected = unit!(b^-1 c^-3);
        assert_eq!(unit1.div(&unit2), expected);
    }
}
