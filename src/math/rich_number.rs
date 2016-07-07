use std::cmp::Ordering;
use std::ops::{Neg, Add, Sub, Mul, Div, Rem};

//TODO: Refactor Unit into its own newtype
//TODO: Define a `.next()` method that returns the next unit from a given unit (use this in EvalContext) instead of manually incrementing the unit in create_unit
pub type Unit = usize;

#[derive(Debug, Clone, Copy)]
pub struct RichNumber {
    //TODO: These should not be public as modifying them may not always force strict adherence to mathematical laws
    pub value: f64,
    pub unit: Option<Unit>,
}

impl RichNumber {
    pub fn new(value: f64, unit: Option<Unit>) -> RichNumber {
        RichNumber {
            value: value,
            unit: unit,
        }
    }

    //TODO: Rename from_unit to with_unit per Rust styleguide
    pub fn from_unit(value: f64, unit: Unit) -> RichNumber {
        RichNumber::new(value, Some(unit))
    }

    pub fn is_dimensionless(&self) -> bool {
        self.unit.is_none()
    }

    pub fn has_units(&self) -> bool {
        !self.is_dimensionless()
    }

    pub fn without_units(&self) -> RichNumber {
        RichNumber::new(self.value, None)
    }

    pub fn pow(&self, other: RichNumber) -> RichNumber {
        assert!(self.is_dimensionless() && other.is_dimensionless(),
            "Applying exponents to quantities with units is not supported yet, only raise dimensionless/scalar quantities to dimensionless/scalar exponents");

        RichNumber::from(self.value.powf(other.value))
    }

    pub fn abs(&self) -> RichNumber {
        RichNumber::new(self.value.abs(), self.unit)
    }

    pub fn floor(&self) -> RichNumber {
        RichNumber::new(self.value.floor(), self.unit)
    }

    pub fn ceil(&self) -> RichNumber {
        RichNumber::new(self.value.ceil(), self.unit)
    }

    pub fn round(&self) -> RichNumber {
        RichNumber::new(self.value.round(), self.unit)
    }

    pub fn sqrt(&self) -> RichNumber {
        assert!(self.is_dimensionless(), "Only supports dimensionless/scalar quantities");

        RichNumber::from(self.value.sqrt())
    }

    pub fn ln(&self) -> RichNumber {
        assert!(self.is_dimensionless(), "Only supports dimensionless/scalar quantities");

        RichNumber::from(self.value.ln())
    }

    pub fn log10(&self) -> RichNumber {
        assert!(self.is_dimensionless(), "Only supports dimensionless/scalar quantities");

        RichNumber::from(self.value.log10())
    }

    pub fn log2(&self) -> RichNumber {
        assert!(self.is_dimensionless(), "Only supports dimensionless/scalar quantities");

        RichNumber::from(self.value.log2())
    }

    pub fn sin(&self) -> RichNumber {
        RichNumber::from(self.value.sin())
    }

    pub fn cos(&self) -> RichNumber {
        RichNumber::from(self.value.cos())
    }

    pub fn tan(&self) -> RichNumber {
        RichNumber::from(self.value.tan())
    }

    pub fn asin(&self) -> RichNumber {
        assert!(self.is_dimensionless(), "Only supports dimensionless/scalar quantities");

        RichNumber::new(self.value.asin(), self.unit)
    }

    pub fn acos(&self) -> RichNumber {
        assert!(self.is_dimensionless(), "Only supports dimensionless/scalar quantities");

        RichNumber::new(self.value.acos(), self.unit)
    }

    pub fn atan(&self) -> RichNumber {
        assert!(self.is_dimensionless(), "Only supports dimensionless/scalar quantities");

        RichNumber::new(self.value.atan(), self.unit)
    }

    pub fn sinh(&self) -> RichNumber {
        RichNumber::from(self.value.sinh())
    }

    pub fn cosh(&self) -> RichNumber {
        RichNumber::from(self.value.cosh())
    }

    pub fn tanh(&self) -> RichNumber {
        RichNumber::from(self.value.tanh())
    }

    pub fn asinh(&self) -> RichNumber {
        assert!(self.is_dimensionless(), "Only supports dimensionless/scalar quantities");

        RichNumber::new(self.value.asinh(), self.unit)
    }

    pub fn acosh(&self) -> RichNumber {
        assert!(self.is_dimensionless(), "Only supports dimensionless/scalar quantities");

        RichNumber::new(self.value.acosh(), self.unit)
    }

    pub fn atanh(&self) -> RichNumber {
        assert!(self.is_dimensionless(), "Only supports dimensionless/scalar quantities");

        RichNumber::new(self.value.atanh(), self.unit)
    }

    /// Applies an operator to two values given the values and what the resulting unit should be
    fn apply_operator<F>(self, other: RichNumber, operator: F, unit: Option<Unit>) -> RichNumber
        where F: FnOnce(f64, f64) -> f64 {

        let value = operator(self.value, other.value);
        RichNumber::new(value, unit)
    }
}

impl From<i64> for RichNumber {
    fn from(number: i64) -> RichNumber {
        RichNumber::from(number as f64)
    }
}

impl From<f64> for RichNumber {
    fn from(number: f64) -> RichNumber {
        RichNumber::new(number, None)
    }
}

impl PartialEq for RichNumber {
    fn eq(&self, other: &RichNumber) -> bool {
        assert!(self.unit == other.unit,
            "Cannot compare values with different units");

        (self.value - other.value).abs() < 1e-10
    }
}

impl Eq for RichNumber {}

impl PartialOrd for RichNumber {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        assert!(self.unit == other.unit,
            "Cannot compare values with different units");

        self.value.partial_cmp(&other.value)
    }
}

impl Neg for RichNumber {
    type Output = RichNumber;

    fn neg(self) -> Self::Output {
        RichNumber::new(-self.value, self.unit)
    }
}

impl Add for RichNumber {
    type Output = RichNumber;

    fn add(self, other: RichNumber) -> Self::Output {
        assert!(self.unit == other.unit,
            "Cannot add values with different units");
        self.apply_operator(other, |a, b| a + b, self.unit)
    }
}

impl Sub for RichNumber {
    type Output = RichNumber;

    fn sub(self, other: RichNumber) -> Self::Output {
        assert!(self.unit == other.unit,
            "Cannot subtract values with different units");
        self.apply_operator(other, |a, b| a - b, self.unit)
    }
}

impl Mul for RichNumber {
    type Output = RichNumber;

    fn mul(self, other: RichNumber) -> Self::Output {
        let unit;
        if self.is_dimensionless() {
            unit = other.unit;
        }
        else if other.is_dimensionless() {
            unit = self.unit;
        }
        else {
            panic!("Multiplying units is not supported yet. Try a dimensionaless/scalar value");
        }

        self.apply_operator(other, |a, b| a * b, unit)
    }
}

impl Div for RichNumber {
    type Output = RichNumber;

    fn div(self, other: RichNumber) -> Self::Output {
        let unit;
        if other.is_dimensionless() {
            unit = self.unit;
        }
        else if self.unit == other.unit {
            unit = None;
        }
        else {
            panic!("Dividing units is not supported yet. Use two values with the same unit or multiply by a dimensionless/scalar value");
        }

        self.apply_operator(other, |a, b| a / b, unit)
    }
}

impl Rem for RichNumber {
    type Output = RichNumber;

    fn rem(self, other: RichNumber) -> Self::Output {
        let unit;
        if other.is_dimensionless() {
            unit = self.unit;
        }
        else if self.unit == other.unit {
            unit = None;
        }
        else {
            panic!("Taking the remainder of quantities with units is not supported. Use two values with the same unit or multiply by a dimensionless/scalar value");
        }

        self.apply_operator(other, |a, b| a % b, unit)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn equality() {
        let a = RichNumber::from(2);
        let b = RichNumber::from(2);
        let c = RichNumber::from(3.6);

        assert_eq!(a, b);
        assert!(a != c);
    }

    #[test]
    fn basic_operations() {
        let a = RichNumber::from(2);
        let b = RichNumber::from(3.6);

        assert_eq!(a + b, RichNumber::from(5.6f64));
        assert_eq!(b + a, RichNumber::from(5.6f64));

        assert_eq!(a - b, RichNumber::from(-1.6f64));
        assert_eq!(b - a, RichNumber::from(1.6f64));

        assert_eq!(a * b, RichNumber::from(7.2f64));
        assert_eq!(b * a, RichNumber::from(7.2f64));

        assert_eq!(a / b, RichNumber::from(0.55555555555f64));
        assert_eq!(b / a, RichNumber::from(1.8f64));

        assert_eq!(a % b, RichNumber::from(2f64));
        assert_eq!(b % a, RichNumber::from(1.6f64));
    }

    #[test]
    fn ordering() {
        let a = RichNumber::from(2);
        let b = RichNumber::from(3.6666666666666);
        let c = RichNumber::from(3.6666666666667);
        let d = RichNumber::from(3.6666666666667);

        assert!(a < b);
        assert!(b < c);
        assert!(c <= d);
        assert!(d > b);
        assert!(b >= a);
    }
}

