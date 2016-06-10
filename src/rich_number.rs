use std::ops::{Add, Sub, Mul, Div, Rem};

// Predefined unit constants
const U_UNITS: usize = 1;

#[derive(Debug, Clone, Copy)]
pub struct RichNumber {
    value: f64,
    unit: usize,
}

impl RichNumber {
    pub fn new(value: f64, unit: usize) -> RichNumber {
        RichNumber {
            value: value as f64,
            unit: unit,
        }
    }

    pub fn zero() -> RichNumber {
        RichNumber::from(0f64)
    }

    /// Attempts to coerce two values into a common unit
    /// Returns (converted self, converted other, common unit)
    fn coerce(&self, other: &RichNumber) -> (f64, f64, usize) {
        //TODO: Convert between units
        (self.value, other.value, U_UNITS)
    }

    /// Applies an operator to two values given the values and their common unit
    fn apply_operator<F, R>(self, other: RichNumber, operator: F) -> R
        where F: FnOnce(f64, f64, usize) -> R {

        let (value, other_value, unit) = self.coerce(&other);
        operator(value, other_value, unit)
    }

    /// Converts self and other into a common or compatible unit and then performs the given operation
    /// operator is given the two values in order and the common unit
    /// Returns a RichNumber with the new value and the common unit
    fn apply_numeric_operator<F>(self, other: RichNumber, operator: F) -> RichNumber
        where F: FnOnce(f64, f64, usize) -> f64 {

        let (value, other_value, unit) = self.coerce(&other);
        RichNumber::new(operator(value, other_value, unit), unit)
    }
}

impl From<i64> for RichNumber {
    fn from(number: i64) -> RichNumber {
        RichNumber::new(number as f64, U_UNITS)
    }
}

impl From<f64> for RichNumber {
    fn from(number: f64) -> RichNumber {
        RichNumber::new(number, U_UNITS)
    }
}

impl PartialEq for RichNumber {
    fn eq(&self, other: &RichNumber) -> bool {
        let (value, other_value, _) = self.coerce(other);

        (value - other_value).abs() < 1e-10
    }
}

impl Add for RichNumber {
    type Output = RichNumber;

    fn add(self, other: RichNumber) -> Self::Output {
        self.apply_numeric_operator(other, |a, b, _| a + b)
    }
}

impl Sub for RichNumber {
    type Output = RichNumber;

    fn sub(self, other: RichNumber) -> Self::Output {
        self.apply_numeric_operator(other, |a, b, _| a - b)
    }
}

impl Mul for RichNumber {
    type Output = RichNumber;

    fn mul(self, other: RichNumber) -> Self::Output {
        self.apply_numeric_operator(other, |a, b, _| a * b)
    }
}

impl Div for RichNumber {
    type Output = RichNumber;

    fn div(self, other: RichNumber) -> Self::Output {
        self.apply_numeric_operator(other, |a, b, _| a / b)
    }
}

impl Rem for RichNumber {
    type Output = RichNumber;

    fn rem(self, other: RichNumber) -> Self::Output {
        self.apply_numeric_operator(other, |a, b, _| a % b)
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
}

