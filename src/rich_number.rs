// Predefined unit constants
const U_UNITS: &'static str = "units";

#[derive(PartialEq, Debug, Clone)]
pub struct RichNumber {
    value: f64,
    unit: String,
}

impl RichNumber {
    pub fn zero() -> RichNumber {
        RichNumber::from(0f64)
    }
}

impl From<f64> for RichNumber {
    fn from(number: f64) -> RichNumber {
        RichNumber {
            value: number,
            unit: String::from(U_UNITS),
        }
    }
}

