const PREFIX_STRING: &'static str = "FIXITY_PREFIX";
const INFIX_STRING: &'static str = "FIXITY_INFIX";
const POSTFIX_STRING: &'static str = "FIXITY_POSTFIX";

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum Fixity {
    Prefix,
    Infix,
    Postfix,
}

impl Fixity {
    pub fn from_str(string: &str) -> Option<Self> {
        Some(match string {
            PREFIX_STRING => Fixity::Prefix,
            INFIX_STRING => Fixity::Infix,
            POSTFIX_STRING => Fixity::Postfix,
            _ => return None,
        })
    }
}

impl ToString for Fixity {
    fn to_string(&self) -> String {
        String::from(match *self {
            Fixity::Prefix => PREFIX_STRING,
            Fixity::Infix => INFIX_STRING,
            Fixity::Postfix => POSTFIX_STRING,
        })
    }
}

