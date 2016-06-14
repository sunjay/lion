#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum Fixity {
    Prefix,
    Infix,
    Postfix,
}

//TODO: Move Fixity into its own module and replace these string literals with `const` declarations
impl Fixity {
    fn from_string(string: String) -> Option<Self> {
        Some(match string.as_ref() {
            "PREFIX" => Fixity::Prefix,
            "INFIX" => Fixity::Infix,
            "POSTFIX" => Fixity::Postfix,
            _ => return None,
        })
    }
}

impl ToString for Fixity {
    fn to_string(&self) -> String {
        String::from(match *self {
            Fixity::Prefix => "PREFIX",
            Fixity::Infix => "INFIX",
            Fixity::Postfix => "POSTFIX",
        })
    }
}

