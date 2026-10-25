//! Maths and unit conversion, worked out by numbat.

use super::{Action, Context, Intent, Module, Reply, Skill, choose, intent, output, pick};
use crate::duration::number_words;
use crate::grammar::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Calc {
    /// A numbat expression: "5 + 3", "sqrt(81)".
    Math(String),
    /// Indices into `UNITS` of the same dimension.
    Convert { value: f64, from: usize, to: usize },
}

fn c(x: Calc) -> Option<Action> {
    Some(Action::Calc(x))
}

pub const MODULE: Module = Module {
    intents,
    slots: &[
        ("expr", |w, _| expression(w).map(Value::Text)),
        ("from", |w, _| lookup(w).map(Value::Index)),
        ("to", |w, _| lookup(w).map(Value::Index)),
    ],
};

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            &[
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (what's|whats|what is|what are|how much is|how many) {number} {from} (in|to|into|as) {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] what would {number} {from} be in {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] [can you] [assist me|help me] [i need|i want] [to] convert {number} {from} (to|into|in) {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] {number} {from} (in|to|into) {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] {number} {from} (is|are) (equal to|how many) [how many] {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (how many|what amount of) {to} (are|is|will make|make|makes|make up|make up in) [there] [in] [a|an|one] {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (how many|what amount of) {to} (are|is|will make|make|makes|make up|make up in) [there] [in] {number} {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (how many|what amount of) {to} [are|is] [there] in [a|an|one] {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (how many|what amount of) {to} [are|is] [there] in {number} {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (how many|what amount of) {to} (are|is) equivalent to [a|an|one] {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (how many|what amount of) {to} (are|is) equivalent to {number} {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (what's|what is) the [total|correct] (number|amount) of {to} in [a|an|one] {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (what's|what is) the [total|correct] (number|amount) of {to} in {number} {from}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] [how] (do|can|could|would) (i|we|you) (convert|change) [from] {from} (to|into) {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] [can you] [assist me|help me] [i need|i want] [to] convert [from] {from} (to|into) {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] (give me|i need) {from} (to|in) {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] [what is|what's|what would be|give me|i need] the conversion (between|from) [a|an] {from} (and|to) [a|an] {to}",
                "![please] [tell me|i need to know|i want to know|i wanna know|i'd like to know|inform me|let me know|can you tell me] if i have {number} {from} how many {to} is that",
            ],
            |s, _| {
                let value = match s.get("number") {
                    Some(Value::Real(v)) => *v,
                    None => 1.0,
                    Some(_) => return None,
                };
                let (Some(Value::Index(from)), Some(Value::Index(to))) =
                    (s.get("from"), s.get("to"))
                else {
                    return None;
                };
                let (from, to) = fluid(*from, *to);
                convertible(from, to).then_some(())?;
                c(Calc::Convert { value, from, to })
            },
        ),
        intent(
            &["!(what's|what|calculate|compute|how) [is|much|many] [is] [the] {expr}"],
            |s, _| match s.get("expr")? {
                Value::Text(e) => c(Calc::Math(e.clone())),
                _ => None,
            },
        ),
    ]
}

/// One spoken operator and how numbat writes it.
const OPERATORS: &[(&[&str], &str)] = &[
    (&["plus"], " + "),
    (&["+"], " + "),
    (&["minus"], " - "),
    (&["-"], " - "),
    (&["times"], " * "),
    (&["x"], " * "),
    (&["×"], " * "),
    (&["*"], " * "),
    (&["multiplied", "by"], " * "),
    (&["divided", "by"], " / "),
    (&["over"], " / "),
    (&["/"], " / "),
    (&["to", "the", "power", "of"], "^"),
    (&["percent", "of"], " / 100 * "),
];

/// The longest run of number words at the start: "twenty five times".
fn leading_number(words: &[&str]) -> Option<(f64, usize)> {
    (1..=words.len())
        .rev()
        .find_map(|n| number_words(&words[..n]).map(|v| (v, n)))
}

/// "5 plus 3", "square root of 81", "2 to the power of 10" as numbat
/// expressions. Only the shape is checked here, so the grammar can try
/// every span cheaply, and numbat computes it once when answering.
pub fn expression(words: &[&str]) -> Option<String> {
    let mut out = String::new();
    let mut rest = words;
    let mut ops = 0;
    loop {
        let root = rest.starts_with(&["square", "root", "of"]);
        if root {
            rest = &rest[3..];
        }
        let (v, n) = leading_number(rest)?;
        rest = &rest[n..];
        let v = format!("{v}");
        if root {
            out.push_str(&format!("sqrt({v})"));
            ops += 1;
        } else {
            out.push_str(&v);
        }
        loop {
            match rest.first() {
                Some(&"squared") => out.push_str("^2"),
                Some(&"cubed") => out.push_str("^3"),
                _ => break,
            }
            rest = &rest[1..];
            ops += 1;
        }
        if rest == ["percent"] {
            return Some(out + " / 100");
        }
        if rest.is_empty() {
            return (ops > 0).then_some(out);
        }
        let (words, op) = OPERATORS.iter().find(|(w, _)| rest.starts_with(w))?;
        out.push_str(op);
        rest = &rest[words.len()..];
        ops += 1;
    }
}

/// numbat's leading number: "8.04672 km", "37.7778".
fn number(out: &str) -> Option<f64> {
    out.split_whitespace().next()?.replace(',', "").parse().ok()
}

fn numbat(expr: &str) -> Option<f64> {
    number(&output("numbat", &["-e", expr])?)
}

fn spoken_number(v: f64) -> String {
    if v.is_infinite() {
        return "infinity".into();
    }
    let rounded = (v * 100.0).round() / 100.0;
    if rounded.fract() == 0.0 {
        format!("{rounded:.0}")
    } else {
        format!("{rounded}")
    }
}

const MATH: &[&str] = &[
    "{}.",
    "That's {}. I'm basically a genius.",
    "{}. Easy. Next.",
    "{}. Show your work? No.",
];
const BIG_MATH: &[&str] = &[
    "{}. Don't make me do that again.",
    "{}. My circuits are sweating.",
    "{}. Next time, use a spreadsheet, darling.",
];

const NO_MATH: &[&str] = &[
    "That doesn't compute. Literally.",
    "I tried. Maths said no.",
    "Undefined. Like your plans for tonight.",
];

impl Skill for Calc {
    fn run(&self, ctx: &Context) -> Reply {
        match self {
            Calc::Math(expr) => match numbat(expr) {
                Some(v) => {
                    let what = spoken_number(v);
                    let speech = choose(&[(v.abs() >= 10_000.0, BIG_MATH)], MATH, &what);
                    Reply::live("Maths", &what, speech)
                }
                None => Reply::live("Maths", "", pick(NO_MATH, "")),
            },
            Calc::Convert { value, from, to } => conversion(*value, *from, *to, ctx),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Dim {
    Length,
    Mass,
    Volume,
    Temperature,
    Speed,
}

pub struct Unit {
    pub singular: &'static str,
    pub plural: &'static str,
    names: &'static [&'static str],
    dim: Dim,
    /// The unit as numbat writes it.
    numbat: &'static str,
}

const fn u(
    singular: &'static str,
    plural: &'static str,
    names: &'static [&'static str],
    dim: Dim,
    numbat: &'static str,
) -> Unit {
    Unit {
        singular,
        plural,
        names,
        dim,
        numbat,
    }
}

use Dim::{Length, Mass, Speed, Temperature, Volume};

pub const UNITS: &[Unit] = &[
    u(
        "millimeter",
        "millimeters",
        &["mm", "millimetre", "millimetres"],
        Length,
        "mm",
    ),
    u(
        "centimeter",
        "centimeters",
        &["cm", "centimetre", "centimetres"],
        Length,
        "cm",
    ),
    u("meter", "meters", &["m", "metre", "metres"], Length, "m"),
    u(
        "kilometer",
        "kilometers",
        &["km", "kilometre", "kilometres", "k"],
        Length,
        "km",
    ),
    u("inch", "inches", &["in"], Length, "inch"),
    u("foot", "feet", &["ft"], Length, "ft"),
    u("yard", "yards", &["yd"], Length, "yard"),
    u("mile", "miles", &["mi"], Length, "mile"),
    u(
        "milligram",
        "milligrams",
        &["mg", "milligramme", "milligrammes"],
        Mass,
        "mg",
    ),
    u("gram", "grams", &["g", "gramme", "grammes"], Mass, "g"),
    u(
        "kilogram",
        "kilograms",
        &["kg", "kilo", "kilos", "kilogramme"],
        Mass,
        "kg",
    ),
    u("ounce", "ounces", &["oz"], Mass, "oz"),
    u("pound", "pounds", &["lb", "lbs"], Mass, "lb"),
    u("stone", "stone", &["stones"], Mass, "stone"),
    u(
        "ton",
        "tons",
        &["tonne", "tonnes", "metric ton", "metric tons"],
        Mass,
        "tonne",
    ),
    u(
        "milliliter",
        "milliliters",
        &["ml", "millilitre", "millilitres"],
        Volume,
        "mL",
    ),
    u("liter", "liters", &["l", "litre", "litres"], Volume, "L"),
    u("teaspoon", "teaspoons", &["tsp"], Volume, "tsp"),
    u("tablespoon", "tablespoons", &["tbsp"], Volume, "tbsp"),
    u("cup", "cups", &[], Volume, "cup"),
    u(
        "fluid ounce",
        "fluid ounces",
        &["fl oz"],
        Volume,
        "fluidounce",
    ),
    u("pint", "pints", &[], Volume, "pint"),
    u("quart", "quarts", &[], Volume, "quart"),
    u("gallon", "gallons", &[], Volume, "gallon"),
    u(
        "degree Celsius",
        "degrees Celsius",
        &[
            "celsius",
            "centigrade",
            "degrees celsius",
            "degree celsius",
            "degrees centigrade",
            "c",
        ],
        Temperature,
        "°C",
    ),
    u(
        "degree Fahrenheit",
        "degrees Fahrenheit",
        &["fahrenheit", "degrees fahrenheit", "degree fahrenheit", "f"],
        Temperature,
        "°F",
    ),
    u("kelvin", "kelvin", &["kelvins"], Temperature, "K"),
    u(
        "kilometer per hour",
        "kilometers per hour",
        &["km/h", "kmh", "kilometres per hour", "kph"],
        Speed,
        "km/h",
    ),
    u("mile per hour", "miles per hour", &["mph"], Speed, "mph"),
    u(
        "meter per second",
        "meters per second",
        &["metres per second"],
        Speed,
        "m/s",
    ),
    u("knot", "knots", &[], Speed, "knot"),
];

/// "kilometers per hour", "a liter", "degrees celsius".
pub fn lookup(words: &[&str]) -> Option<usize> {
    let words = match words {
        ["a" | "an" | "one" | "the", rest @ ..] => rest,
        w => w,
    };
    let name = words.join(" ");
    UNITS.iter().position(|u| {
        u.singular.eq_ignore_ascii_case(&name)
            || u.plural.eq_ignore_ascii_case(&name)
            || u.names.contains(&name.as_str())
    })
}

pub fn convertible(from: usize, to: usize) -> bool {
    matches!((UNITS.get(from), UNITS.get(to)), (Some(f), Some(t)) if f.dim == t.dim)
}

/// numbat's expression for the conversion.
fn conversion_expr(value: f64, from: usize, to: usize) -> Option<String> {
    let (f, t) = (UNITS.get(from)?, UNITS.get(to)?);
    Some(format!("{value} {} -> {}", f.numbat, t.numbat))
}

pub fn convert(value: f64, from: usize, to: usize) -> Option<f64> {
    if !convertible(from, to) {
        return None;
    }
    numbat(&conversion_expr(value, from, to)?)
}

/// "Ounces in a cup" means fluid ounces.
pub fn fluid(from: usize, to: usize) -> (usize, usize) {
    let fix = |a: usize, b: usize| match (UNITS.get(a), UNITS.get(b)) {
        (Some(x), Some(y)) if x.singular == "ounce" && y.dim == Volume => UNITS
            .iter()
            .position(|u| u.singular == "fluid ounce")
            .unwrap_or(a),
        _ => a,
    };
    (fix(from, to), fix(to, from))
}

/// Three significant digits, whole numbers from 100 up.
pub fn spoken(x: f64) -> String {
    let digits = if x.abs() >= 100.0 || x == 0.0 {
        0
    } else {
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        let d = (2.0 - x.abs().log10().floor()) as usize;
        d.min(6)
    };
    let s = format!("{x:.digits$}");
    let s = if s.contains('.') {
        s.trim_end_matches('0').trim_end_matches('.')
    } else {
        &s
    };
    if s == "-0" { "0".into() } else { s.into() }
}

pub fn name(unit: usize, value: f64) -> &'static str {
    UNITS.get(unit).map_or("", |u| {
        if (value - 1.0).abs() < f64::EPSILON {
            u.singular
        } else {
            u.plural
        }
    })
}

const CONVERTED: &[&str] = &[
    "{}.",
    "{}. Science.",
    "{}. I did the maths so you don't have to.",
    "{}. Metric would've been easier, just saying.",
    "{}. You're welcome, Imperial victim.",
    "That's {}. Write it down this time.",
    "{}. No calculator was harmed.",
    "{}. Precise enough for cooking, not for rocket science.",
    "{}. Size isn't everything, but this one's exact.",
    "Roughly {}. Don't sue me over the decimals.",
];
const CONVERTED_LATE: &[&str] = &[
    "{}. Baking at this hour? Respect.",
    "{}. Measuring things at midnight. I won't ask.",
    "{}. Now go to bed.",
];

fn conversion(value: f64, from: usize, to: usize, ctx: &Context) -> Reply {
    let Some(result) = convert(value, from, to) else {
        return Reply::live(
            "Convert",
            "",
            pick(&["Those don't convert. Apples and oranges."], ""),
        );
    };
    let what = format!(
        "{} {} is {} {}",
        spoken(value),
        name(from, value),
        spoken(result),
        name(to, result)
    );
    Reply::live(
        "Convert",
        &what,
        choose(&[(ctx.late(), CONVERTED_LATE)], CONVERTED, &what),
    )
}

#[cfg(test)]
mod tests {
    use super::super::{Action, Context, parse_at};
    use super::*;

    fn calc(t: &str) -> Option<Calc> {
        match parse_at(t, &Context::at(0))? {
            Action::Calc(x) => Some(x),
            _ => None,
        }
    }

    fn conv(t: &str) -> Option<(f64, &'static str, &'static str)> {
        match calc(t)? {
            Calc::Convert { value, from, to } => {
                Some((value, UNITS[from].plural, UNITS[to].plural))
            }
            _ => None,
        }
    }

    #[test]
    fn conversions() {
        for (text, want) in [
            ("How many cups are in a liter?", (1.0, "liters", "cups")),
            (
                "convert 5 miles to kilometers",
                (5.0, "miles", "kilometers"),
            ),
            (
                "What is 100 degrees fahrenheit in celsius",
                (100.0, "degrees Fahrenheit", "degrees Celsius"),
            ),
            ("how many ounces in a pound", (1.0, "pounds", "ounces")),
            ("what's 3 feet in centimeters", (3.0, "feet", "centimeters")),
            (
                "how many teaspoons are there in a tablespoon",
                (1.0, "tablespoons", "teaspoons"),
            ),
            (
                "how many ounces are in a cup",
                (1.0, "cups", "fluid ounces"),
            ),
            (
                "how do I convert pounds to kilograms",
                (1.0, "pounds", "kilograms"),
            ),
            (
                "12 feet is equal to how many inches",
                (12.0, "feet", "inches"),
            ),
            (
                "what's the conversion between tablespoons and cups",
                (1.0, "tablespoons", "cups"),
            ),
            (
                "twenty kilometers per hour in miles per hour",
                (20.0, "kilometers per hour", "miles per hour"),
            ),
        ] {
            let (v, from, to) = want;
            assert_eq!(conv(text), Some((v, from, to)), "{text}");
        }
        assert_eq!(conv("how many miles in a kilogram"), None);
    }

    #[test]
    fn maths() {
        for (text, want) in [
            ("What is 5 plus 3?", "5 + 3"),
            ("What's 7×6?", "7 * 6"),
            ("What is twenty five times four?", "25 * 4"),
            ("Calculate 100 divided by 8", "100 / 8"),
            ("What's the square root of 81?", "sqrt(81)"),
            ("What is 20 percent of 50?", "20 / 100 * 50"),
            ("What's 12 squared?", "12^2"),
            ("How much is 10 minus 15?", "10 - 15"),
            ("What is 2 to the power of 10", "2^10"),
            ("what's 3 plus 4 times 2", "3 + 4 * 2"),
        ] {
            assert_eq!(calc(text), Some(Calc::Math(want.into())), "{text:?}");
        }
        assert_eq!(calc("what is the meaning of life"), None);
        assert_eq!(spoken_number(12.5), "12.5");
        assert_eq!(spoken_number(42.0), "42");
        assert_eq!(spoken_number(1.0 / 3.0), "0.33");
    }

    fn convert_live(v: f64, from: &str, to: &str) -> String {
        let (f, t) = (lookup(&[from]).unwrap(), lookup(&[to]).unwrap());
        spoken(convert(v, f, t).unwrap())
    }

    #[test]
    fn numbat_expressions() {
        let e =
            |v, f: &str, t: &str| conversion_expr(v, lookup(&[f]).unwrap(), lookup(&[t]).unwrap());
        assert_eq!(e(5.0, "miles", "km").as_deref(), Some("5 mile -> km"));
        assert_eq!(
            e(100.0, "fahrenheit", "celsius").as_deref(),
            Some("100 °F -> °C")
        );
        assert_eq!(number("8.04672 km"), Some(8.04672));
        assert_eq!(number("37.7778"), Some(37.7778));
        assert_eq!(number("1,609.34 m"), Some(1609.34));
        assert!(!convertible(
            lookup(&["mile"]).unwrap(),
            lookup(&["gram"]).unwrap()
        ));
    }

    #[test]
    #[ignore = "needs numbat"]
    fn converts() {
        assert_eq!(convert_live(5.0, "miles", "km"), "8.05");
        assert_eq!(convert_live(100.0, "fahrenheit", "celsius"), "37.8");
        assert_eq!(convert_live(0.0, "celsius", "fahrenheit"), "32");
        assert_eq!(numbat("3 + 4 * 2"), Some(11.0));
        assert_eq!(numbat("20 / 100 * 50"), Some(10.0));
        assert_eq!(numbat("2^10"), Some(1024.0));
        assert_eq!(numbat("1 / 0"), None);
        assert_eq!(convert_live(1.0, "liter", "cups"), "4.23");
        assert_eq!(convert_live(1.0, "pound", "grams"), "454");
        assert_eq!(convert_live(1.0, "inch", "mm"), "25.4");
    }
}
