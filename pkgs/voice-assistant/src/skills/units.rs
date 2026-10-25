//! Unit conversion.

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
fn expression(value: f64, from: usize, to: usize) -> Option<String> {
    let (f, t) = (UNITS.get(from)?, UNITS.get(to)?);
    Some(format!("{value} {} -> {}", f.numbat, t.numbat))
}

/// The leading number of numbat's answer: "8.04672 km", "37.7778".
fn number(out: &str) -> Option<f64> {
    out.split_whitespace().next()?.replace(',', "").parse().ok()
}

pub fn convert(value: f64, from: usize, to: usize) -> Option<f64> {
    if !convertible(from, to) {
        return None;
    }
    number(&super::output(
        "numbat",
        &["-e", &expression(value, from, to)?],
    )?)
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

#[cfg(test)]
mod tests {
    use super::*;

    fn conv(v: f64, from: &str, to: &str) -> String {
        let (f, t) = (lookup(&[from]).unwrap(), lookup(&[to]).unwrap());
        spoken(convert(v, f, t).unwrap())
    }

    #[test]
    fn numbat_expressions() {
        let e = |v, f: &str, t: &str| expression(v, lookup(&[f]).unwrap(), lookup(&[t]).unwrap());
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
        assert_eq!(conv(5.0, "miles", "km"), "8.05");
        assert_eq!(conv(100.0, "fahrenheit", "celsius"), "37.8");
        assert_eq!(conv(0.0, "celsius", "fahrenheit"), "32");
        assert_eq!(conv(1.0, "liter", "cups"), "4.23");
        assert_eq!(conv(1.0, "pound", "grams"), "454");
        assert_eq!(conv(1.0, "inch", "mm"), "25.4");
    }
}
