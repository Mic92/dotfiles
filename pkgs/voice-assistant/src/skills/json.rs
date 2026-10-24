//! Panic-free access into parsed JSON.

use tinyjson::JsonValue;

pub fn key<'a>(v: &'a JsonValue, k: &str) -> Option<&'a JsonValue> {
    match v {
        JsonValue::Object(o) => o.get(k),
        _ => None,
    }
}

pub fn idx(v: &JsonValue, i: usize) -> Option<&JsonValue> {
    match v {
        JsonValue::Array(a) => a.get(i),
        _ => None,
    }
}

pub fn text(v: &JsonValue) -> Option<&str> {
    match v {
        JsonValue::String(s) => Some(s.trim()),
        _ => None,
    }
}

/// wttr.in sends every number as a string.
pub fn num(v: &JsonValue, k: &str) -> Option<i64> {
    text(key(v, k)?)?.parse().ok()
}

/// `{"weatherDesc": [{"value": "Sunny"}]}` and similar.
pub fn first_value(v: &JsonValue, k: &str) -> Option<String> {
    text(key(idx(key(v, k)?, 0)?, "value")?).map(str::to_owned)
}
