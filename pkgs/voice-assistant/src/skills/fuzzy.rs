//! Sound-alike matching for names whisper mangles ("Laslos" for
//! "lassulus", "Yif" for "eve"): compare a crude phonetic skeleton by
//! edit distance.

/// Lowercase letters and digits, every vowel the same, voiced and
/// voiceless consonants merged, no doubled letters.
pub fn skeleton(s: &str) -> Vec<char> {
    let mut out: Vec<char> = Vec::new();
    for c in s.chars().filter(char::is_ascii_alphanumeric) {
        let c = match c.to_ascii_lowercase() {
            'a' | 'e' | 'i' | 'o' | 'u' | 'y' => 'a',
            // Voiced and voiceless twins sound alike through a headset.
            'f' => 'v',
            'p' => 'b',
            't' => 'd',
            'k' | 'c' | 'q' => 'g',
            's' => 'z',
            c => c,
        };
        if out.last() != Some(&c) {
            out.push(c);
        }
    }
    out
}

pub fn distance(a: &[char], b: &[char]) -> usize {
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    for (i, ca) in a.iter().enumerate() {
        let mut cur = vec![i + 1];
        for (j, cb) in b.iter().enumerate() {
            let sub = prev[j] + usize::from(ca != cb);
            cur.push(sub.min(prev[j + 1] + 1).min(cur[j] + 1));
        }
        prev = cur;
    }
    prev[b.len()]
}

/// The candidate id closest to `heard`, if close enough and unambiguous.
pub fn closest<'a>(
    heard: &[&str],
    candidates: impl IntoIterator<Item = (usize, &'a [&'a str])>,
) -> Option<usize> {
    let h = skeleton(&heard.concat());
    if h.len() < 2 {
        return None;
    }
    let mut best: Option<(usize, usize)> = None;
    let mut tie = false;
    for (id, words) in candidates {
        let c = skeleton(&words.concat());
        let d = distance(&h, &c);
        // About one edit per four sounds.
        if d > c.len().div_ceil(4) {
            continue;
        }
        match best {
            Some((bd, bid)) if d == bd && bid != id => tie = true,
            Some((bd, _)) if d >= bd => {}
            _ => {
                best = Some((d, id));
                tie = false;
            }
        }
    }
    best.filter(|_| !tie).map(|(_, id)| id)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sound_alikes() {
        let names: &[(usize, &[&str])] = &[
            (0, &["lassulus"]),
            (1, &["fm4"]),
            (2, &["deutschlandfunk"]),
            (3, &["bayern", "3"]),
        ];
        let find = |h: &[&str]| closest(h, names.iter().copied());
        assert_eq!(find(&["laslos"]), Some(0));
        assert_eq!(find(&["lasso", "lose"]), Some(0));
        assert_eq!(find(&["fm"]), Some(1));
        assert_eq!(find(&["deutschland", "funk"]), Some(2));
        assert_eq!(find(&["bayern", "drei"]), None);
        assert_eq!(find(&["pizza"]), None);
        assert_eq!(find(&["a"]), None);
        let hosts: &[(usize, &[&str])] = &[
            (0, &["eve"]),
            (1, &["eva"]),
            (2, &["evo"]),
            (3, &["blob", "64"]),
        ];
        assert_eq!(
            closest(&["yif"], hosts.iter().copied()),
            None,
            "eve, eva and evo tie"
        );
        assert_eq!(distance(&skeleton("kitten"), &skeleton("sitting")), 2);
    }

    #[test]
    fn ambiguous_is_none() {
        let names: &[(usize, &[&str])] = &[(0, &["eve"]), (1, &["eva"])];
        assert_eq!(closest(&["ava"], names.iter().copied()), None);
    }
}
