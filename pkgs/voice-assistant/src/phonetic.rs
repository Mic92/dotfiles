//! What words sound like: espeak-ng phonemes and a distance that makes
//! near-identical sounds ("ch" and "j", "k" and "g") cheap to swap, for
//! finding the command a mangled transcript was meant to be.

use std::ffi::{CStr, CString, c_char, c_int, c_void};
use std::ptr;
use std::sync::{LazyLock, Mutex};

#[link(name = "espeak-ng")]
unsafe extern "C" {
    fn espeak_Initialize(
        output: c_int,
        buflength: c_int,
        path: *const c_char,
        options: c_int,
    ) -> c_int;
    fn espeak_SetVoiceByName(name: *const c_char) -> c_int;
    fn espeak_TextToPhonemes(
        textptr: *mut *const c_void,
        textmode: c_int,
        phonememode: c_int,
    ) -> *const c_char;
}

const AUDIO_OUTPUT_SYNCHRONOUS: c_int = 2;
const INITIALIZE_DONT_EXIT: c_int = 0x8000;
const CHARS_UTF8: c_int = 1;
const PHONEMES_IPA: c_int = 0x02;
/// Bits 8-23 of the phoneme mode: a separator between phonemes.
const SEPARATOR: c_int = (b'_' as c_int) << 8;

/// espeak-ng keeps global state, so one caller at a time. `None` when it
/// failed to start; sound-alikes are then simply not offered.
static ESPEAK: LazyLock<Mutex<Option<()>>> = LazyLock::new(|| {
    // SAFETY: called once, under the lock below, before any other espeak
    // call. A null path means the data compiled into the library.
    let ok = unsafe {
        espeak_Initialize(
            AUDIO_OUTPUT_SYNCHRONOUS,
            0,
            ptr::null(),
            INITIALIZE_DONT_EXIT,
        ) > 0
            && espeak_SetVoiceByName(c"en-us".as_ptr()) == 0
    };
    if !ok {
        eprintln!("voice-assistant: espeak-ng failed to start, no sound-alikes");
    }
    Mutex::new(ok.then_some(()))
});

/// What makes a speech sound close to others.
#[derive(Clone, Copy)]
struct Sound {
    vowel: bool,
    /// Schwa-like sounds that come and go with sloppy speech.
    weak: bool,
    /// Twins that differ only in voicing (`t`/`d`, `tʃ`/`dʒ`).
    twin: u8,
    /// Wider families (hissing sounds, nasals, liquids, stops).
    family: u8,
}

/// Every sound seen so far, indexed by `Phone`.
static SOUNDS: LazyLock<Mutex<Vec<(String, Sound)>>> = LazyLock::new(|| Mutex::new(Vec::new()));

/// A speech sound, as an index into `SOUNDS`.
pub type Phone = u8;

const TWINS: &[&[&str]] = &[
    &["p", "b"],
    &["t", "d", "ɾ"],
    &["k", "g", "ɡ"],
    &["f", "v"],
    &["s", "z"],
    &["ʃ", "ʒ"],
    &["tʃ", "dʒ"],
    &["θ", "ð"],
];
const FAMILIES: &[&[&str]] = &[
    &["s", "z", "ʃ", "ʒ", "tʃ", "dʒ"],
    &["m", "n", "ŋ"],
    &["l", "ɹ", "ɚ", "ɫ"],
    &["p", "t", "k", "b", "d", "g", "ɡ", "ɾ"],
    &["f", "θ", "v", "ð"],
    &["w", "ʊ", "uː"],
    &["j", "i", "iː", "ɪ"],
];

fn group(table: &[&[&str]], name: &str) -> u8 {
    table
        .iter()
        .position(|g| g.contains(&name))
        .and_then(|i| u8::try_from(i + 1).ok())
        .unwrap_or(0)
}

fn sounds() -> std::sync::MutexGuard<'static, Vec<(String, Sound)>> {
    SOUNDS
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
}

/// `None` once all 256 slots are taken, which English never gets near.
fn phone(name: &str) -> Option<Phone> {
    let mut all = sounds();
    let i = match all.iter().position(|(n, _)| n == name) {
        Some(i) => i,
        None => {
            let sound = Sound {
                vowel: name.starts_with(|c| "ɐəɪɛæʊʌɑɔaeiouɚɜᵻ".contains(c)),
                weak: matches!(name, "ɐ" | "ə" | "ɪ" | "ᵻ"),
                twin: group(TWINS, name),
                family: group(FAMILIES, name),
            };
            all.push((name.to_owned(), sound));
            all.len() - 1
        }
    };
    Phone::try_from(i).ok()
}

/// The phonemes of `text`, without stress or word boundaries.
pub fn phonemes(text: &str) -> Vec<Phone> {
    let Ok(text) = CString::new(text) else {
        return Vec::new();
    };
    let lock = ESPEAK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if lock.is_none() {
        return Vec::new();
    }
    let mut ipa = String::new();
    let mut next: *const c_void = text.as_ptr().cast();
    // Each call reads one clause and moves `next` on, to null at the end.
    while !next.is_null() {
        // SAFETY: espeak is initialised and held by this thread; `next`
        // points into `text`, which outlives the loop. The returned string
        // lives in espeak's buffer until the next call, so copy it at once.
        let out = unsafe {
            let p = espeak_TextToPhonemes(&raw mut next, CHARS_UTF8, PHONEMES_IPA | SEPARATOR);
            if p.is_null() {
                break;
            }
            CStr::from_ptr(p).to_string_lossy().into_owned()
        };
        ipa.push(' ');
        ipa.push_str(&out);
    }
    drop(lock);
    ipa.split(|c: char| c == '_' || c.is_whitespace())
        .map(|p| p.trim_matches(|c| matches!(c, 'ˈ' | 'ˌ' | 'ʲ' | '-')))
        .filter(|p| !p.is_empty())
        .filter_map(phone)
        .collect()
}

/// Costs in tenths, so the inner loop compares integers.
type Cost = u16;

fn swap(a: Sound, b: Sound) -> Cost {
    if a.vowel && b.vowel {
        4
    } else if a.twin != 0 && a.twin == b.twin {
        3
    } else if a.family != 0 && a.family == b.family {
        6
    } else {
        10
    }
}

fn gap(p: Sound) -> Cost {
    if p.weak {
        3
    } else if p.vowel {
        6
    } else {
        8
    }
}

/// A transcript to compare many commands against, with the cost of every
/// swap worked out once.
/// Sounds first seen after it was made cost as much as any other.
pub struct Heard {
    phones: Vec<Phone>,
    /// `swaps[i][p]`: replacing the transcript's `i`th sound with `p`.
    swaps: Vec<[Cost; 256]>,
    gaps: [Cost; 256],
}

impl Heard {
    pub fn new(text: &str) -> Self {
        let phones = phonemes(text);
        let all = sounds();
        let mut gaps = [8; 256];
        for (g, (_, s)) in gaps.iter_mut().zip(all.iter()) {
            *g = gap(*s);
        }
        let swaps = phones
            .iter()
            .map(|&x| {
                let mut row = [10; 256];
                let a = all[usize::from(x)].1;
                for (i, (c, (_, b))) in row.iter_mut().zip(all.iter()).enumerate() {
                    *c = if i == usize::from(x) { 0 } else { swap(a, *b) };
                }
                row
            })
            .collect();
        Heard {
            phones,
            swaps,
            gaps,
        }
    }

    pub fn len(&self) -> usize {
        self.phones.len()
    }

    /// What `Lexicon::nearest` computes, one sentence at a time, to check
    /// it against. `None` once it is sure to be above `max`.
    #[cfg(test)]
    pub fn distance(&self, b: &[Phone], max: f32) -> Option<f32> {
        let a = &self.phones;
        let len = a.len().max(b.len()).max(1);
        // In tenths, like the costs.
        #[allow(
            clippy::cast_possible_truncation,
            clippy::cast_sign_loss,
            clippy::cast_precision_loss
        )]
        let budget = (max.min(1000.0) * 10.0 * len as f32) as Cost;
        // Every phone one side has more costs at least the cheapest gap.
        if a.len().abs_diff(b.len()) * 3 > usize::from(budget) {
            return None;
        }
        ROW.with_borrow_mut(|row| {
            // One row, updated in place: before the update `row[j]` is the
            // cell above, after it the cell itself.
            row.clear();
            row.push(0);
            let mut total = 0;
            for &y in b {
                total += self.gaps[usize::from(y)];
                row.push(total);
            }
            let (first, rest) = row.split_first_mut()?;
            for (&x, swaps) in a.iter().zip(&self.swaps) {
                let gx = self.gaps[usize::from(x)];
                let mut diag = *first;
                let mut left = diag + gx;
                *first = left;
                let mut low = left;
                for (&y, cell) in b.iter().zip(rest.iter_mut()) {
                    let y = usize::from(y);
                    let up = *cell;
                    left = (diag + swaps[y]).min(up + gx).min(left + self.gaps[y]);
                    diag = up;
                    *cell = left;
                    low = low.min(left);
                }
                // Costs only grow down the table, so a row's best bounds the end.
                if low > budget {
                    return None;
                }
            }
            #[allow(clippy::cast_precision_loss)]
            let d = f32::from(*rest.last().unwrap_or(first)) / 10.0 / len as f32;
            (d <= max).then_some(d)
        })
    }
}

/// Many sentences sharing their beginnings ("abort my timer", "abort the
/// timer"), as a trie in depth-first order, so a shared beginning is
/// compared once.
pub struct Lexicon {
    nodes: Vec<Node>,
}

struct Node {
    phone: Phone,
    depth: usize,
    /// The node after this one's subtree.
    skip: usize,
    /// The longest sentence in the subtree, in phones.
    longest: usize,
    /// The sentences that end here; several can sound the same.
    ends: Vec<usize>,
}

impl Lexicon {
    pub fn new(sentences: impl IntoIterator<Item = Vec<Phone>>) -> Self {
        let mut all: Vec<(Vec<Phone>, usize)> = sentences
            .into_iter()
            .enumerate()
            .map(|(i, p)| (p, i))
            .collect();
        all.sort();
        let mut nodes: Vec<Node> = Vec::new();
        // The open path: node index at each depth.
        let mut path: Vec<usize> = Vec::new();
        let mut last: &[Phone] = &[];
        for (phones, id) in &all {
            let shared = last.iter().zip(phones).take_while(|(a, b)| a == b).count();
            for &open in &path[shared..] {
                nodes[open].skip = nodes.len();
            }
            path.truncate(shared);
            for (depth, &phone) in phones.iter().enumerate().skip(shared) {
                path.push(nodes.len());
                nodes.push(Node {
                    phone,
                    depth: depth + 1,
                    skip: 0,
                    longest: 0,
                    ends: Vec::new(),
                });
            }
            for &open in &path {
                nodes[open].longest = nodes[open].longest.max(phones.len());
            }
            if let Some(&end) = path.last() {
                nodes[end].ends.push(*id);
            }
            last = phones;
        }
        for &open in &path {
            nodes[open].skip = nodes.len();
        }
        Lexicon { nodes }
    }

    /// The `limit` sentences closest to `heard` within `max`, closest
    /// first. The distance is a weighted edit distance over the longer
    /// length: 0 for the same sounds, around 0.3 for a mishearing of the
    /// same words, more for other words.
    pub fn nearest(&self, heard: &Heard, max: f32, limit: usize) -> Vec<(f32, usize)> {
        let a = &heard.phones;
        let mut best: Vec<(f32, usize)> = Vec::with_capacity(limit + 1);
        // cols[d]: the cost of each prefix of `heard` against the path's
        // first d phones.
        let mut cols: Vec<Vec<Cost>> = vec![Vec::with_capacity(a.len() + 1)];
        let mut total = 0;
        cols[0].push(0);
        for &x in a {
            total += heard.gaps[usize::from(x)];
            cols[0].push(total);
        }
        let mut i = 0;
        while let Some(node) = self.nodes.get(i) {
            let limit_now = match best.get(limit.saturating_sub(1)) {
                Some((d, _)) if best.len() >= limit => *d,
                _ => max,
            };
            if cols.len() <= node.depth {
                cols.push(Vec::with_capacity(a.len() + 1));
            }
            let (done, open) = cols.split_at_mut(node.depth);
            let (prev, col) = (&done[node.depth - 1], &mut open[0]);
            let y = usize::from(node.phone);
            let gy = heard.gaps[y];
            col.clear();
            let mut left = prev[0] + gy;
            col.push(left);
            let mut low = left;
            for (((&x, swaps), &diag), &up) in
                a.iter().zip(&heard.swaps).zip(prev.iter()).zip(&prev[1..])
            {
                left = (diag + swaps[y])
                    .min(up + gy)
                    .min(left + heard.gaps[usize::from(x)]);
                col.push(left);
                low = low.min(left);
            }
            let len = a.len().max(node.longest).max(1);
            #[allow(clippy::cast_precision_loss)]
            if f32::from(low) / 10.0 / len as f32 > limit_now {
                i = node.skip;
                continue;
            }
            #[allow(clippy::cast_precision_loss)]
            let d = f32::from(left) / 10.0 / a.len().max(node.depth).max(1) as f32;
            for &id in &node.ends {
                if d <= limit_now {
                    let at = best.partition_point(|(e, _)| *e <= d);
                    best.insert(at, (d, id));
                    best.truncate(limit);
                }
            }
            i += 1;
        }
        best
    }
}

#[cfg(test)]
thread_local! {
    static ROW: std::cell::RefCell<Vec<Cost>> = const { std::cell::RefCell::new(Vec::new()) };
}

#[cfg(test)]
mod tests {
    use super::*;

    fn d(a: &str, b: &str) -> f32 {
        Heard::new(a)
            .distance(&phonemes(b), f32::MAX)
            .unwrap_or(f32::MAX)
    }

    #[test]
    fn mishearings_are_close() {
        assert_eq!(d("tell a joke", "tell a joke"), 0.0);
        for (heard, meant) in [
            ("tell it chalk", "tell a joke"),
            ("tell a choke", "tell a joke"),
            ("Peng Yif", "ping eve"),
        ] {
            let near = d(heard, meant);
            let far = d(heard, "what time is it");
            assert!(near < 0.5 && near < far, "{heard}: {near} vs {far}");
        }
    }

    #[test]
    fn phonemes_split() {
        assert_eq!(phonemes("joke").len(), 3, "dʒ oʊ k");
        assert!(phonemes("").is_empty());
    }
}
