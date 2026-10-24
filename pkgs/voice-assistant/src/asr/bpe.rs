//! SentencePiece BPE from parakeet's `tokens.txt` (`piece id` per line).
//! Pieces are listed in merge order, so the id is the merge priority.

use std::collections::HashMap;

pub struct Bpe {
    pieces: Vec<String>,
    ids: HashMap<String, usize>,
    unk: usize,
}

const SPACE: char = '\u{2581}';

impl Bpe {
    pub fn parse(tokens: &str) -> Result<Self, String> {
        let mut pieces = Vec::new();
        for line in tokens.lines() {
            let (piece, id) = line
                .rsplit_once(' ')
                .ok_or_else(|| format!("bad tokens.txt line {line:?}"))?;
            if id.parse::<usize>().ok() != Some(pieces.len()) {
                return Err(format!("tokens.txt out of order at {line:?}"));
            }
            pieces.push(piece.to_owned());
        }
        let ids = pieces
            .iter()
            .enumerate()
            .filter(|(_, p)| !(p.starts_with('<') && p.ends_with('>')))
            .map(|(i, p)| (p.clone(), i))
            .collect();
        let unk = pieces.iter().position(|p| p == "<unk>").unwrap_or(0);
        Ok(Bpe { pieces, ids, unk })
    }

    /// Size including the blank, which comes last.
    pub fn len(&self) -> usize {
        self.pieces.len()
    }

    pub fn encode(&self, text: &str) -> Vec<usize> {
        let words: Vec<&str> = text.split_whitespace().collect();
        let mut syms: Vec<String> = format!("{SPACE}{}", words.join(&SPACE.to_string()))
            .chars()
            .map(String::from)
            .collect();
        // Merge the adjacent pair that forms the earliest-learned piece.
        loop {
            let best = syms
                .windows(2)
                .enumerate()
                .filter_map(|(i, w)| Some((self.ids.get(&format!("{}{}", w[0], w[1]))?, i)))
                .min();
            let Some((_, i)) = best else { break };
            let merged = format!("{}{}", syms[i], syms[i + 1]);
            syms.splice(i..i + 2, [merged]);
        }
        syms.iter()
            .map(|s| self.ids.get(s).copied().unwrap_or(self.unk))
            .collect()
    }

    pub fn decode(&self, ids: &[usize]) -> String {
        ids.iter()
            .filter_map(|&i| self.pieces.get(i))
            .map(|p| p.replace(SPACE, " "))
            .collect::<String>()
            .trim()
            .to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn toy() -> Bpe {
        let pieces = [
            "<unk>", "▁t", "he", "▁the", "▁a", "e", "h", "t", "a", "▁", "<blk>",
        ];
        let txt: String = pieces
            .iter()
            .enumerate()
            .map(|(i, p)| format!("{p} {i}\n"))
            .collect();
        Bpe::parse(&txt).unwrap_or_else(|e| panic!("{e}"))
    }

    #[test]
    fn merges_in_learned_order() {
        let b = toy();
        // "▁t" merges first, then "he", then the two into "▁the".
        assert_eq!(b.encode("the"), [3]);
        assert_eq!(b.encode("the  a"), [3, 4]);
        assert_eq!(b.decode(&b.encode("the a")), "the a");
        // Unknown letters fall back to <unk>. Control pieces never match.
        assert_eq!(b.encode("x"), [9, 0]);
        assert_eq!(b.len(), 11);
    }

    #[test]
    fn rejects_shuffled_tokens() {
        assert!(Bpe::parse("a 1\n").is_err());
    }
}
