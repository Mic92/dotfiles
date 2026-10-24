//! BERT WordPiece tokenizer (uncased) from `vocab.txt`.

use std::collections::HashMap;

const MAX_TOKENS: usize = 128;

pub struct WordPiece {
    ids: HashMap<String, i64>,
    unk: i64,
    cls: i64,
    sep: i64,
}

impl WordPiece {
    pub fn parse(vocab: &str) -> Result<Self, String> {
        let ids: HashMap<String, i64> = vocab
            .lines()
            .zip(0..)
            .map(|(t, i)| (t.to_owned(), i))
            .collect();
        let id = |t: &str| ids.get(t).copied().ok_or(format!("vocab lacks {t}"));
        Ok(WordPiece {
            unk: id("[UNK]")?,
            cls: id("[CLS]")?,
            sep: id("[SEP]")?,
            ids,
        })
    }

    fn word(&self, word: &str, out: &mut Vec<i64>) {
        let start = out.len();
        let mut rest = word;
        let mut first = true;
        while !rest.is_empty() {
            let piece = rest
                .char_indices()
                .map(|(i, c)| i + c.len_utf8())
                .rev()
                .find_map(|end| {
                    let p = &rest[..end];
                    let key = if first {
                        p.to_owned()
                    } else {
                        format!("##{p}")
                    };
                    Some((end, *self.ids.get(&key)?))
                });
            let Some((end, id)) = piece else {
                out.truncate(start);
                out.push(self.unk);
                return;
            };
            out.push(id);
            rest = &rest[end..];
            first = false;
        }
    }

    /// `[CLS] pieces [SEP]`, truncated to the model's 128 positions.
    pub fn encode(&self, text: &str) -> Vec<i64> {
        let lower = text.to_lowercase();
        let mut out = vec![self.cls];
        for chunk in lower.split_whitespace() {
            let mut word = String::new();
            for c in chunk.chars() {
                if c.is_alphanumeric() {
                    word.push(c);
                } else {
                    self.word(&word, &mut out);
                    word.clear();
                    self.word(&c.to_string(), &mut out);
                }
            }
            self.word(&word, &mut out);
        }
        out.truncate(MAX_TOKENS - 1);
        out.push(self.sep);
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn splits_words_and_punctuation() -> Result<(), String> {
        let v = WordPiece::parse("[PAD]\n[UNK]\n[CLS]\n[SEP]\nplay\n##ing\nwhat\n'\ns\n?")?;
        assert_eq!(v.encode("Playing what's?"), [2, 4, 5, 6, 7, 8, 9, 3]);
        assert_eq!(v.encode("xyz"), [2, 1, 3]);
        Ok(())
    }
}
