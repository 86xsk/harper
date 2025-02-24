use crate::linting::{Lint, LintKind, Linter, Suggestion};
use crate::{CharString, Document, Punctuation, Span, Token, TokenKind};

#[derive(Debug, PartialEq, PartialOrd, Clone)]
struct PatternToken {
    /// The general variant of the token.
    /// The inner data of the [`TokenKind`] should be replaced with the default
    /// value.
    kind: TokenKind,
    content: Option<CharString>,
}

impl PatternToken {
    fn from_token(token: Token, document: &Document) -> Self {
        if token.kind.is_word() {
            Self {
                kind: token.kind.with_default_data(),
                content: Some(document.get_span_content(token.span).into()),
            }
        } else {
            Self {
                kind: token.kind,
                content: None,
            }
        }
    }
}

macro_rules! vecword {
    ($lit:literal) => {
        $lit.chars().collect()
    };
}

macro_rules! pt {
    ($str:literal) => {
        PatternToken {
            kind: TokenKind::Word(None),
            content: Some($str.chars().collect()),
        }
    };
    (Word) => {
        PatternToken {
            kind: TokenKind::Word,
            content: None,
        }
    };
    (Period) => {
        PatternToken {
            kind: TokenKind::Punctuation(Punctuation::Period),
            content: None,
        }
    };
    (Hyphen) => {
        PatternToken {
            kind: TokenKind::Punctuation(Punctuation::Hyphen),
            content: None,
        }
    };
    (Space) => {
        PatternToken {
            kind: TokenKind::Space(1),
            content: None,
        }
    };
    ( $($($str:literal),* => $repl:literal),*) => {
        vec![
            $(
                {
                    let mut rule = Rule {
                        pattern: vec![$(
                            pt!($str),
                            pt!(Space),
                        )*],
                        replace_with: $repl.chars().collect()
                    };

                    if rule.pattern.len() > 0{
                        rule.pattern.pop();
                    }

                    rule
                },
            )*
        ]
    };
}

struct Rule {
    pattern: Vec<PatternToken>,
    replace_with: Vec<char>,
}

/// A linter that uses a variety of curated pattern matches to find and fix
/// common grammatical issues.
pub struct Matcher {
    triggers: Vec<Rule>,
}

impl Matcher {
    pub fn new() -> Self {
        // This match list needs to be automatically expanded instead of explicitly
        // defined like it is now.
        let mut triggers = Vec::new();

        // hyphenated phrases
        triggers.extend(pt! {
            "case", "sensitive" => "case-sensitive",
            "chock","full" => "chock-full",
            "ngram" => "n-gram",
            "off","the","cuff" => "off-the-cuff",
            "Tree", "sitter" => "Tree-sitter",
            "wellbeing" => "well-being"
        });

        // expand abbreviations
        triggers.extend(pt! {
            "dep" => "dependency",
            "deps" => "dependencies",
            "min" => "minimum",
            "stdin" => "standard input",
            "stdout" => "standard output",
            "w/" => "with",
            "w/o" => "without"
        });

        // expand compound words
        triggers.extend(pt! {
            "hashmap" => "hash map",
            "hashtable" => "hash table",
            "wordlist" => "word list"
        });

        // mixing up than/then in context
        triggers.extend(pt! {
            "then","her" => "than her",
            "then","hers" => "than hers",
            "then","him" => "than him",
            "then","his" => "than his",
            "then","last","week" => "than last week"
        });

        // not a perfect fit for any of the other categories
        triggers.extend(pt! {
            "performing","this" => "perform this",
            "simply","grammatical" => "simple grammatical",
            "the","challenged" => "that challenged",
            "to", "towards" => "towards",
            "To-Do" => "To-do",
            "todo" => "to-do"
        });

        // wrong set phrases and collocations
        triggers.extend(pt! {
            "discuss", "about" => "discuss",
            "discussed", "about" => "discussed",
            "discusses", "about" => "discusses",
            "discussing", "about" => "discussing"
        });

        // normalization
        triggers.extend(pt! {
            "world","war","2" => "World War II",
            "world","War","ii" => "World War II",
            "World","war","ii" => "World War II",
            "World","War","iI" => "World War II",
            "World","War","Ii" => "World War II"
        });

        triggers.push(Rule {
            pattern: vec![pt!("L"), pt!(Period), pt!("L"), pt!(Period), pt!("M")],
            replace_with: vecword!("large language model"),
        });

        triggers.push(Rule {
            pattern: vec![
                pt!("L"),
                pt!(Period),
                pt!("L"),
                pt!(Period),
                pt!("M"),
                pt!(Period),
            ],
            replace_with: vecword!("large language model"),
        });

        Self { triggers }
    }
}

impl Default for Matcher {
    fn default() -> Self {
        Self::new()
    }
}

impl Linter for Matcher {
    fn lint(&mut self, document: &Document) -> Vec<Lint> {
        let mut lints = Vec::new();

        let mut match_tokens = Vec::new();

        for (index, _) in document.tokens().enumerate() {
            for trigger in &self.triggers {
                match_tokens.clear();

                for (p_index, pattern) in trigger.pattern.iter().enumerate() {
                    let Some(token) = document.get_token(index + p_index) else {
                        break;
                    };

                    let t_pattern = PatternToken::from_token(token, document);

                    if t_pattern != *pattern {
                        break;
                    }

                    match_tokens.push(token);
                }

                if match_tokens.len() == trigger.pattern.len() && !match_tokens.is_empty() {
                    let span = Span::new(
                        match_tokens.first().unwrap().span.start,
                        match_tokens.last().unwrap().span.end,
                    );

                    lints.push(Lint {
                        span,
                        lint_kind: LintKind::Miscellaneous,
                        suggestions: vec![Suggestion::ReplaceWith(trigger.replace_with.to_owned())],
                        message: format!(
                            "Did you mean “{}”?",
                            trigger.replace_with.iter().collect::<String>()
                        ),
                        priority: 15,
                    })
                }
            }
        }

        lints
    }

    fn description(&self) -> &'static str {
        "A collection of curated rules. A catch-all that will be removed in the future."
    }
}
