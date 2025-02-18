use super::{Lint, LintKind, PatternLinter};
use crate::linting::Suggestion;
use crate::patterns::{All, Invert, OwnedPatternExt, Pattern, SequencePattern, WordSet};
use crate::Token;

#[doc = "Corrects the misuse of `then` to `than`."]
pub struct ThenThan {
    pattern: Box<dyn Pattern>,
}

impl ThenThan {
    pub fn new() -> Self {
        Self {
            pattern: Box::new(All::new(vec![
                Box::new(
                    SequencePattern::default()
                        .then(Box::new(WordSet::all(&["better", "other"]).or(Box::new(
                            |tok: &Token, _source: &[char]| tok.kind.is_adjective(),
                        ))))
                        .then_whitespace()
                        .then_any_capitalization_of("then"),
                ),
                // Denotes exceptions to the rule.
                Box::new(Invert::new(Box::new(WordSet::all(&["back"])))),
            ])),
        }
    }
}

impl Default for ThenThan {
    fn default() -> Self {
        Self::new()
    }
}

impl PatternLinter for ThenThan {
    fn pattern(&self) -> &dyn Pattern {
        self.pattern.as_ref()
    }
    fn match_to_lint(&self, matched_tokens: &[Token], source: &[char]) -> Option<Lint> {
        let span = matched_tokens.last()?.span;
        let offending_text = span.get_content(source);

        Some(Lint {
            span,
            lint_kind: LintKind::Miscellaneous,
            suggestions: vec![Suggestion::replace_with_match_case(
                "than".chars().collect(),
                offending_text,
            )],
            message: "Did you mean `than`?".to_string(),
            priority: 31,
        })
    }
    fn description(&self) -> &'static str {
        "Corrects the misuse of `then` to `than`."
    }
}

#[cfg(test)]
mod tests {
    use super::ThenThan;
    use crate::linting::tests::{assert_lint_count, assert_suggestion_result};

    #[test]
    fn allows_back_then() {
        assert_lint_count("I was a gross kid back then.", ThenThan::default(), 0);
    }

    #[test]
    fn catches_shorter_then() {
        assert_suggestion_result(
            "One was shorter then the other.",
            ThenThan::default(),
            "One was shorter than the other.",
        );
    }

    #[test]
    fn catches_better_then() {
        assert_suggestion_result(
            "One was better then the other.",
            ThenThan::default(),
            "One was better than the other.",
        );
    }

    #[test]
    fn catches_longer_then() {
        assert_suggestion_result(
            "One was longer then the other.",
            ThenThan::default(),
            "One was longer than the other.",
        );
    }

    #[test]
    fn catches_less_then() {
        assert_suggestion_result(
            "I eat less then you.",
            ThenThan::default(),
            "I eat less than you.",
        );
    }

    #[test]
    fn catches_more_then() {
        assert_suggestion_result(
            "I eat more then you.",
            ThenThan::default(),
            "I eat more than you.",
        );
    }

    #[test]
    fn stronger_should_change() {
        assert_suggestion_result(
            "a chain is no stronger then its weakest link",
            ThenThan::default(),
            "a chain is no stronger than its weakest link",
        );
    }

    #[test]
    fn half_a_loaf_should_change() {
        assert_suggestion_result(
            "half a loaf is better then no bread",
            ThenThan::default(),
            "half a loaf is better than no bread",
        );
    }

    #[test]
    fn then_everyone_clapped_should_be_allowed() {
        assert_lint_count("and then everyone clapped", ThenThan::default(), 0);
    }

    #[test]
    fn crazier_than_rat_should_change() {
        assert_suggestion_result(
            "crazier then a shithouse rat",
            ThenThan::default(),
            "crazier than a shithouse rat",
        );
    }

    #[test]
    fn poke_in_eye_should_change() {
        assert_suggestion_result(
            "better then a poke in the eye with a sharp stick",
            ThenThan::default(),
            "better than a poke in the eye with a sharp stick",
        );
    }

    #[test]
    fn other_then_should_change() {
        assert_suggestion_result(
            "There was no one other then us at the campsite.",
            ThenThan::default(),
            "There was no one other than us at the campsite.",
        );
    }

    #[test]
    fn allows_and_then() {
        assert_lint_count("And then we left.", ThenThan::default(), 0);
    }

    #[test]
    fn allows_this_then() {
        assert_lint_count("Do this then that.", ThenThan::default(), 0);
    }
}
