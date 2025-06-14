use crate::expr::Expr;
use crate::expr::LongestMatchOf;
use crate::expr::SequenceExpr;
use crate::{
    CharStringExt, Token,
    linting::{ExprLinter, Lint, LintKind, Suggestion},
    patterns::WordSet,
};

pub struct PronounKnew {
    expr: Box<dyn Expr>,
}

trait PronounKnewExt {
    fn then_pronoun(self) -> Self;
}

impl Default for PronounKnew {
    fn default() -> Self {
        // The pronoun that would occur before a verb would be a subject pronoun.
        // But "its" commonly occurs before "new" and is a possessive pronoun. (Much more commonly a determiner)
        // Since "his" and "her" are possessive and object pronouns respectively, we ignore them too.
        let pronoun_pattern = |tok: &Token, source: &[char]| {
            if !tok.kind.is_pronoun() {
                return false;
            }

            let pronorm = tok.span.get_content_string(source).to_lowercase();
            let excluded = ["its", "his", "her", "every", "something", "nothing"];
            !excluded.contains(&&*pronorm)
        };

        // "{pronoun} new {noun}" is a mistake in most cases, but allowing it here prevents us from
        // incorrectly flagging text like "To learn some new tricks."
        // "{pronoun} new {not-noun}"
        let pronoun_new_notnoun = SequenceExpr::default()
            .then(pronoun_pattern)
            .then_whitespace()
            .then_any_capitalization_of("new")
            .then_whitespace()
            .then_anything_but_noun();

        // To catch mistakes like:
        // "Though some new that was the case."
        // "She new danger lurked nearby."
        // "{pronoun} new {noun} {verb}"
        let pronoun_new_noun_verb = SequenceExpr::default()
            .then(pronoun_pattern)
            .then_whitespace()
            .then_any_capitalization_of("new")
            .then_whitespace()
            .then_noun()
            .then_whitespace()
            .then_verb();

        // "{pronoun} {always|never|also|often} new"
        let pronoun_adverb_new = SequenceExpr::default()
            .then(pronoun_pattern)
            .then_whitespace()
            .then(WordSet::new(&["always", "never", "also", "often"]))
            .then_whitespace()
            .then_any_capitalization_of("new");

        let combined_pattern = LongestMatchOf::new(vec![
            Box::new(pronoun_new_notnoun),
            Box::new(pronoun_new_noun_verb),
            Box::new(pronoun_adverb_new),
        ]);

        Self {
            expr: Box::new(combined_pattern),
        }
    }
}

impl ExprLinter for PronounKnew {
    fn expr(&self) -> &dyn Expr {
        self.expr.as_ref()
    }

    fn match_to_lint(&self, tokens: &[Token], source: &[char]) -> Option<Lint> {
        // This feels hacky. It could potentially work incorrectly when there are multiple 'new's in
        // the match.
        let typo_token = tokens
            .iter()
            .find(|t| *t.span.get_content(source).to_lower() == ['n', 'e', 'w'])?;
        let typo_span = typo_token.span;
        let typo_text = typo_span.get_content(source);

        Some(Lint {
            span: typo_span,
            lint_kind: LintKind::WordChoice,
            suggestions: vec![Suggestion::replace_with_match_case(
                "knew".chars().collect(),
                typo_text,
            )],
            message: "Did you mean “knew” (the past tense of “know”)?".to_string(),
            priority: 31,
        })
    }

    fn description(&self) -> &str {
        "Detects when “new” following a pronoun (optionally with an adverb) is a typo for the past tense “knew.”"
    }
}

#[cfg(test)]
mod tests {
    use super::PronounKnew;
    use crate::linting::tests::{assert_lint_count, assert_suggestion_result};

    #[test]
    fn simple_pronoun_new() {
        assert_suggestion_result(
            "I new you would say that.",
            PronounKnew::default(),
            "I knew you would say that.",
        );
    }

    #[test]
    fn with_adverb() {
        assert_suggestion_result(
            "She often new the answer.",
            PronounKnew::default(),
            "She often knew the answer.",
        );
    }

    #[test]
    fn does_not_flag_without_pronoun() {
        assert_lint_count("The software is new.", PronounKnew::default(), 0);
    }

    #[test]
    fn does_not_flag_other_context() {
        assert_lint_count("They called it \"new\".", PronounKnew::default(), 0);
    }

    #[test]
    fn does_not_flag_with_its() {
        assert_lint_count(
            "In 2015, the US was paying on average around 2% for its new issuance bonds.",
            PronounKnew::default(),
            0,
        );
    }

    #[test]
    fn does_not_flag_with_his() {
        assert_lint_count("His new car is fast.", PronounKnew::default(), 0);
    }

    #[test]
    fn does_not_flag_with_her() {
        assert_lint_count("Her new car is fast.", PronounKnew::default(), 0);
    }

    #[test]
    fn does_not_flag_with_nothing_1298() {
        assert_lint_count("This is nothing new.", PronounKnew::default(), 0);
    }

    #[test]
    fn does_not_flag_with_some_1381() {
        assert_lint_count("To learn some new tricks.", PronounKnew::default(), 0);
    }

    #[test]
    fn flags_she_new_danger() {
        assert_lint_count("She new danger lurked nearby.", PronounKnew::default(), 1);
    }

    // I think both "new" and "knew" would be correct here, so this should probably not be marked
    // as a mistake. This test ensures that.
    // Dealing with "some" is tricky, seemingly because it can function as both a pronoun and
    // a determiner.
    #[test]
    fn does_not_flag_with_some_new_tricks_were() {
        assert_lint_count(
            "Some new tricks were being developed.",
            PronounKnew::default(),
            0,
        );
    }

    #[test]
    fn flags_some_new_that() {
        assert_suggestion_result(
            "Though some new that was the case.",
            PronounKnew::default(),
            "Though some knew that was the case.",
        );
    }
}
