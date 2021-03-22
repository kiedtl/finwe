use crate::*;
use pest::error::Error;
use pest::Parser;
use pest::iterators::{Pair, Pairs};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct ZfParser;

pub fn parse(env: &mut ZfEnv, source: &str)
    -> Result<Vec<ZfToken>, Error<Rule>>
{
    let pairs = ZfParser::parse(Rule::program, source)?;
    parse_pairs(env, pairs)
}

fn parse_pairs(env: &mut ZfEnv, pairs: Pairs<Rule>)
    -> Result<Vec<ZfToken>, Error<Rule>>
{
    let mut ast = vec![];
    for pair in pairs {
        match parse_term(env, pair) {
            Some(i) => ast.push(i),
            None => (),
        }
    }
    Ok(ast)
}

fn parse_term(env: &mut ZfEnv, pair: Pair<Rule>) -> Option<ZfToken> {
    match pair.as_rule() {
        Rule::EOI => None,
        Rule::word_decl => {
            let mut items = pair.into_inner();

            let name;
            let ident = parse_term(env, items.nth(0).unwrap()).unwrap();
            if let ZfToken::Ident(s) = ident {
                name = s;
            } else { unreachable!() }

            let body = items
                .skip(0)
                .map(|p| parse_term(env, p))
                .filter(|i| i.is_some())
                .map(|i| i.unwrap())
                .collect::<Vec<_>>();
            env.addword(name, body);
            None
        }
        Rule::quote => {
            let quote = pair.into_inner()
                .map(|p| parse_term(env, p))
                .filter(|i| i.is_some())
                .map(|i| i.unwrap())
                .collect::<Vec<_>>();
            let _ref = env.addword(random::phrase(), quote);
            Some(ZfToken::SymbRef(_ref))
        }
        Rule::float => {
            // FIXME: Rust's parse::<f64> doesn't support the following formats:
            //  - 0__.0__e+10__, 0x2351, 0o261, 0b100111, 100_000_000
            // At some point, a custom float-parsing function should be made.
            let dstr = pair.as_str();
            let (sign, dstr) = match &dstr[..1] {
                "_" => (-1.0, &dstr[1..]),
                _ => (1.0, &dstr[..]),
            };
            let mut flt: f64 = dstr.parse().unwrap();
            if flt != 0.0 {
                // Avoid negative zeroes; only multiply sign by nonzeroes.
                flt *= sign;
            }
            Some(ZfToken::Number(flt))
        }
        // TODO: escape sequences: \r \n \a \b \f \t \v \0 \x00 \uXXXX &c
        Rule::string => {
            let str = &pair.as_str();
            // Strip leading and ending quotes.
            let str = &str[1..str.len() - 1];
            // Escaped string quotes become single quotes here.
            let str = str.replace("''", "'");
            Some(ZfToken::String(str[..].to_owned()))
        }
        Rule::character => {
            let ch = &pair.as_str();
            let ch = &ch[1..].chars().next().unwrap();
            Some(ZfToken::Number(*ch as u32 as f64))
        }
        Rule::reference => {
            let ident = pair.as_str().to_owned();
            match env.findword(&ident[1..]) {
                Some(i) => Some(ZfToken::SymbRef(i)),
                None => panic!("bad reference {}", ident),
            }
        }
        Rule::word => {
            let ident = pair.as_str().to_owned();
            match env.findword(&ident) {
                Some(i) => Some(ZfToken::Symbol(i)),
                None => panic!("unknown word {}", ident),
            }
        }
        Rule::fetch => Some(ZfToken::Fetch(pair.as_str()[1..].to_owned())),
        Rule::store => Some(ZfToken::Store(pair.as_str()[1..].to_owned())),
        Rule::ident => Some(ZfToken::Ident(pair.as_str().to_owned())),
        Rule::guard => {
            let mut inner = pair.into_inner();
            assert!(inner.clone().count() == 2);

            let mut guardsets = vec![];
            for _ in 0..=1 {
                let mut guardset = vec![];
                let innerset = inner.next().unwrap().into_inner();
                for minion in innerset {
                    guardset.push(match minion.as_str() {
                        "a" => GuardItem::Any,
                        "n" => GuardItem::Number,
                        "s" => GuardItem::Str,
                        "q" => GuardItem::Quote,
                        "*" => GuardItem::Unchecked,
                        _   => panic!("welp"),
                    });
                }
                guardsets.push(guardset);
            }

            Some(ZfToken::Guard {
                before: guardsets[0].clone(),
                after:  guardsets[1].clone()
            })
        }
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_float_literal() {
        parses_to! { parser: ZfParser, input: "123", rule: Rule::float,
            tokens: [ float(0, 3) ] };
        parses_to! { parser: ZfParser, input: "0.", rule: Rule::float,
            tokens: [ float(0, 2) ] };
        parses_to! { parser: ZfParser, input: "1e10", rule: Rule::float,
            tokens: [ float(0, 4) ] };
        parses_to! { parser: ZfParser, input: "0.e0", rule: Rule::float,
            tokens: [ float(0, 4) ] };
        parses_to! { parser: ZfParser, input: "0_0.0e+10", rule: Rule::float,
            tokens: [ float(0, 9) ] };
    }

    #[test]
    fn test_word() {
        parses_to! { parser: ZfParser, input: "drop", rule: Rule::program,
            tokens: [word(0, 4)] };
        parses_to! { parser: ZfParser, input: "2dup", rule: Rule::program,
            tokens: [word(0, 4)] };
        parses_to! { parser: ZfParser, input: "+", rule: Rule::program,
            tokens: [word(0, 1)] };
        parses_to! { parser: ZfParser, input: "1+", rule: Rule::program,
            tokens: [word(0, 2)] };
        parses_to! { parser: ZfParser, input: "test[", rule: Rule::program,
            tokens: [word(0, 5)] };
    }
}
