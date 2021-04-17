use crate::*;
use pest::error::Error;
use pest::Parser;
use pest::iterators::{Pair, Pairs};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct ZfParser;

#[cfg(test)]
mod parser_tests {
    use super::*;
    use super::Node::*;

    #[test]
    fn test_basic() {
        let s = "1 2 3 \"test\" abcd 99 'b'";
        assert_eq!(do_parse(s).unwrap(), vec![
            Number(1.), Number(2.), Number(3.), Str("test".to_owned()),
            Word("abcd".to_owned()), Number(99.), Char(98.)
        ]);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CondArm { Is(Vec<Node>), Any }

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Decl((String, Vec<Node>)),
    Quote(Vec<Node>),
    Number(f64),
    Str(String),
    Char(f64),
    Stack(String),
    Fetch(String),
    Store(String),
    Word(String),
    Refer(String),
    Table(Vec<(Node, Node)>),
    Guard((Vec<GuardItem>, Vec<GuardItem>)),
    If((Vec<Node>, Option<Vec<Node>>)),
    StackBlock((String, Vec<Node>)),
    Until(Vec<Node>),
    Loop(Vec<Node>),
    Cond(Vec<(CondArm, Vec<Node>)>),
    Ident(String),
    Continue,
    Break,
}

pub fn do_parse(source: &str) -> Result<Vec<Node>, Error<Rule>> {
    let mut ast = Vec::new();
    let pairs = ZfParser::parse(Rule::program, source)?;

    for pair in pairs {
        if let Some(node) = parse_node(pair) {
            ast.push(node);
        }
    }

    Ok(ast)
}

fn parse_node(pair: Pair<Rule>) -> Option<Node> {
    match pair.as_rule() {
        Rule::EOI => None,
        Rule::word_decl => {
            let mut items = pair.into_inner();
    
            let name;
            if let Some(Node::Ident(s)) = parse_node(items.nth(0).unwrap()) {
                name = s;
            } else {
                unreachable!();
            }
    
            let body = items.nth(0).unwrap().into_inner()
                .map(|p| parse_node(p))
                .filter(|p| p.is_some()).map(|p| p.unwrap())
                .collect::<Vec<Node>>();
    
            Some(Node::Decl((name, body)))
        },
        Rule::quote => {
            let quote = pair.into_inner()
                .map(|p| parse_node(p))
                .filter(|p| p.is_some()).map(|p| p.unwrap())
                .collect::<Vec<Node>>();
    
            if quote.len() == 1 {
                if let Node::Word(w) = &quote[0] {
                    return Some(Node::Refer(w.clone()));
                }
            }
    
            Some(Node::Quote(quote))
        },
        Rule::float => {
            // FIXME: Rust's parse::<f64> doesn't support the following formats:
            //  - 0__.0__e+10__, 0x2351, 0o261, 0b100111, 100_000_000
            // At some point, a custom float-parsing function should be made.
            //
            // XXX: we trim whitespace because the grammar requires whitespace
            // to be at the end of the literal.
            let dstr = pair.as_str().trim_end();
            let (sign, dstr) = match &dstr[..1] {
                "_" => (-1.0, &dstr[1..]),
                _ => (1.0, &dstr[..]),
            };
            let mut flt: f64 = match dstr.parse() {
                Ok(fl) => fl,
                Err(_) => panic!("`{}' is not a valid float", dstr),
            };
            if flt != 0.0 {
                // Avoid negative zeroes; only multiply sign by nonzeroes.
                flt *= sign;
            }
            Some(Node::Number(flt))
        },
        Rule::string => {
            let str = &pair.as_str();
            // Strip leading and ending quotes.
            Some(Node::Str(str[1..str.len()-1].to_owned()))
        },
        Rule::character => {
            let ch = &pair.as_str();
            let ch = &ch[1..].chars().next().unwrap();
            Some(Node::Char(*ch as u32 as f64))
        },
        Rule::word  => Some(Node::Word(pair.as_str().to_owned())),
        Rule::stack => Some(Node::Stack(pair.as_str()[1..].to_owned())),
        Rule::fetch => Some(Node::Fetch(pair.as_str()[1..].to_owned())),
        Rule::store => Some(Node::Store(pair.as_str()[1..].to_owned())),
        Rule::ident => Some(Node::Ident(pair.as_str().to_owned())),
        Rule::table => {
            let mut res = Vec::new();
            let mut ctr = 0.;
    
            pair.into_inner().for_each(|item| {
                match item.as_rule() {
                    Rule::table_val => {
                        let val = parse_node(item.into_inner().next().unwrap()).unwrap();
                        res.push((Node::Number(ctr), val));
                        ctr += 1.;
                    },
                    Rule::table_keyval => {
                        let mut items = item.into_inner();
                        let key = parse_node(items.nth(0).unwrap()).unwrap();
                        let val = parse_node(items.nth(1).unwrap()).unwrap();
                        res.push((key, val));
                    },
                    _ => unreachable!(),
                }
            });
    
            Some(Node::Table(res))
        },
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
                        _   => panic!("'{}' is not a valid guard item",
                            minion.as_str()),
                    });
                }
                guardsets.push(guardset);
            }
    
            Some(Node::Guard((guardsets[0].clone(), guardsets[1].clone())))
        },
        Rule::ifblk => {
            let ifstmt;
            let mut elsestmt = None;
    
            let mut ast = pair.into_inner();
            ifstmt = ast.nth(0).unwrap().into_inner()
                .map(|p| parse_node(p))
                .filter(|p| p.is_some()).map(|p| p.unwrap())
                .collect::<Vec<Node>>();
    
            let orelse_ast = ast.nth(0);
            if orelse_ast.is_some() {
                let elseblk = orelse_ast.unwrap().into_inner()
                    .map(|p| parse_node(p))
                    .filter(|p| p.is_some()).map(|p| p.unwrap())
                    .collect::<Vec<Node>>();
                elsestmt = Some(elseblk);
            }
    
            Some(Node::If((ifstmt, elsestmt)))
        },
        Rule::stackblk => {
            let mut ast = pair.into_inner();
    
            let name;
            if let Some(Node::Ident(s)) = parse_node(ast.nth(0).unwrap()) {
                name = s;
            } else {
                unreachable!();
            }
    
            let stackblk = ast.nth(0).unwrap().into_inner()
                .map(|p| parse_node(p))
                .filter(|p| p.is_some()).map(|p| p.unwrap())
                .collect::<Vec<Node>>();
    
            Some(Node::StackBlock((name, stackblk)))
        },
        Rule::continuestmt => Some(Node::Continue),
        Rule::breakstmt    => Some(Node::Break),
        Rule::loopblk => {
            let loopblk = pair.into_inner()
                .nth(0).unwrap().into_inner()
                .map(|p| parse_node(p))
                .filter(|p| p.is_some()).map(|p| p.unwrap())
                .collect::<Vec<Node>>();
            Some(Node::Loop(loopblk))
        },
        Rule::until => {
            let loopblk = pair.into_inner()
                .nth(0).unwrap().into_inner()
                .map(|p| parse_node(p))
                .filter(|p| p.is_some()).map(|p| p.unwrap())
                .collect::<Vec<Node>>();
            Some(Node::Until(loopblk))
        },
        Rule::cond => {
            let mut arms = vec![];
            let mut any  = None;

            for arm_ast in pair.into_inner() {
                if any.is_some() {
                    // It's a syntax error for arms to come after the wildcard.
                    panic!("Found match arm after wildcard");
                }

                match arm_ast.as_rule() {
                    Rule::condarm => {
                        let mut inner = arm_ast.into_inner();
                        let cond = inner.nth(0).unwrap().into_inner()
                            .map(|p| parse_node(p))
                            .filter(|p| p.is_some()).map(|p| p.unwrap())
                            .collect::<Vec<Node>>();
                        let body = inner.nth(0).unwrap().into_inner()
                            .map(|p| parse_node(p))
                            .filter(|p| p.is_some()).map(|p| p.unwrap())
                            .collect::<Vec<Node>>();
                        arms.push((CondArm::Is(cond), body));
                    },
                    Rule::condany => {
                        let body = arm_ast.into_inner()
                            .nth(0).unwrap().into_inner()
                            .map(|p| parse_node(p))
                            .filter(|p| p.is_some()).map(|p| p.unwrap())
                            .collect::<Vec<Node>>();
                        any = Some((CondArm::Any, body));
                    },
                    _ => unreachable!(),
                }
            }

            if any.is_some() {
                arms.push(any.unwrap());
            }

            Some(Node::Cond(arms))
        },
        what_the_hell => panic!("Unexpected expression: {:?}", what_the_hell),
    }
}

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
        ast.extend(parse_term(env, pair, false));
    }
    Ok(ast)
}

fn parse_term(env: &mut ZfEnv, pair: Pair<Rule>, in_loop: bool) -> Vec<ZfToken> {
    match pair.as_rule() {
        Rule::EOI => vec![],
        Rule::word_decl => {
            let mut items = pair.into_inner();

            let name;
            let ident = parse_term(env, items.nth(0).unwrap(), false)[0].clone();
            if let ZfToken::Ident(s) = ident {
                name = s;
            } else { unreachable!() }

            // Add the name with an empty body.
            // This ensures that if the word is recursive (and the word is referenced
            // by itself) no word-not-found errors will be thrown.
            env.addword(name.clone(), vec![]);

            let body = items
                .nth(0).unwrap().into_inner()
                .map(|p| parse_term(env, p, false))
                .fold(Vec::new(), |mut a, i| { a.extend(i); a });
            env.addword(name, body);
            vec![]
        }
        Rule::quote => {
            let quote = pair.into_inner()
                .map(|p| parse_term(env, p, false))
                .fold(Vec::new(), |mut a, i| { a.extend(i); a });

            // If the quote has only one element, and that element is a word,
            // then just return a reference to that single word instead of creating
            // a useless wrapper.
            if quote.len() == 1 {
                if let ZfToken::Symbol(r) = quote[0] {
                    return vec![ZfToken::SymbRef(r)];
                }
            }

            let _ref = env.addword(random::phrase(), quote);
            vec![ZfToken::SymbRef(_ref)]
        }
        Rule::float => {
            // FIXME: Rust's parse::<f64> doesn't support the following formats:
            //  - 0__.0__e+10__, 0x2351, 0o261, 0b100111, 100_000_000
            // At some point, a custom float-parsing function should be made.
            //
            // XXX: we trim whitespace because the grammar requires whitespace
            // to be at the end of the literal.
            let dstr = pair.as_str().trim_end();
            let (sign, dstr) = match &dstr[..1] {
                "_" => (-1.0, &dstr[1..]),
                _ => (1.0, &dstr[..]),
            };
            let mut flt: f64 = match dstr.parse() {
                Ok(fl) => fl,
                Err(_) => panic!("`{}' is not a valid float", dstr),
            };
            if flt != 0.0 {
                // Avoid negative zeroes; only multiply sign by nonzeroes.
                flt *= sign;
            }
            vec![ZfToken::Number(flt)]
        }
        // TODO: escape sequences: \r \n \a \b \f \t \v \0 \x00 \uXXXX &c
        Rule::string => {
            let str = &pair.as_str();
            // Strip leading and ending quotes.
            let str = &str[1..str.len() - 1];
            // Escaped string quotes become single quotes here.
            let str = str.replace("''", "'");
            vec![ZfToken::String(str[..].to_owned())]
        }
        Rule::character => {
            let ch = &pair.as_str();
            let ch = &ch[1..].chars().next().unwrap();
            vec![ZfToken::Number(*ch as u32 as f64)]
        }
        Rule::word => {
            let ident = pair.as_str().to_owned();
            match env.findword(&ident) {
                Some(i) => vec![ZfToken::Symbol(i)],
                None => panic!("unknown word {}", ident),
            }
        }
        Rule::stack => vec![ZfToken::Stack(pair.as_str()[1..].to_owned())],
        Rule::fetch => vec![ZfToken::Fetch(pair.as_str()[1..].to_owned())],
        Rule::store => vec![ZfToken::Store(pair.as_str()[1..].to_owned())],
        Rule::ident => vec![ZfToken::Ident(pair.as_str().to_owned())],
        Rule::table => {
            let mut res = HashMap::new();
            let mut ctr = 0.;

            pair.into_inner().for_each(|item| {
                match item.as_rule() {
                    Rule::table_val => {
                        let val = parse_term(env, item.into_inner().next().unwrap(), in_loop);
                        res.insert(ZfToken::Number(ctr), val[0].clone()); 
                        ctr += 1.;
                    },
                    Rule::table_keyval => {
                        let mut items = item.into_inner();
                        let key = parse_term(env, items.nth(0).unwrap(), in_loop)[0].clone();
                        let val = parse_term(env, items.nth(1).unwrap(), in_loop)[1].clone();
                        res.insert(key, val);
                    },
                    _ => unreachable!(),
                }
            });

            vec![ZfToken::Table(res)]
        },
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
                        _   => panic!("'{}' is not a valid guard item",
                            minion.as_str()),
                    });
                }
                guardsets.push(guardset);
            }

            vec![ZfToken::Guard {
                before: guardsets[0].clone(),
                after:  guardsets[1].clone()
            }]
        }
        Rule::ifblk => {
            let mut ifstmt = vec![];

            let mut ast = pair.into_inner();
            let body = ast.nth(0).unwrap().into_inner()
                .map(|p| parse_term(env, p, in_loop))
                .fold(Vec::new(), |mut a, i| { a.extend(i); a });

            let orelse_ast = ast.nth(0);
            if orelse_ast.is_some() {
                let orelse = orelse_ast.unwrap().into_inner()
                    .map(|p| parse_term(env, p, in_loop))
                    .fold(Vec::new(), |mut a, i| { a.extend(i); a });

                ifstmt.push(ZfToken::ZJump((body.len() + 2) as isize));
                ifstmt.extend(body);
                ifstmt.push(ZfToken::UJump((orelse.len() + 1) as isize));
                ifstmt.extend(orelse);
            } else {
                ifstmt.push(ZfToken::ZJump((body.len() + 1) as isize));
                ifstmt.extend(body);
            }

            ifstmt
        },
        Rule::stackblk => {
            let mut stackblk = vec![];
            let mut ast = pair.into_inner();

            let name;
            let ident = parse_term(env, ast.nth(0).unwrap(), false)[0].clone();
            if let ZfToken::Stack(s) = ident {
                name = s;
            } else { unreachable!() }

            let body = ast.nth(0).unwrap().into_inner()
                .map(|p| parse_term(env, p, true))
                .fold(Vec::new(), |mut a, i| { a.extend(i); a });

            stackblk.push(ZfToken::Switch(name));
            stackblk.extend(body);
            stackblk.push(ZfToken::Switch(DEFAULT_STACK.to_owned()));

            stackblk
        },
        Rule::continuestmt if in_loop => vec![ZfToken::Continue],
        Rule::breakstmt    if in_loop => vec![ZfToken::Break],
        Rule::loopblk => {
            let mut ast = pair.into_inner();
            let mut loopblk = ast.nth(0).unwrap().into_inner()
                .map(|p| parse_term(env, p, true))
                .fold(Vec::new(), |mut a, i| { a.extend(i); a });

            let len = loopblk.len() as isize;
            loopblk.push(ZfToken::UJump(-len));

            // Convert breaks and continues to jumps
            for i in 0..loopblk.len() {
                match &loopblk[i] {
                    ZfToken::Continue => loopblk[i] = ZfToken::UJump(-(i as isize)),
                    ZfToken::Break =>
                        loopblk[i] = ZfToken::UJump((loopblk.len() - i) as isize),
                    _ => (),
                }
            }

            loopblk
        },
        Rule::until => {
            let mut until = vec![];

            let mut ast = pair.into_inner();
            let body = ast.nth(0).unwrap().into_inner()
                .map(|p| parse_term(env, p, true))
                .fold(Vec::new(), |mut a, i| { a.extend(i); a });

            let len = body.len() as isize;
            until.extend(body);
            until.push(ZfToken::ZJump(-len));

            // Convert breaks and continues to jumps
            for i in 0..until.len() {
                match &until[i] {
                    ZfToken::Continue => until[i] = ZfToken::UJump(-(i as isize)),
                    ZfToken::Break =>
                        until[i] = ZfToken::UJump((until.len() - i) as isize),
                    _ => (),
                }
            }

            until
        },
        Rule::cond => {
            let mut arms = vec![];
            let mut any  = None;

            for arm_ast in pair.into_inner() {
                if any.is_some() {
                    // It's a syntax error for arms to come after the wildcard.
                    panic!("Found match arm after wildcard");
                }

                match arm_ast.as_rule() {
                    Rule::condarm => {
                        let mut inner = arm_ast.into_inner();
                        let cond = inner.nth(0).unwrap().into_inner()
                            .map(|p| parse_term(env, p, in_loop))
                            .fold(Vec::new(), |mut a, i| { a.extend(i); a});
                        let body = inner.nth(0).unwrap().into_inner()
                            .map(|p| parse_term(env, p, in_loop))
                            .fold(Vec::new(), |mut a, i| { a.extend(i); a});
                        arms.push((cond, body));
                    },
                    Rule::condany => {
                        let body = arm_ast.into_inner()
                            .nth(0).unwrap().into_inner()
                            .map(|p| parse_term(env, p, in_loop))
                            .fold(Vec::new(), |mut a, i| { a.extend(i); a});
                        any = Some(body);
                    },
                    _ => unreachable!(),
                }
            }

            // Now we get to construct the bytecode for the cond block.
            // Fun fun fun~~

            let mut condblk = vec![];
            let mut restlen = arms.iter()
                .fold(0, |a, i| a + i.0.len() + 1 + i.1.len() + 1)
                + any.clone().unwrap_or(vec![]).len();

            for arm in arms {
                restlen -= arm.0.len() + 1 + arm.1.len() + 1;
                condblk.extend(arm.0);
                condblk.push(ZfToken::ZJump((arm.1.len() + 2) as isize));
                condblk.extend(arm.1);
                condblk.push(ZfToken::UJump(restlen as isize + 1));
            }

            if any.is_some() {
                condblk.extend(any.unwrap());
            } else {
                // Pop the last unecessary UJump.
                condblk.pop();
            }

            condblk
        },
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_comments() {
        parses_to! { parser: ZfParser, input: "(abcd)", rule: Rule::program, tokens: [] };
        parses_to! { parser: ZfParser, input: "( ab )", rule: Rule::program, tokens: [] };
        parses_to! { parser: ZfParser, input: "((ab))", rule: Rule::program, tokens: [] };
        parses_to! { parser: ZfParser, input: "([12])", rule: Rule::program, tokens: [] };
        parses_to! { parser: ZfParser, input: "(word)", rule: Rule::program, tokens: [] };
        parses_to! { parser: ZfParser, input: "\\  \n", rule: Rule::program, tokens: [] };
    }

    #[test]
    fn test_float_literal() {
        parses_to! { parser: ZfParser, input: "123  ", rule: Rule::program,
            tokens: [ float(0, 5) ] }; // XXX: end pos includes ws
        parses_to! { parser: ZfParser, input: "0.\n", rule: Rule::program,
            tokens: [ float(0, 3) ] }; // XXX: end pos includes ws
        parses_to! { parser: ZfParser, input: "1e10 ", rule: Rule::program,
            tokens: [ float(0, 5) ] }; // XXX: end pos includes ws
        parses_to! { parser: ZfParser, input: "0.e0", rule: Rule::program,
            tokens: [ float(0, 4) ] };
        parses_to! { parser: ZfParser, input: "0_0.0e+10", rule: Rule::program,
            tokens: [ float(0, 9) ] };
    }

    #[test]
    fn test_table() {
        parses_to! { parser: ZfParser, input: "{ 1 2 3 }", rule: Rule::program,
            tokens: [ table(0, 9, [
                table_val(2, 4, [float(2, 4)]),
                table_val(4, 6, [float(4, 6)]),
                table_val(6, 8, [float(6, 8)])
            ]) ] };
        parses_to! { parser: ZfParser, input: "{{ 1 2  3 5 }}", rule: Rule::program,
            tokens: [ table(0, 14, [
                table_keyval(3,  8, [float(3,  5), float( 5,  8)]),
                table_keyval(8, 12, [float(8, 10), float(10, 12)])
            ]) ] };
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

    #[test]
    fn test_ast_quotes() {
        use crate::ZfToken::*;

        let mut env = ZfEnv::new();
        env.dict.push(("drop".to_string(),
            ZfProc::Builtin(Rc::new(Box::new(crate::stdlib::DROP)))));
        let drop = env.findword("drop").unwrap();

        assert_eq!(parser::parse(&mut env, "[ drop ]").unwrap(),
            vec![SymbRef(drop)]);
    }
}
