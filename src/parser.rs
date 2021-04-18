use crate::*;
use pest::error::Error;
use pest::Parser;
use pest::iterators::Pair;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct ZfParser;

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

pub fn compile(env: &mut ZfEnv, source: Vec<Node>) -> Result<Vec<ZfToken>, ()> {
    let mut bytecode = vec![];
    for node in source {
        bytecode.extend(compile_node(env, node)?);
    }
    Ok(bytecode)
}

fn compile_block(env: &mut ZfEnv, nodes: Vec<Node>) -> Result<Vec<ZfToken>, ()> {
    let mut bytecode = vec![];
    for node in nodes {
        bytecode.extend(compile_node(env, node)?);
    }
    Ok(bytecode)
}

fn compile_node(env: &mut ZfEnv, node: Node) -> Result<Vec<ZfToken>, ()> {
    match node {
        Node::Decl((name, uncompiled_body)) => {
            // Add the word with an empty body.
            // This ensures that if the word is recursive (and the word is
            // referenced by itself) no word-not-found errors will be thrown.
            env.addword(name.clone(), vec![]);

            let body = compile_block(env, uncompiled_body)?;
            env.addword(name, body);
            Ok(vec![])
        },
        Node::Quote(quote) => {
            let compiled = compile_block(env, quote)?;
            let _ref = env.addword(random::phrase(), compiled);
            Ok(vec![ZfToken::SymbRef(_ref)])
        },
        Node::Str(s)    => Ok(vec![ZfToken::String(s)]),
        Node::Number(f) => Ok(vec![ZfToken::Number(f)]),
        Node::Char(f)   => Ok(vec![ZfToken::Number(f)]),
        Node::Stack(f)  => Ok(vec![ZfToken::Stack(f)]),
        Node::Fetch(f)  => Ok(vec![ZfToken::Fetch(f)]),
        Node::Store(f)  => Ok(vec![ZfToken::Store(f)]),
        Node::Refer(r) => {
            match env.findword(&r) {
                Some(i) => Ok(vec![ZfToken::SymbRef(i)]),
                None => panic!("unknown word {}", r),
            }
        },
        Node::Word(w) => {
            match env.findword(&w) {
                Some(i) => Ok(vec![ZfToken::Symbol(i)]),
                None => panic!("unknown word {}", w),
            }
        },
        Node::Table(i) => {
            let mut accm = HashMap::new();
            for (key, val) in i {
                let mut key = compile_node(env, key)?;
                let mut val = compile_node(env, val)?;
                assert!(key.len() == 1);
                assert!(val.len() == 1);
                accm.insert(key.pop().unwrap(), val.pop().unwrap());
            }
            Ok(vec![ZfToken::Table(accm)])
        },
        Node::Guard((before, after)) => Ok(vec![ZfToken::Guard { before: before, after: after }]),
        Node::If((_if, _else)) => {
            let mut res = vec![];
            let ifblk = compile_block(env, _if)?;

            if _else.is_some() {
                let orelse = compile_block(env, _else.unwrap())?;

                res.push(ZfToken::ZJump((ifblk.len() + 2) as isize));
                res.extend(ifblk);
                res.push(ZfToken::UJump((orelse.len() + 1) as isize));
                res.extend(orelse);
            } else {
                res.push(ZfToken::ZJump((ifblk.len() + 1) as isize));
                res.extend(ifblk);
            }

            Ok(res)
        },
        Node::StackBlock((name, body)) => {
            let mut res = vec![];
            let body = compile_block(env, body)?;

            res.push(ZfToken::Switch(name));
            res.extend(body);
            res.push(ZfToken::Switch(DEFAULT_STACK.to_owned()));

            Ok(res)
        },
        Node::Continue => Ok(vec![ZfToken::Continue]),
        Node::Break => Ok(vec![ZfToken::Break]),
        Node::Loop(body) => {
            let mut res = compile_block(env, body)?;

            let len = res.len() as isize;
            res.push(ZfToken::UJump(-len));

            // Convert breaks and continue's to jumps
            //
            // This is Very Ugly to say the least -- there shouldn't be magic
            // `break` and `continue` instructions in the VM that are fake
            // and never run. Ideally we should traverse the whole tree and
            // convert them...
            //
            // See also: Node::Until(_)
            //
            for i in 0..res.len() {
                match &res[i] {
                    ZfToken::Continue => res[i] = ZfToken::UJump(-(i as isize)),
                    ZfToken::Break => res[i] = ZfToken::UJump((res.len() - i) as isize),
                    _ => (),
                }
            }

            Ok(res)
        },
        Node::Until(body) => {
            let mut res = compile_block(env, body)?;

            let len = res.len() as isize;
            res.push(ZfToken::ZJump(-len));

            // Convert breaks and continue's to jumps
            // See also: Node::Loop(_)
            for i in 0..res.len() {
                match &res[i] {
                    ZfToken::Continue => res[i] = ZfToken::UJump(-(i as isize)),
                    ZfToken::Break => res[i] = ZfToken::UJump((res.len() - i) as isize),
                    _ => (),
                }
            }

            Ok(res)
        },
        Node::Cond(arms) => {
            // Ahh, now we can construct the bytecode for cond blocks!!
            // Fun fun fun ~~~

            let mut res = vec![];

            let mut bodylen = arms.iter().fold(0, |a, i| {
                match &i.0 {
                    CondArm::Is(body) => a + body.len() + 1 + i.1.len() + 1,
                    CondArm::Any => a + i.1.len(),
                }
            });

            // Store the any branch here if we find it, since it has to be
            // dealt with last.
            let mut any = None;

            for arm in arms {
                match arm.0 {
                    CondArm::Any => {
                        any = Some(arm.1);
                        continue;
                    },
                    CondArm::Is(u_cond) => {
                        let cond = compile_block(env, u_cond)?;
                        let body = compile_block(env, arm.1)?;

                        bodylen -= cond.len() + 1 + body.len() + 1;
                        res.extend(cond);
                        res.push(ZfToken::ZJump((body.len() + 2) as isize));
                        res.extend(body);
                        res.push(ZfToken::UJump(bodylen as isize + 1));
                    },
                }
            }

            if any.is_some() {
                res.extend(compile_block(env, any.unwrap())?);
            } else {
                // Pop the last UJump, since it's now unnecessary.
                res.pop();
            }

            Ok(res)
        },
        Node::Ident(_) => unreachable!(),
    }
}

pub fn parse(source: &str) -> Result<Vec<Node>, Error<Rule>> {
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

            // If the quote has only one item, compile it to a word reference
            // without the useless wrapper.
            //
            // In the past, there was a special syntax for this ("&foo" instead of "[ foo ]")
            // but it was removed later on.
            //
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
            let foo = parse_node(ast.nth(0).unwrap());
            if let Some(Node::Stack(s)) = foo {
                name = s;
            } else {
                panic!("{:?}", foo);
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


#[cfg(test)]
mod grammar_tests {
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
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use super::Node::*;

    fn test_parse(s: &str, e: &[Node]) {
        assert_eq!(parse(s).unwrap(), e.to_vec());
    }

    #[test]
    fn test_basic() {
        test_parse("1 2 3 \"test\" abcd 99 'b'", &[
            Number(1.), Number(2.), Number(3.), Str("test".to_owned()),
            Word("abcd".to_owned()), Number(99.), Char(98.)
        ]);
    }

    #[test]
    fn test_decl() {
        test_parse("word foo [[ 1 2 3 ]]", &[
            Decl(("foo".to_string(), vec![
                    Number(1.), Number(2.), Number(3.)
            ]))
        ]);
    }

    #[test]
    fn test_until() {
        let s = r###"
word e_iter [[ ( iters accm -- )
	until [[
		swap 1- swap over
		fact 1
		swap / + over
		1 =
	]]
]]
word e      [[ (     -- n   ) @E_ITERS 0 e_iter nip 1 + ]]
"###;
        test_parse(s, &[
            Decl(("e_iter".to_string(), vec![
                Until(vec![
                    Word("swap".to_string()), Word("1-".to_string()),
                    Word("swap".to_string()), Word("over".to_string()),
                    Word("fact".to_string()), Number(1.),
                    Word("swap".to_string()), Word("/".to_string()),
                    Word("+".to_string()), Word("over".to_string()),
                    Number(1.), Word("=".to_string()),
                ]),
            ])),
            Decl(("e".to_string(), vec![
                Fetch("E_ITERS".to_string()),
                Number(0.), Word("e_iter".to_string()), Word("nip".to_string()),
                Number(1.), Word("+".to_string()),
            ])),
        ]);
    }

    // Test that quotes with only a single item get compiled into a symbol
    // reference, without the useless wrapper.
    #[test]
    fn test_quotes() {
        test_parse("[ drop ]",      &[ Refer("drop".to_string()) ]);
        test_parse("[ swap drop ]", &[
            Quote(vec![
                Word("swap".to_string()), Word("drop".to_string())
            ])
        ]);
    }
}
