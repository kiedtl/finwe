#[derive(PartialEq)]
enum Section { Integer, Decimal, Exponent }

fn _digits(f: f64) -> usize {
    let mut n = 0;
    let mut f = f as usize;
    while f != 0 {
        f /= 10;
        n += 1;
    }
    n
}

// FIXME: currently, this produces rounding errors when exponent is large.
// The "proper" way of producing a float from a mantissa/exponent (that is,
// fiddling bits and converting them to f64 via to_bits()) should be used.

pub fn parse(s: &str) -> Result<f64, String> {
    if s.is_empty() {
        return Err(format!("Empty string."));
    } 

    let mut stage = Section::Integer;

    let mut n_after_dot = 0;

    let mut accm: u64 = 0;
    let mut accmsign: i32 = 1;
    let mut expo: i32 = 0;
    let mut exposign: i32 = 1;

    let mut base: u32 = 10;

    let chs = s.chars().collect::<Vec<_>>();
    let mut i = 0;

    match chs.get(i) {
        None => return Err(format!("Empty string")),
        Some(&'-') => { accmsign = -1; i += 1; },
        Some(&'+') => { accmsign =  1; i += 1; },
        _ => (),
    }

    if chs.get(i) == Some(&'0') {
        i += 1;
        match chs.get(i) {
            Some(&'b') => { base =  2; i += 1; },
            Some(&'o') => { base =  8; i += 1; },
            Some(&'x') => { base = 16; i += 1; },
            _ => (),
        }
    }

    for j in i..chs.len() {
        let ch = chs[j];

        match ch {
            '_' => continue,
            _ if ch.is_digit(base) => {
                let digit = ch.to_digit(base).unwrap();
                match stage {
                    Section::Decimal | Section::Integer => {
                        if stage == Section::Decimal {
                            n_after_dot += 1;
                        }
                        accm = accm.checked_mul(base  as u64).ok_or_else(|| "m")?;
                        accm = accm.checked_add(digit as u64).ok_or_else(|| "m")?;
                    }
                    Section::Exponent => expo = (expo * base as i32) + digit as i32,
                }
            },
            '.'       if base == 10 => {
                stage = Section::Decimal;
                continue;
            }
            '-' | '+' if chs[j-1] == 'e' || chs[j-1] == 'E' => {
                match ch {
                    '-' => exposign = -1,
                    '+' => exposign =  1,
                    _   => unreachable!(),
                }
            },
            'E' | 'e' if base == 10 => {
                stage = Section::Exponent;
                continue;
            }
            _ => return Err(format!("Non-digit '{}' (radix {}) found", ch, base)),
        }
    }

    expo = (-(n_after_dot as i32) + expo) * exposign;

    let mut flt = accm as f64;
    flt *= 10_f64.powf(expo as f64);
    flt *= accmsign as f64;

    Ok(flt)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_parse {
        ($str:expr, $expect:expr) => {
            match parse($str) {
                Ok(fl) => if fl != $expect {
                    panic!("Invalid parse result for {:?}: {} != {}",
                        $str, fl, $expect);
                },
                Err(e) => panic!("failed to parse {:?}: {}", $str, e),
            }
        }
    }

    #[test]
    fn test_digits() {
        assert_eq!(_digits(5.90037), 1);
        assert_eq!(_digits(24.), 2);
    }

    #[test]
    fn test_simple() {
        test_parse!("0",       0_f64);
        test_parse!("0001",    1_f64);
        test_parse!("1",       1_f64);
        test_parse!("10",      10_f64);
        test_parse!("3489",    3489_f64);
        test_parse!("98",      98_f64);
        test_parse!("100_340", 100_340_f64);
    }

    #[test]
    fn test_fract() {
        test_parse!(".123", 0.123);
        test_parse!("5.0376091412", 5.0376091412);
    }

    #[test]
    fn test_neg() {
        test_parse!("-1",    -1.);
        test_parse!("-0",    -0.);
        test_parse!("-0.12", -0.12);
        test_parse!("-32",   -32.);
    }

    #[test]
    fn test_bases() {
        test_parse!("0b10",     2.);
        test_parse!("0x0ff",  255.);
        test_parse!("0xFF",   255.);
        test_parse!("0x1b",    27.);
        test_parse!("0o0033",  27.);
    }

    #[test]
    fn test_neg_bases() {
        test_parse!("-0o206",  -134.);
        test_parse!("-0x0FF",  -255.);
    }

    #[test]
    fn test_exponent() {
        test_parse!("1e2",          1e2);
        test_parse!("1e0",          1e0);
        test_parse!("+100_340e12",   100_340e12);
        test_parse!("-1e7",        -1e7);
        test_parse!("-0e1",        -0e1);
        test_parse!("-0.12e3",     -0.12e3);
    }

    #[test]
    fn test_fuzzy() {
        // Poor-man's fuzz testing
        use std::io::{BufReader, Read};
        use std::fs::File;

        let mut rng = BufReader::new(File::open("/dev/urandom").unwrap());

        let mut random = |limit| -> f64 {
            let mut b = [0u8; 5];
            rng.read_exact(&mut b).unwrap();
            let r = 0u64
                | (b[0] as u64) <<  0
                | (b[1] as u64) <<  8
                | (b[2] as u64) << 16
                | (b[3] as u64) << 32;
            let s = if (b[4] % 2) == 0 { -1 } else { 1 };
            ((r % limit) as f64) * s as f64
        };

        for _ in 0..1000 {
            let expo = (random(20) + 1.).round();
            let mant = random(100000);
            let float = mant * 10_f64.powf(expo);
            test_parse!(&format!("{:+e}", float), float);
            test_parse!(&format!("{}", float), float);
        }
    }

    // TODO: bad-input tests
    // TODO: inf/nan
}
