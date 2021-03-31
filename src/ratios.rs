macro_rules! rat {
    ($e:expr) => (Ratio64::from($e))
}

// Josef Stein's binary GCD algorithm
fn _gcd(mut u: usize, mut v: usize) -> usize {
    use std::mem::swap;

    // gcd(0, v) == v, gcd(u, 0) == u
    if u == 0 {
        return v;
    } else if v == 0 {
        return u
    }

    // found the answer
    if v == u {
        return v;
    }

    if (u & 1) == 0 {
        if (v & 1) == 0 {
            // gcd(2u, 2v) = gcd(u, v)
            2 * _gcd(u / 2, v / 2)
        } else {
            // gcd(2u, v) = gcd(u, v)
            _gcd(u / 2, v)
        }
    } else if (v & 1) == 0 {
        // gcd(u, 2v) = gcd(u, v)
        _gcd(u, v / 2)
    } else {
        // gcd(u, v) = gcd(|u - v|, min(u, v))
        if u < v {
            swap(&mut u, &mut v);
        }

        _gcd(u - v, v)
    }
}

#[derive(Copy, Clone, Debug)]
struct Ratio64 {
    n: i64,
    d: i64,        // should never be <0
}

impl Ratio64 {
    pub fn new(n: i64, d: i64) -> Self {
        Ratio64 { n: n, d: d }
    }

    #[must_use]
    pub fn reciprocal(self) -> Self {
        Ratio64::new(self.d, self.n)
    }

    #[must_use]
    pub fn normalise(self) -> Self {
        let mut res = self;

        if res.n < 0 || res.d < 0 {
            res.n = -(res.n.abs());
            res.d = res.d.abs();
        }

        let gcd = _gcd(res.n.abs() as usize, res.d as usize);
        res.n = res.n / gcd as i64;
        res.d = res.d / gcd as i64;

        res
    }
}

macro_rules! gen_from_signed {
    ($($t:ident),+) => {
        $(impl From<$t> for Ratio64 {
            fn from(u: $t) -> Self { Ratio64::new(u as i64, 1) }
        })+
    }
}

macro_rules! gen_from_unsigned {
    ($($t:ident),+) => {
        $(impl From<$t> for Ratio64 {
            fn from(u: $t) -> Self { Ratio64::new(u as i64, 1) }
        })+
    }
}

gen_from_signed!  (isize, i64, i32, i16, i8);
gen_from_unsigned!(usize, u64, u32, u16, u8);

impl PartialEq for Ratio64 {
    fn eq(&self, rhs: &Self) -> bool {
        let lhs = self.normalise();
        let rhs = rhs.normalise();
        lhs.n == rhs.n && lhs.d == rhs.d
    }
}
impl Eq for Ratio64 {}

impl std::ops::Add for Ratio64 {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        let a = Ratio64::new(self.d * rhs.n,  self.d * rhs.d);
        let b = Ratio64::new(rhs.d  * self.n, self.d * rhs.d);
        debug_assert!(a.d == b.d);
        Ratio64::new(a.n + b.n, a.d)
    }
}

impl std::ops::Sub for Ratio64 {
    type Output = Self;
    fn sub(mut self, rhs: Self) -> Self::Output {
        self.n *= -1;
        let mut res = self + rhs;
        res.n *= -1;
        res
    }
}

impl std::ops::Mul for Ratio64 {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        if rhs == rat!(1) {
            return self;
        } else if self == rat!(1) {
            return rhs;
        }

        Ratio64::new(self.n * rhs.n, self.d * rhs.d)
    }
}

impl std::ops::Div for Ratio64 {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        if self == rat!(1) {
            return rhs;
        }

        let rhs = rhs.reciprocal();
        Ratio64::new(self.n * rhs.n, self.d * rhs.d)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_addition() {
        assert_eq!(rat!( 1) + rat!( 2), rat!( 3));
        assert_eq!(rat!(-3) + rat!( 2), rat!(-1));
        assert_eq!(rat!(-3) + rat!(-2), rat!(-5));
        assert_eq!(rat!(-7) + rat!( 1), rat!(-6));
        assert_eq!(rat!(-4) + rat!( 8), rat!( 4));
    }

    #[test]
    fn test_subtraction() {
        assert_eq!(rat!( 2) - rat!( 2), rat!( 0));
        assert_eq!(rat!(-4) - rat!(-8), rat!( 4));
        assert_eq!(rat!( 4) - rat!(-8), rat!(12));
        assert_eq!(rat!( 5) - rat!( 8), rat!(-3));
    }

    #[test]
    fn test_multipliciation() {
        assert_eq!(rat!( 3) * rat!( 3), rat!( 9));
        assert_eq!(rat!( 0) * rat!( 0), rat!( 0));
        assert_eq!(rat!( 4) * rat!( 1), rat!( 4));
        assert_eq!(rat!( 4) * rat!(-2), rat!(-8));
        assert_eq!(rat!(-2) * rat!( 4), rat!(-8));
        assert_eq!(rat!(-2) * rat!(-2), rat!( 4));
    }

    #[test]
    fn test_division() {
        assert_eq!(rat!( 9) / rat!(-3), rat!(-3));
        assert_eq!(rat!( 9) / rat!( 3), rat!( 3));
        assert_eq!(rat!(-9) / rat!( 3), rat!(-3));
    }
}
