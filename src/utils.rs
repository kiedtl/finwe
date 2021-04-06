macro_rules! pop {
    ($e:ident) => {
        match $e.pile.pop() {
            Some(e) => e,
            None => return Err(format!("stack underflow")),
        }
    }
}

macro_rules! peek {
    ($e:ident) => {{
        let len = $e.pile.len();
        if len == 0 {
            return Err(format!("stack underflow"));
        }
        &$e.pile[len - 1]
    }}
}

macro_rules! pop_as {
    ($e:ident, $t:ident) => {{
        match pop!($e) {
            ZfToken::$t(v) => v,
            x => return Err(format!("expected {}, got {:?}",
                    stringify!($t), x)),
        }
    }}
}
