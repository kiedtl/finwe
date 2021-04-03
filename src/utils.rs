macro_rules! pop {
    ($e:ident) => {
        match $e.pile.pop() {
            Some(e) => e,
            None => return Err(format!("stack underflow")),
        }
    }
}

macro_rules! pop_as {
    ($e:ident, $t:ident) => {{
        let popped = pop!($e);
        match popped {
            ZfToken::$t(v) => v,
            _ => return Err(format!("expected {}, got {:?}",
                    stringify!($t), popped)),
        }
    }}
}
