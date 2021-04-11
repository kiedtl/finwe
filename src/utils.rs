macro_rules! pop_as {
    ($e:ident, $t:ident) => {{
        match $e.pop()? {
            ZfToken::$t(v) => v,
            x => return Err(format!("expected {}, got {:?}",
                    stringify!($t), x)),
        }
    }}
}
