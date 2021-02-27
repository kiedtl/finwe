const WORDS: usize = 2;
const SYLLABLES: [&str; 159] = [
    "a",    "ab",   "ag",   "aks",  "ala",  "an",   "ankh", "app",  "arg",  "arze",
    "ash",  "ban",  "bar",  "bat",  "bek",  "bie",  "bin",  "bit",  "bjor",
    "blu",  "bot",  "bop",  "byt",  "comp", "con",  "cos",  "cre",  "dalf",
    "dan",  "den",  "do",   "e",    "eep",  "el",   "eng",  "er",   "ere",  "erk",
    "esh",  "evs",  "fa",   "fid",  "for",  "fri",  "foo",  "gan",  "gar",
    "glen", "gop",  "gre",  "ha",   "he",   "hyd",  "hai",  "ing",  "ion",  "ip",
    "ish",  "it",   "ite",  "iv",   "jo",   "kho",  "kli",  "klis", "la",   "lech",
    "man",  "mar",  "me",   "mi",   "mic",  "mik",  "mon",  "mung", "mur",
    "nej",  "nelg", "nep",  "ner",  "nes",  "nes",  "nih",  "nin",  "o",    "od",
    "ood",  "org",  "orn",  "ox",   "oxy",  "pay",  "pet",  "ple",  "plu",  "po",
    "pot",  "prok", "re",   "rea",  "rhov", "ri",   "ro",   "rog",  "rok",  "rol",
    "sa",   "san",  "sat",  "see",  "sef",  "seh",  "shu",  "ski",  "sna",
    "sne",  "snik", "sno",  "so",   "sol",  "sri",  "sta",  "sun",  "ta",
    "tab",  "tem",  "ther", "ti",   "tox",  "trol", "tue",  "turs", "u",
    "ulk",  "um",   "un",   "uni",  "ur",   "val",  "viv",  "vly",  "vom",  "wah",
    "wed",  "werg", "wex",  "whon", "wun",  "xo",   "y",    "yot",  "yum",
    "zant", "zap",  "zeb",  "zim",  "zok",  "zon",  "zum",
];

static mut STATE: usize = 0xdeadbeef;

fn _rnd(range: usize) -> usize {
    unsafe {
        STATE = STATE * 11109 + 13849;
        STATE = (STATE & 0x7FFFF) >>1;
    
        match range {
            0 => 0,
            _ => STATE % range,
        }
    }
}

// Put the fun back into computing !!!
pub fn phrase() -> String {
    let mut phrase = String::new();
    for _ in 0..WORDS {
        for _ in 0..(_rnd(2)+1) {
            let syll = SYLLABLES[_rnd(SYLLABLES.len())];
            phrase = format!("{}{}", phrase, syll);
        }
        phrase = format!("{}_", phrase);
    }
    phrase
}
