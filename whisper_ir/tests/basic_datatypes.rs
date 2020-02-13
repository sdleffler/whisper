whisper_derive::module! {
    fn basic_datatypes();

    foo;
    foo -32i;
    foo 16u;
    foo -2.34;
    foo (bar baz);
    Foo bar;
    raw "beef";
    raw "DeadBeef"; // Not a variable!

    schema (<SCHEMA::variant> Variant) is_variant Variant;

    foo true false; // `true`/`false` should parse as an ident/const, not as a Rust keyword.

    foo bar::true::false::crate; // They should parse like such in paths, too.

    list [thing, other_thing, "Blargh" | bleergh];
    foo (tagged : (value foo)); // Tagged values
    map { key: value, key2: value2, | meeeeep };

    trailing_comma [thing, boo, | haha_got_you];
}
