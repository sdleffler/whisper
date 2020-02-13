whisper_derive::relation! {
    fn let_sugar();

    foo X if
        bar X,
        let X = baz (quux corge) fred;
}
