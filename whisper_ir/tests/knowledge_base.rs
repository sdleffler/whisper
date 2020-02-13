whisper::knowledge_base! {
    fn foo();

    mod snafu {
        fubar if fubar in extern;
    }

    wrapped if
        wrapped in extern,
        fubar in snafu;
}
