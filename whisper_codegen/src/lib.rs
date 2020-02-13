mod symbols;

#[proc_macro]
pub fn reserved_symbols(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    symbols::reserved_symbols(input.into()).into()
}
