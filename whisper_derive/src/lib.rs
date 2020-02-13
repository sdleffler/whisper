extern crate proc_macro;

use proc_macro::TokenStream;

#[proc_macro]
pub fn knowledge_base(input: TokenStream) -> TokenStream {
    match whisper_ir::proc_macro::quasiquote::knowledge_base(input.into()) {
        Ok(output) => output.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro]
pub fn module(input: TokenStream) -> TokenStream {
    match whisper_ir::proc_macro::quasiquote::module(input.into()) {
        Ok(output) => output.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro]
pub fn query(input: TokenStream) -> TokenStream {
    match whisper_ir::proc_macro::quasiquote::query(input.into()) {
        Ok(output) => output.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro]
pub fn term(input: TokenStream) -> TokenStream {
    match whisper_ir::proc_macro::quasiquote::term(input.into()) {
        Ok(output) => output.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro]
pub fn goal(input: TokenStream) -> TokenStream {
    match whisper_ir::proc_macro::quasiquote::goal(input.into()) {
        Ok(output) => output.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro]
pub fn relation(input: TokenStream) -> TokenStream {
    match whisper_ir::proc_macro::quasiquote::relation(input.into()) {
        Ok(output) => output.into(),
        Err(err) => err.to_compile_error().into(),
    }
}
