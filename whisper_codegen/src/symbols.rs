use ::{
    proc_macro2::TokenStream,
    quote::{format_ident, quote, ToTokens},
    string_cache_codegen::AtomType,
    syn::{
        parse::{Parse, ParseStream, Result},
        punctuated::Punctuated,
        Ident, LitStr, Path, Token,
    },
};

mod kw {
    syn::custom_keyword!(internal);
    syn::custom_keyword!(std);
}

pub enum SymbolSet {
    Public,
    Internal,
    Module,
}

impl PartialEq<str> for SymbolSet {
    fn eq(&self, other: &str) -> bool {
        match self {
            SymbolSet::Public if other == "PUBLIC" => true,
            SymbolSet::Internal if other == "INTERNAL" => true,
            SymbolSet::Module if other == "mod" => true,
            _ => false,
        }
    }
}

impl Parse for SymbolSet {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![pub]) {
            input.parse::<Token![pub]>()?;
            Ok(SymbolSet::Public)
        } else if lookahead.peek(kw::internal) {
            input.parse::<kw::internal>()?;
            Ok(SymbolSet::Internal)
        } else if lookahead.peek(Token![mod]) {
            input.parse::<Token![mod]>()?;
            Ok(SymbolSet::Module)
        } else {
            Err(lookahead.error())
        }
    }
}

pub struct SymbolEntry {
    name: LitStr,
    set: SymbolSet,
}

impl Parse for SymbolEntry {
    fn parse(input: ParseStream) -> Result<Self> {
        let set = input.parse()?;
        let name = input.parse()?;
        Ok(SymbolEntry { name, set })
    }
}

pub struct ReservedSymbols {
    symbols: Vec<SymbolEntry>,
}

impl Parse for ReservedSymbols {
    fn parse(input: ParseStream) -> Result<Self> {
        let symbols = Punctuated::<SymbolEntry, Token![,]>::parse_terminated(input)?
            .into_iter()
            .collect();
        Ok(Self { symbols })
    }
}

pub struct Input {
    atom_path: Path,
    atom_macro: Ident,
    reserved: ReservedSymbols,
}

impl Parse for Input {
    fn parse(input: ParseStream) -> Result<Self> {
        let atom_path = Path::parse_mod_style(input)?;
        input.parse::<Token![,]>()?;
        let atom_macro = input.parse()?;
        input.parse::<Token![!]>()?;
        input.parse::<Token![;]>()?;
        let reserved = input.parse()?;

        Ok(Self {
            atom_macro,
            atom_path,
            reserved,
        })
    }
}

pub fn reserved_symbols(input: TokenStream) -> TokenStream {
    let input: Input = match syn::parse2(input) {
        Ok(v) => v,
        Err(err) => return TokenStream::from(err.to_compile_error()),
    };

    let string_cache: TokenStream = {
        let mut string_cache_raw = Vec::new();
        let atoms = input
            .reserved
            .symbols
            .iter()
            .map(|entry| entry.name.value());
        let mut path_string = input.atom_path.into_token_stream().to_string();
        path_string.retain(|c| !c.is_whitespace());
        let mut builder = AtomType::new(&path_string, &(input.atom_macro.to_string() + "!"));
        builder
            .atoms(atoms)
            .write_to(&mut string_cache_raw)
            .unwrap();
        syn::parse_str(&String::from_utf8(string_cache_raw).unwrap()).unwrap()
    };

    let mut symbol_list = Vec::new();
    let mut symbol_id_list = Vec::new();
    let mut symbol_constants = Vec::new();
    let atom_macro = &input.atom_macro;

    for entry in &input.reserved.symbols {
        use SymbolSet::*;
        let constant_name = &entry.name;

        let name_string = match entry.name.value() {
            ref s if s == "()" => "unit".to_owned(),
            other => other.replace(" ", "_"),
        };

        let constant_ident = match entry.set {
            ref set if set == name_string.as_ref() => {
                format_ident!("{}", name_string.to_uppercase())
            }
            Public => format_ident!("{}", name_string.to_uppercase()),
            Internal => format_ident!("INTERNAL_{}", name_string.to_uppercase()),
            Module => format_ident!("MOD_{}", name_string.to_uppercase()),
        };

        let constant_id = match entry.set {
            Public => quote!(Scope::PUBLIC),
            Internal => quote!(Scope::INTERNAL),
            Module => quote!(Scope::MOD),
        };

        let constant_value =
            quote!(Symbol::new(Ident::from_atom(#atom_macro!(#constant_name)), #constant_id));
        symbol_constants.push(quote!(pub const #constant_ident: Symbol = #constant_value;));

        let id = symbol_id_list.len();
        let id_name = format_ident!("{}_INDEX", constant_ident);
        symbol_constants.push(quote!(pub const #id_name: SymbolIndex = SymbolIndex(#id);));

        symbol_list.push(constant_ident);
        symbol_id_list.push(id_name);
    }

    let num_builtins = symbol_list.len();

    quote! {
        #string_cache

        impl Symbol {
            #(#symbol_constants)*
        }

        const NUM_BUILTINS: usize = #num_builtins;

        lazy_static::lazy_static! {
            static ref BUILTINS_IDX_TO_SYM: &'static [Symbol] = {
                let mut vector = Vec::new();
                #(vector.push(Symbol::#symbol_list);)*
                Box::leak(Box::<[Symbol]>::from(vector))
            };

            static ref BUILTINS_SYM_TO_IDX: &'static std::collections::HashMap<Symbol, SymbolIndex> = {
                let mut map = std::collections::HashMap::new();
                #(map.insert(Symbol::#symbol_list, Symbol::#symbol_id_list);)*
                Box::leak(Box::new(map))
            };
        }
    }
}
