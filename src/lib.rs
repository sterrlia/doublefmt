use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Expr, Ident, Token,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

struct MacroInput {
    template: Expr,
    args: Punctuated<Arg, Token![,]>,
}

struct Arg {
    name: Ident,
    value: Expr,
}

impl Parse for Arg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![=]>()?;
        let value: Expr = input.parse()?;
        Ok(Arg { name, value })
    }
}

impl Parse for MacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let template: Expr = input.parse()?;
        let mut args = Punctuated::new();
        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            args = Punctuated::parse_terminated(input)?;
        }
        Ok(MacroInput { template, args })
    }
}

#[proc_macro]
pub fn doublefmt(input: TokenStream) -> TokenStream {
    let MacroInput { template, args } = syn::parse_macro_input!(input as MacroInput);

    // store arguments into a map at runtime
    let mut keys = Vec::new();
    let mut values = Vec::new();
    for arg in args {
        keys.push(arg.name.to_string());
        values.push(arg.value);
    }

    let expanded = quote! {{
        let tpl: &str = #template;
        let mut s = String::new();
        let mut i = 0;
        while let Some(start) = tpl[i..].find("{{") {
            let start = i + start;
            if let Some(end) = tpl[start + 2..].find("}}") {
                let end = start + 2 + end;
                // push literal text
                s.push_str(&tpl[i..start]);
                // extract placeholder name
                let name = tpl[start + 2..end].trim();
                match name {
                    #(
                        #keys => {
                            use std::fmt::Write as _;
                            write!(&mut s, "{}", #values).unwrap();
                        }
                    )*
                    _ => panic!("unknown placeholder: {}", name),
                }
                i = end + 2;
            } else {
                // no closing "}}"
                s.push_str(&tpl[start..]);
                break;
            }
        }
        s.push_str(&tpl[i..]);
        s
    }};
    expanded.into()
}
