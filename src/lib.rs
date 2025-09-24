use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Expr, Ident, LitStr, Token,
};

struct MacroInput {
    template: LitStr,
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
        let template: LitStr = input.parse()?;
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

    // collect named args into a map
    let mut arg_map = std::collections::HashMap::new();
    for arg in args {
        arg_map.insert(arg.name.to_string(), arg.value);
    }

    let raw = template.value();
    let mut parts = Vec::new();
    let mut buf = String::new();
    let mut chars = raw.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '{' && chars.peek() == Some(&'{') {
            chars.next();
            if !buf.is_empty() {
                parts.push(quote! { s.push_str(#buf); });
                buf.clear();
            }
            let mut ident = String::new();
            while let Some(ch) = chars.next() {
                if ch == '}' && chars.peek() == Some(&'}') {
                    chars.next();
                    break;
                } else {
                    ident.push(ch);
                }
            }
            let ident = ident.trim();
            if let Some(expr) = arg_map.get(ident) {
                parts.push(
                    quote! { use std::fmt::Write as _; write!(&mut s, "{}", #expr).unwrap(); },
                );
            } else {
                let msg = format!("unknown placeholder: {}", ident);
                return syn::Error::new(template.span(), msg)
                    .to_compile_error()
                    .into();
            }
        } else {
            buf.push(c);
        }
    }
    if !buf.is_empty() {
        parts.push(quote! { s.push_str(#buf); });
    }

    let expanded = quote! {{
        let mut s = String::new();
        #(#parts)*
        s
    }};
    expanded.into()
}
