use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Expr, ExprLit, ExprMacro, Ident, Lit, LitStr, Token,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
};

struct FormatStrMacroInput {
    template: Expr,
    args: Punctuated<FormatStrArg, Token![,]>,
}

struct FormatStrArg {
    name: Ident,
    value: Expr,
}

impl Parse for FormatStrArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![=]>()?;
        let value: Expr = input.parse()?;
        Ok(FormatStrArg { name, value })
    }
}

impl Parse for FormatStrMacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let template: Expr = input.parse()?;
        let mut args = Punctuated::new();
        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            args = Punctuated::parse_terminated(input)?;
        }
        Ok(FormatStrMacroInput { template, args })
    }
}

#[proc_macro]
pub fn format_str(input: TokenStream) -> TokenStream {
    let FormatStrMacroInput { template, args } =
        syn::parse_macro_input!(input as FormatStrMacroInput);

    // Extract literal string from template if possible
    let tpl_str: Option<String> = match &template {
        Expr::Lit(ExprLit {
            lit: Lit::Str(lit), ..
        }) => Some(lit.value()),
        _ => None,
    };

    let raw = match tpl_str {
        Some(s) => s,
        None => {
            return syn::Error::new(template.span(), "template must be a string literal")
                .to_compile_error()
                .into();
        }
    };

    // Collect arguments
    let mut arg_map = std::collections::HashMap::new();
    for arg in &args {
        let name = arg.name.to_string();
        if arg_map.contains_key(&name) {
            return syn::Error::new(arg.name.span(), format!("duplicate argument: {}", name))
                .to_compile_error()
                .into();
        }
        arg_map.insert(name, &arg.value);
    }

    // Parse template for {{ placeholders }}
    let mut parts = Vec::new();
    let mut buf = String::new();
    let mut chars = raw.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '{' && chars.peek() == Some(&'{') {
            chars.next(); // consume second '{'
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
                return syn::Error::new(template.span(), format!("unknown placeholder: {}", ident))
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

struct FormatFileMacroInput {
    path: LitStr,
    args: Punctuated<FormatFileArg, Token![,]>,
}

struct FormatFileArg {
    name: Ident,
    value: Expr,
}

impl Parse for FormatFileArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![=]>()?;
        let value: Expr = input.parse()?;
        Ok(FormatFileArg { name, value })
    }
}

impl Parse for FormatFileMacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let path: LitStr = input.parse()?;
        let mut args = Punctuated::new();
        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            args = Punctuated::parse_terminated(input)?;
        }
        Ok(FormatFileMacroInput { path, args })
    }
}

#[proc_macro]
pub fn format_file(input: TokenStream) -> TokenStream {
    let FormatFileMacroInput { path, args } =
        syn::parse_macro_input!(input as FormatFileMacroInput);

    // Resolve the file path relative to project root
    let rel_path = path.value();
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let full_path = std::path::Path::new(&manifest_dir).join(&rel_path);

    let content = match std::fs::read_to_string(&full_path) {
        Ok(c) => c,
        Err(e) => {
            return syn::Error::new(path.span(), e.to_string())
                .to_compile_error()
                .into();
        }
    };

    // Collect named arguments
    let mut arg_map = std::collections::HashMap::new();
    for arg in &args {
        let name = arg.name.to_string();
        if arg_map.contains_key(&name) {
            return syn::Error::new(arg.name.span(), format!("duplicate argument: {}", name))
                .to_compile_error()
                .into();
        }
        arg_map.insert(name, &arg.value);
    }

    // Parse template for {{ placeholders }}
    let mut parts = Vec::new();
    let mut buf = String::new();
    let mut chars = content.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '{' && chars.peek() == Some(&'{') {
            chars.next(); // consume second '{'
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
                return syn::Error::new(path.span(), format!("unknown placeholder: {}", ident))
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

