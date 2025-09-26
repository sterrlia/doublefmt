use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Expr, ExprLit, Ident, Lit, LitStr, Token,
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

    // extract literal
    let raw = match &template {
        Expr::Lit(ExprLit {
            lit: Lit::Str(lit), ..
        }) => lit.value(),
        _ => {
            return syn::Error::new(template.span(), "template must be a string literal")
                .to_compile_error()
                .into();
        }
    };

    // collect arg expressions
    let mut arg_exprs: std::collections::HashMap<String, &Expr> = std::collections::HashMap::new();
    for arg in &args {
        let name = arg.name.to_string();
        if arg_exprs.contains_key(&name) {
            return syn::Error::new(arg.name.span(), format!("duplicate argument: {}", name))
                .to_compile_error()
                .into();
        }
        arg_exprs.insert(name, &arg.value);
    }

    // create local idents for each arg: __arg_<name>
    let mut arg_idents: std::collections::HashMap<String, proc_macro2::Ident> =
        std::collections::HashMap::new();
    for name in arg_exprs.keys() {
        let ident =
            proc_macro2::Ident::new(&format!("__arg_{}", name), proc_macro2::Span::call_site());
        arg_idents.insert(name.clone(), ident);
    }

    // Node AST for template
    enum Node<'a> {
        Text(String),
        Expr(proc_macro2::Ident), // ident to print (the local __arg_* binding)
        If {
            name: String,
            ident: proc_macro2::Ident, // __arg_name
            expr: &'a Expr,            // original expression (not used for check; we use the ident)
            body: Vec<Node<'a>>,
        },
    }

    // Parse into nested nodes using a stack (supports nesting)
    let mut body_stack: Vec<Vec<Node>> = vec![Vec::new()];
    let mut if_stack: Vec<(String, proc_macro2::Ident, &Expr)> = Vec::new();

    let mut chars = raw.chars().peekable();
    let mut buf = String::new();

    while let Some(c) = chars.next() {
        if c == '{' {
            if chars.peek() == Some(&'{') {
                // {{ expr }}
                chars.next(); // consume second '{'
                if !buf.is_empty() {
                    body_stack.last_mut().unwrap().push(Node::Text(buf.clone()));
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
                if let Some(local_ident) = arg_idents.get(ident) {
                    body_stack
                        .last_mut()
                        .unwrap()
                        .push(Node::Expr(local_ident.clone()));
                } else {
                    return syn::Error::new(
                        template.span(),
                        format!("unknown placeholder: {}", ident),
                    )
                    .to_compile_error()
                    .into();
                }
            } else if chars.peek() == Some(&'%') {
                // {% directive %}
                chars.next(); // consume '%'
                if !buf.is_empty() {
                    body_stack.last_mut().unwrap().push(Node::Text(buf.clone()));
                    buf.clear();
                }
                let mut directive = String::new();
                while let Some(ch) = chars.next() {
                    if ch == '%' && chars.peek() == Some(&'}') {
                        chars.next();
                        break;
                    } else {
                        directive.push(ch);
                    }
                }
                let directive = directive.trim();
                if let Some(rest) = directive.strip_prefix("if ") {
                    // expect "var is Some"
                    let tokens: Vec<&str> = rest.split_whitespace().collect();
                    if tokens.len() == 3 && tokens[1] == "is" && tokens[2] == "Some" {
                        let var = tokens[0];
                        if let (Some(expr), Some(local_ident)) =
                            (arg_exprs.get(var), arg_idents.get(var))
                        {
                            if_stack.push((var.to_string(), local_ident.clone(), expr));
                            body_stack.push(Vec::new()); // new body for this if
                            if chars.peek() == Some(&'\n') {
                                chars.next();
                            }
                        } else {
                            return syn::Error::new(
                                template.span(),
                                format!("unknown variable in if: {}", var),
                            )
                            .to_compile_error()
                            .into();
                        }
                    } else {
                        return syn::Error::new(
                            template.span(),
                            format!("unsupported if syntax: {}", directive),
                        )
                        .to_compile_error()
                        .into();
                    }
                } else if directive == "endif" {
                    // close last if
                    let body = match body_stack.pop() {
                        Some(b) => b,
                        None => {
                            return syn::Error::new(template.span(), "unexpected {% endif %}")
                                .to_compile_error()
                                .into();
                        }
                    };
                    let (name, ident, expr) = match if_stack.pop() {
                        Some(x) => x,
                        None => {
                            return syn::Error::new(template.span(), "unexpected {% endif %}")
                                .to_compile_error()
                                .into();
                        }
                    };
                    body_stack.last_mut().unwrap().push(Node::If {
                        name,
                        ident,
                        expr,
                        body,
                    });

                    if chars.peek() == Some(&'\n') {
                        chars.next();
                    }
                } else {
                    return syn::Error::new(
                        template.span(),
                        format!("unknown directive: {}", directive),
                    )
                    .to_compile_error()
                    .into();
                }
            } else {
                buf.push(c);
            }
        } else {
            buf.push(c);
        }
    }

    if !if_stack.is_empty() {
        return syn::Error::new(template.span(), "unclosed {% if %} block")
            .to_compile_error()
            .into();
    }

    if !buf.is_empty() {
        body_stack.last_mut().unwrap().push(Node::Text(buf.clone()));
        buf.clear();
    }

    let nodes = body_stack.pop().unwrap_or_default();

    // Render nodes recursively into tokens (balanced)
    fn render_nodes(nodes: &[Node]) -> proc_macro2::TokenStream {
        use quote::quote;
        use syn::LitStr;
        let mut tokens = proc_macro2::TokenStream::new();
        for n in nodes {
            match n {
                Node::Text(s) => {
                    let lit = LitStr::new(s, proc_macro2::Span::call_site());
                    tokens.extend(quote! {
                        s.push_str(#lit);
                    });
                }
                Node::Expr(ident) => {
                    tokens.extend(quote! {
                        use std::fmt::Write as _;
                        write!(&mut s, "{}", #ident).unwrap();
                    });
                }
                Node::If { ident, body, .. } => {
                    let inner = render_nodes(body);
                    // Balanced block: if (__arg).is_some() { let __arg = (__arg).unwrap(); ... }
                    tokens.extend(quote! {
                        if (#ident).is_some() {
                            let #ident = (#ident).unwrap();
                            #inner
                        }
                    });
                }
            }
        }
        tokens
    }

    // Build initial bindings for each arg: let __arg_x = (<expr>);
    let mut bindings: Vec<proc_macro2::TokenStream> = Vec::new();
    for (name, expr) in &arg_exprs {
        let ident = arg_idents.get(name).expect("ident exists");
        bindings.push(quote! {
            let #ident = (#expr);
        });
    }

    let body_tokens = render_nodes(&nodes);

    let expanded = quote! {{
        use std::fmt::Write as _;
        #(#bindings)*
        let mut s = String::new();
        #body_tokens
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

    // collect args
    let mut arg_exprs: std::collections::HashMap<String, &Expr> = std::collections::HashMap::new();
    for arg in &args {
        let name = arg.name.to_string();
        if arg_exprs.contains_key(&name) {
            return syn::Error::new(arg.name.span(), format!("duplicate argument: {}", name))
                .to_compile_error()
                .into();
        }
        arg_exprs.insert(name, &arg.value);
    }

    // create idents
    let mut arg_idents: std::collections::HashMap<String, proc_macro2::Ident> =
        std::collections::HashMap::new();
    for name in arg_exprs.keys() {
        let ident =
            proc_macro2::Ident::new(&format!("__arg_{}", name), proc_macro2::Span::call_site());
        arg_idents.insert(name.clone(), ident);
    }

    // Node AST same as above
    enum Node<'a> {
        Text(String),
        Expr(proc_macro2::Ident),
        If {
            clauses: Vec<(String, proc_macro2::Ident, &'a Expr)>,
            body: Vec<Node<'a>>,
        },
    }

    let mut body_stack: Vec<Vec<Node>> = vec![Vec::new()];
    let mut if_stack: Vec<Vec<(String, proc_macro2::Ident, &Expr)>> = Vec::new();

    let mut chars = content.chars().peekable();
    let mut buf = String::new();

    while let Some(c) = chars.next() {
        if c == '{' {
            if chars.peek() == Some(&'{') {
                chars.next();
                if !buf.is_empty() {
                    body_stack.last_mut().unwrap().push(Node::Text(buf.clone()));
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
                if let Some(local_ident) = arg_idents.get(ident) {
                    body_stack
                        .last_mut()
                        .unwrap()
                        .push(Node::Expr(local_ident.clone()));
                } else {
                    return syn::Error::new(path.span(), format!("unknown placeholder: {}", ident))
                        .to_compile_error()
                        .into();
                }
            } else if chars.peek() == Some(&'%') {
                chars.next();
                if !buf.is_empty() {
                    body_stack.last_mut().unwrap().push(Node::Text(buf.clone()));
                    buf.clear();
                }
                let mut directive = String::new();
                while let Some(ch) = chars.next() {
                    if ch == '%' && chars.peek() == Some(&'}') {
                        chars.next();
                        break;
                    } else {
                        directive.push(ch);
                    }
                }
                let directive = directive.trim();
                if let Some(rest) = directive.strip_prefix("if ") {
                    // Split by "and"
                    let clauses: Vec<&str> = rest.split(" and ").map(|s| s.trim()).collect();

                    let mut clause_vars = Vec::new();
                    let mut checks = Vec::new();
                    for clause in clauses {
                        let tokens: Vec<&str> = clause.split_whitespace().collect();
                        if tokens.len() == 3 && tokens[1] == "is" && tokens[2] == "Some" {
                            let var = tokens[0];
                            if let (Some(expr), Some(local_ident)) =
                                (arg_exprs.get(var), arg_idents.get(var))
                            {
                                clause_vars.push((var.to_string(), local_ident.clone(), *expr));
                                checks.push(quote! { (#local_ident).is_some() });
                            } else {
                                return syn::Error::new(
                                    path.span(),
                                    format!("unknown variable in if: {}", var),
                                )
                                .to_compile_error()
                                .into();
                            }
                        } else {
                            return syn::Error::new(
                                path.span(),
                                format!("unsupported if clause: {}", clause),
                            )
                            .to_compile_error()
                            .into();
                        }
                    }

                    if_stack.push(clause_vars);
                    body_stack.push(Vec::new());

                    // trim following newline
                    if chars.peek() == Some(&'\n') {
                        chars.next();
                    }
                    if chars.peek() == Some(&'\n') {
                        chars.next();
                    }
                } else if directive == "endif" {
                    let body = match body_stack.pop() {
                        Some(b) => b,
                        None => {
                            return syn::Error::new(path.span(), "unexpected {% endif %}")
                                .to_compile_error()
                                .into();
                        }
                    };

                    let clauses = if_stack.pop().unwrap(); // Vec<(String, Ident, &Expr)>
                    body_stack.last_mut().unwrap().push(Node::If { clauses, body });

                    if chars.peek() == Some(&'\n') {
                        chars.next();
                    }
                    if chars.peek() == Some(&'\n') {
                        chars.next();
                    }
                } else {
                    return syn::Error::new(
                        path.span(),
                        format!("unknown directive: {}", directive),
                    )
                    .to_compile_error()
                    .into();
                }
            } else {
                buf.push(c);
            }
        } else {
            buf.push(c);
        }
    }

    if !if_stack.is_empty() {
        return syn::Error::new(path.span(), "unclosed {% if %} block")
            .to_compile_error()
            .into();
    }
    if !buf.is_empty() {
        body_stack.last_mut().unwrap().push(Node::Text(buf.clone()));
        buf.clear();
    }

    let nodes = body_stack.pop().unwrap_or_default();

    fn render_nodes(nodes: &[Node]) -> proc_macro2::TokenStream {
        use quote::quote;
        use syn::LitStr;
        let mut tokens = proc_macro2::TokenStream::new();
        for n in nodes {
            match n {
                Node::Text(s) => {
                    let lit = LitStr::new(s, proc_macro2::Span::call_site());
                    tokens.extend(quote! {
                        s.push_str(#lit);
                    });
                }
                Node::Expr(ident) => {
                    tokens.extend(quote! {
                        use std::fmt::Write as _;
                        write!(&mut s, "{}", #ident).unwrap();
                    });
                }
                Node::If { clauses, body } => {
                    let conds: Vec<_> = clauses
                        .iter()
                        .map(|(_, ident, _)| {
                            quote! { (#ident).is_some() }
                        })
                        .collect();
                    let inner = render_nodes(body);

                    // unwrap each inside the block
                    let unwrappers: Vec<_> = clauses
                        .iter()
                        .map(|(_, ident, _)| {
                            quote! { let #ident = (#ident).unwrap(); }
                        })
                        .collect();

                    tokens.extend(quote! {
                        if #(#conds)&&* {
                            #(#unwrappers)*
                            #inner
                        }
                    });
                }
            }
        }
        tokens
    }

    let mut bindings: Vec<proc_macro2::TokenStream> = Vec::new();
    for (name, expr) in &arg_exprs {
        let ident = arg_idents.get(name).expect("ident exists");
        bindings.push(quote! {
            let #ident = (#expr);
        });
    }

    let body_tokens = render_nodes(&nodes);

    let expanded = quote! {{
        use std::fmt::Write as _;
        #(#bindings)*
        let mut s = String::new();
        #body_tokens
        s
    }};
    expanded.into()
}
