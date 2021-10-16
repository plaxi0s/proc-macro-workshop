#![allow(unused, unused_macros)]
use std::collections::{HashMap, HashSet};

use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    parse_macro_input, parse_quote, parse_str, Attribute, Data::Struct, DeriveInput, Error, Field,
    Fields, GenericParam, Generics, Ident, Meta, NestedMeta, PredicateType, Token, Type, TypePath,
    WhereClause, WherePredicate,
};
macro_rules! show  {
    ($($e: expr), +$ (,)?) => {
        $(eprintln!("{:#?}",$e);)*
    };
}

fn get_field_attr<'a>(attr_name: &str, field: &'a Field) -> Option<&'a Attribute> {
    field
        .attrs
        .iter()
        .filter(|a| {
            a.path.segments.len() == 1 && a.path.segments.first().unwrap().ident == attr_name
        })
        .last()
}
fn add_debug_trait_bound(mut g: Generics) -> Generics {
    g
}
fn is_phantom_data(ty: &Type) -> bool {
    if let Some(outer) = get_outer(ty) {
        if vec!["PhantomData"].iter().any(|&o| *o == outer) {
            return true;
        }
    }
    false
}
// Gets the outer of a type Outer<SomeType> or None
fn get_outer(ty: &Type) -> Option<String> {
    match ty {
        Type::Path(tp) if tp.qself.is_none() => {
            let segments = &tp.path.segments;
            let outer_type = if segments.len() == 1 {
                segments.first().unwrap().ident.to_string()
            } else {
                segments.iter().fold(String::new(), |mut acc, s| {
                    acc.push_str("::");
                    acc.push_str(&s.ident.to_string());
                    acc
                })
            };
            Some(outer_type)
        }
        _ => None,
    }
}
// Gets the inner of a type SomeType<Inner> or none if it doesn't exist
fn get_inner<'a>(ty: &'a Type) -> Option<&'a Type> {
    match ty {
        Type::Path(tp) if tp.qself.is_none() => {
            let mut tp = tp
                .path
                .segments
                .iter()
                .skip_while(|s| s.arguments.is_empty());
            if let Some(segment) = tp.next() {
                match &segment.arguments {
                    syn::PathArguments::AngleBracketed(ab) if ab.args.len() == 1 => {
                        if let Some(syn::GenericArgument::Type(t)) = ab.args.first() {
                            return Some(t);
                        }
                        return None;
                    }
                    _ => return None,
                }
            }
            None
        }
        _ => None,
    }
}
fn get_associated_types<'a>(
    ty: &'a Type,
    generics_list: &[&Ident],
) -> Option<(&'a Ident, &'a TypePath)> {
    if let Some(inner) = get_inner(ty) {
        return get_associated_types(inner, generics_list);
    }
    if let Type::Path(tp) = ty {
        if tp.path.segments.len() < 2 {
            return None;
        }
        if generics_list.contains(&&tp.path.segments[0].ident) {
            return Some((&tp.path.segments[0].ident, tp));
        }
    }
    None
}
#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let data = match ast.data {
        Struct(s) => s,
        _ => {
            panic!("derive works only for struct")
        }
    };

    let fields = match data.fields {
        Fields::Named(f) => f.named,
        _ => {
            panic!("Builder derive only for named fields");
        }
    };

    let name = ast.ident;

    let extract_attr_value = |attr: &Attribute| match attr.parse_meta() {
        Ok(syn::Meta::List(ml)) => {
            if ml.path.segments.len() != 1 || ml.nested.len() != 1 {
                return None;
            }
            match ml.nested.first().unwrap() {
                NestedMeta::Meta(Meta::NameValue(nv)) => {
                    if !nv.path.is_ident("bound") {
                        panic!("bound not found");
                    }
                    match &nv.lit {
                        syn::Lit::Str(s) => {
                            return Some(s.value());
                        }
                        _ => panic!("expected str"),
                    }
                }
                _ => panic!("Not found named_value"),
            }
        }
        _ => panic!("not meta list"),
    };

    let bound_present = ast
        .attrs
        .iter()
        .filter_map(extract_attr_value)
        .collect::<Vec<_>>();

    let generic_tys = ast
        .generics
        .type_params()
        .map(|t| &t.ident)
        .collect::<Vec<_>>();

    let phantom_types = fields
        .iter()
        .filter_map(|f| {
            if is_phantom_data(&f.ty) {
                let inner = get_inner(&f.ty).unwrap();
                if let Type::Path(tp) = &f.ty {
                    return Some((get_outer(&inner).unwrap(), f.ty.clone()));
                }
            }
            None
        })
        .collect::<HashMap<_, _>>();

    let associated_types = fields
        .iter()
        .filter_map(|f| get_associated_types(&f.ty, &generic_tys))
        .collect::<Vec<_>>();

    let mut generics = ast.generics;

    if bound_present.is_empty() {
        for p in &mut generics.params {
            if let GenericParam::Type(ref mut t) = *p {
                if !phantom_types.contains_key(&t.ident.to_string())
                    && !associated_types.iter().any(|(id, tp)| *id == &t.ident)
                {
                    t.bounds.push(parse_quote!(::std::fmt::Debug));
                }
            }
        }
    } else {
        let mut where_clause = generics.make_where_clause();
        for b in bound_present {
            where_clause.predicates.push(parse_str(&b).unwrap());
        }
    }

    let mut where_clause = generics.make_where_clause();

    for (_, tp) in phantom_types {
        where_clause
            .predicates
            .push(parse_quote!(#tp: ::std::fmt::Debug));
    }
    for (_, at) in associated_types {
        where_clause
            .predicates
            .push(parse_quote!(#at: ::std::fmt::Debug));
    }

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let fmt_fields = fields.iter().map(|f| {
        let field_name = &f.ident;
        let fmt: String = if let Some(field_attr) = get_field_attr("debug", f) {
            match field_attr.parse_meta() {
                Ok(syn::Meta::NameValue(m)) => {
                    if let syn::Lit::Str(s) = m.lit {
                        s.value()
                    } else {
                        panic!("adw");
                    }
                }
                _ => todo!(),
            }
        } else {
            "{:?}".into()
        };
        quote! {
            .field(stringify!(#field_name),&format_args!(#fmt,&self.#field_name))
        }
    });

    let output = quote! {
        impl #impl_generics ::std::fmt::Debug for #name #ty_generics #where_clause{
            fn fmt(&self, f:&mut ::std::fmt::Formatter<'_>) -> ::core::result::Result<(),::std::fmt::Error>{
                f.debug_struct(stringify!(#name))
                #(#fmt_fields)*
                .finish()
            }
        }
    };

    output.into()
}
