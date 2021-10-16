#![allow(unused, unused_macros)]

use std::fmt::Display;

use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    parse_macro_input, Attribute, Data::Struct, DeriveInput, Error, Field, Fields, Ident, Token,
    Type,
};
fn camel(ident: &Ident) -> String {
    ident
        .to_string()
        .split('_')
        .collect::<Vec<&str>>()
        .iter()
        .fold(String::new(), |mut acc, s| {
            let (l, r) = s.split_at(1);
            acc.push_str(&l.to_ascii_uppercase());
            acc.push_str(r);
            acc
        })
}
fn is_option(ty: &Type) -> bool {
    if let Some(outer) = get_outer(ty) {
        if vec!["Option", "::std::option::Option"]
            .iter()
            .any(|&o| *o == outer)
        {
            return true;
        }
    }
    false
}
fn is_vec(ty: &Type) -> bool {
    if let Some(outer) = get_outer(ty) {
        if vec!["Vec", "::std::vec::Vec"].iter().any(|&o| *o == outer) {
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
fn get_inner(ty: &Type) -> Option<Type> {
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
                            return Some(t.clone());
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
fn get_field_error_name(ident: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("_{}MissingError", camel(ident)), ident.span())
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
fn get_compile_err<T: ToTokens, U: Display>(tokens: T, msg: U) -> Option<proc_macro2::TokenStream> {
    Some(Error::new_spanned(tokens, msg).to_compile_error())
}
#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let data = match ast.data {
        Struct(s) => s,
        _ => {
            panic!("derive works only for struct")
        }
    };
    let name = ast.ident;
    let builder_name = format_ident!("{}Builder", name);
    let fields = match data.fields {
        Fields::Named(f) => f.named,
        _ => {
            panic!("Builder derive only for named fields");
        }
    };

    let builder_fields: Vec<_> = fields
        .iter()
        .map(|f| {
            let field_ty = &f.ty;
            let builder_ty = if is_option(&f.ty) || is_vec(&f.ty) {
                quote! {#field_ty}
            } else {
                quote! {
                    ::std::option::Option<#field_ty>
                }
            };

            Field {
                ty: syn::parse2(builder_ty).unwrap(),
                attrs: Vec::new(),
                ..f.clone()
            }
        })
        .collect();

    let builder_defaults: Vec<_> = fields
        .iter()
        .map(|f| {
            let field_name = &f.ident;
            if is_vec(&f.ty) {
                quote! {
                    #field_name: ::std::vec::Vec::new()
                }
            } else {
                quote! {
                    #field_name: ::std::option::Option::None
                }
            }
        })
        .collect();

    let setters: Vec<_> = fields
        .iter()
        .filter_map(|f| {
            let field_ty = &f.ty;
            let field_name = &f.ident;
            if get_field_attr("builder", f).is_some() {
                return None;
            }
            if is_option(&f.ty) {
                let inner = get_inner(&f.ty).unwrap();
                Some(quote! {
                     fn #field_name(&mut self, #field_name: #inner) -> &mut Self{
                                    self.#field_name=::std::option::Option::Some(#field_name);
                                    self
                    }
                })
            } else if is_vec(&f.ty) {
                Some(quote! {
                     fn #field_name(&mut self, #field_name: #field_ty) -> &mut Self{
                            self.#field_name=#field_name;
                        self
                    }
                })
            } else {
                Some(quote! {
                    fn #field_name(&mut self, #field_name: #field_ty) -> &mut Self{
                        self.#field_name=::std::option::Option::Some(#field_name);
                        self
                    }
                })
            }
        })
        .collect();

    let vector_setters: Vec<_> = fields
        .iter()
        .filter_map(|f| {
            let field_ty = &f.ty;
            let field_name = &f.ident;
            if is_vec(field_ty) {
                if let Some(attr) = get_field_attr("builder",f) {
                    match attr.parse_meta() {
                        Ok(syn::Meta::List(m)) => {
                            if m.path.segments.len() != 1 {
                                return None;
                            }
                            if m.nested.len() != 1 {
                                return None;
                            }
                            match m.nested.first().unwrap() {
                                syn::NestedMeta::Meta(syn::Meta::NameValue(mv)) => {
                                    if !mv.path.is_ident("each") {
                                        return get_compile_err(&mv.path,"expected `builder(each = \"...\")`");
                                    }
                                    match &mv.lit {
                                        syn::Lit::Str(s) => {
                                            let field_push_method=
                                                Ident::new(&s.value(), s.span());
                                            let inner = get_inner(field_ty);
                                            return Some(quote! {
                                                pub fn #field_push_method(&mut self, val: #inner) -> &mut Self{
                                                    self.#field_name.push(val);
                                                    self
                                                }
                                            });
                                        }
                                        _ => {
                                            todo!();
                                        }
                                    }
                                }
                                _ => todo!(),
                            }
                            return Some(quote! {});
                        }
                        _ => {
                            return None;
                        }
                    };
                } else {
                    return None;
                }
            }
            None
        })
        .collect();
    let missing_field_err_impls: Vec<_> = fields
        .iter()
        .map(|f| {
            let field_name = &f.ident;
            let missing_field_err_name = get_field_error_name(&field_name.as_ref().unwrap());
            quote! {
                struct #missing_field_err_name{}
                impl ::std::error::Error for #missing_field_err_name{}
                impl ::std::fmt::Display for #missing_field_err_name{
                    fn fmt(&self, f:&mut ::std::fmt::Formatter<'_>) -> ::core::result::Result<(),::std::fmt::Error>{
                        write!(f,"field {} not set",stringify!(#field_name))
                    }
                }
                impl ::std::fmt::Debug for #missing_field_err_name{
                    fn fmt(&self, f:&mut ::std::fmt::Formatter<'_>) -> ::core::result::Result<(),::std::fmt::Error>{
                        write!(f,"field {} not set",stringify!(#field_name))
                    }
                }
            }
        })
        .collect();
    let build_fields: Vec<_> = fields
        .iter()
        .map(|f| {
            let field_name = &f.ident;
            let missing_field_err_name = get_field_error_name(&field_name.as_ref().unwrap());
            is_option(&f.ty);
            if is_option(&f.ty) || is_vec(&f.ty) {
                quote! {
                    #field_name: self.#field_name.clone()
                }
            } else {
                quote! {
                    #field_name: self.#field_name.clone().ok_or(#missing_field_err_name{})?
                }
            }
        })
        .collect();

    let expanded = quote! {
        #(#missing_field_err_impls)*

        pub struct #builder_name {
            #(#builder_fields),*
        }

        impl #name{
            pub fn builder() -> #builder_name{
                #builder_name{

                    #(#builder_defaults),*

                }
            }
        }

        impl #builder_name{
            #(#setters)*

            #(#vector_setters)*

            pub fn build(&mut self) ->::core::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>>{
                Ok(
                    #name{
                        #(#build_fields),*
                    }
                )
            }

        }

    };
    TokenStream::from(expanded)
}
