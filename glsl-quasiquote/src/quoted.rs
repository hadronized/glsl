//! A set of small traits that enable tokenizing some common types that get tokenizing erased
//! normally, such as `Option<T>` as `Some(_)` or `None`, `Box<T>` as `Box::new(_)`, etc.

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

use glsl::syntax::{Identifier, TypeName};

// Quoted type.
pub trait Quoted {
  fn quote(&self) -> TokenStream;
}

impl Quoted for String {
  fn quote(&self) -> TokenStream {
    quote! { #self.to_owned() }
  }
}

impl<T> Quoted for Option<T>
where
  T: ToTokens,
{
  fn quote(&self) -> TokenStream {
    if let Some(ref x) = *self {
      quote! { Some(#x) }
    } else {
      quote! { None }
    }
  }
}

impl<T> Quoted for Box<T>
where
  T: ToTokens,
{
  fn quote(&self) -> TokenStream {
    quote! { Box::new(#self) }
  }
}

impl Quoted for Identifier {
  fn quote(&self) -> TokenStream {
    let s = &self.0;
    quote! { glsl::syntax::Identifier(#s.to_owned()) }
  }
}

impl Quoted for TypeName {
  fn quote(&self) -> TokenStream {
    let s = &self.0;
    quote! { glsl::syntax::TypeName(#s.to_owned()) }
  }
}
