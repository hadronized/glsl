use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput};

#[proc_macro_derive(NodeContents)]
pub fn node_contents(input: TokenStream) -> TokenStream {
  // Parse the input tokens into a syntax tree
  let input = parse_macro_input!(input as DeriveInput);

  // Add anonymous lifetimes as needed
  let lifetimes: Vec<_> = input.generics.lifetimes().map(|_| quote! { '_ }).collect();

  // Build the contents_eq method
  let contents_eq_body = match input.data {
    Data::Struct(ds) => {
      let mut current_expr = None;

      for (id, field) in ds.fields.iter().enumerate() {
        let field_id = field
          .ident
          .as_ref()
          .map(|id| quote! { #id })
          .unwrap_or_else(|| {
            let id = syn::Index::from(id);
            quote! { #id }
          });

        let this_field_expr = quote! { self.#field_id.contents_eq(&other.#field_id) };

        current_expr = Some(match current_expr {
          Some(before) => quote! { #before && #this_field_expr },
          None => this_field_expr,
        });
      }

      current_expr.unwrap_or_else(|| quote! { true })
    }
    Data::Enum(de) => {
      let mut arms = Vec::new();

      for variant in de.variants {
        let variant_name = &variant.ident;

        // Build the result expression
        let mut expr = None;
        // The left variant binding
        let mut bind_left = Vec::new();
        // The right variant binding
        let mut bind_right = Vec::new();

        let (bind_left, bind_right) = match variant.fields {
          // Enum variant with named fields: Enum::X { .. }
          syn::Fields::Named(named) => {
            for (id, field) in named.named.iter().enumerate() {
              let field_id = &field.ident;
              let left_field_id = format_ident!("a{}", id + 1);
              let right_field_id = format_ident!("b{}", id + 1);

              bind_left.push(quote! {
                #field_id: #left_field_id
              });
              bind_right.push(quote! {
                #field_id: #right_field_id
              });

              let this_field_expr = quote! { #left_field_id.contents_eq(&#right_field_id) };

              expr = Some(match expr {
                Some(before) => quote! { #before && #this_field_expr },
                None => this_field_expr,
              });
            }

            (
              quote! { { #(#bind_left),* } },
              quote! { { #(#bind_right),* } },
            )
          }
          // Enum variant with unnamed fields: Enum::X(..)
          syn::Fields::Unnamed(unnamed) => {
            for (id, _) in unnamed.unnamed.iter().enumerate() {
              let left_field_id = format_ident!("a{}", id + 1);
              let right_field_id = format_ident!("b{}", id + 1);

              bind_left.push(quote! { #left_field_id });
              bind_right.push(quote! { #right_field_id });

              let this_field_expr = quote! { #left_field_id.contents_eq(&#right_field_id) };

              expr = Some(match expr {
                Some(before) => quote! { #before && #this_field_expr },
                None => this_field_expr,
              });
            }

            (quote! { (#(#bind_left),*) }, quote! { (#(#bind_right),*) })
          }
          // Enum with no fields
          syn::Fields::Unit => {
            arms.push(quote! { (Self::#variant_name, Self::#variant_name) => true });
            continue;
          }
        };

        let expr = expr.unwrap_or_else(|| quote! { true });
        arms.push(
          quote! { (Self::#variant_name #bind_left, Self::#variant_name #bind_right) => #expr },
        );
      }

      quote! { match (self, other) {
        #(#arms),*,
        _ => false
      } }
    }
    _ => panic!("unsupported type for NodeContents derive"),
  };

  // Generate the name of the target for usage in impl targets
  let base_ident = input.ident;
  let struct_name = if lifetimes.is_empty() {
    quote! { #base_ident }
  } else {
    quote! { #base_ident<#(#lifetimes),*> }
  };

  // Build the output, possibly using quasi-quotation
  let expanded = quote! {
    #[automatically_derived]
    impl NodeContents for #struct_name {}
    #[automatically_derived]
    impl NodeContentsEq for #struct_name {
      fn contents_eq(&self, other: &Self) -> bool {
        #contents_eq_body
      }
    }
  };

  // Is this a "Data" node?
  let raw_name = base_ident
    .to_string()
    .strip_suffix("Data")
    .map(|id| format_ident!("{}", id));
  let expanded = if let Some(raw_name) = raw_name {
    let lifetimes: Vec<_> = input.generics.lifetimes().collect();
    let type_name = if lifetimes.is_empty() {
      quote! { #raw_name }
    } else {
      quote! { #raw_name<#(#lifetimes),*> }
    };

    quote! {
      #expanded

      pub type #type_name = Node<#struct_name>;

      impl<U> From<U> for #type_name
        where U: Into<#base_ident> {
          fn from(u: U) -> Self {
            Node::new(u.into(), None)
          }
      }
    }
  } else {
    expanded
  };

  TokenStream::from(expanded)
}
