use std::fmt;

use crate::parsers::ParseInput;

/// Span information for a node, constructed from a nom_locate::LocatedSpan
#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, Hash)]
pub struct NodeSpan {
  /// The index of this span into the list of parsed units. This is used to
  /// identify which source string this span refers to when combining multiple ASTs
  pub source_id: usize,

  /// The offset represents the position of the fragment relatively to
  /// the input of the parser. It starts at offset 0.
  pub offset: usize,

  /// The line number of the fragment relatively to the input of the
  /// parser. It starts at line 1.
  pub line: u32,

  /// The column starting from the left (assuming ASCII text)
  pub column: u32,

  /// The length of the span represented by this structure
  pub length: usize,
}

impl NodeSpan {
  /// Return a 0-length span located at the start of the given source
  ///
  /// This may be used in span range queries.
  pub fn new_start(source_id: usize) -> Self {
    Self {
      source_id,
      offset: 0,
      line: 1,
      column: 0,
      length: 0,
    }
  }

  /// Return a 0-length span located at the end of the given source (as indicated by the offset)
  ///
  /// This may be used in span range queries.
  pub fn new_end(source_id: usize, length: usize) -> Self {
    Self {
      source_id,
      offset: length,
      line: 1,
      column: 0,
      length: 0,
    }
  }

  /// Return a 0-length span located at the end point of this span.
  ///
  /// This may be used in span range queries. Note that the line and column information will not be
  /// accurate.
  pub fn to_end_location(&self) -> Self {
    Self {
      source_id: self.source_id,
      line: self.line,
      column: self.column,
      offset: self.offset + self.length,
      length: 0,
    }
  }
}

impl std::convert::From<(usize, ParseInput<'_, '_, '_>)> for NodeSpan {
  fn from((source_id, span): (usize, ParseInput<'_, '_, '_>)) -> Self {
    Self {
      source_id,
      offset: span.location_offset(),
      line: span.location_line(),
      column: span.get_column() as u32,
      length: span.fragment().len(),
    }
  }
}

impl std::cmp::PartialOrd for NodeSpan {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.source_id.cmp(&other.source_id).then_with(|| {
      self
        .offset
        .cmp(&other.offset)
        .then_with(|| other.length.cmp(&self.length))
    }))
  }
}

pub trait NodeContents: fmt::Debug + Clone + PartialEq + Sized {}

/// A syntax node with span information
#[derive(Debug, Clone, PartialEq)]
pub struct Node<T: NodeContents> {
  pub contents: T,
  pub span: Option<NodeSpan>,
}

impl<T: NodeContents> Node<T> {
  /// Create a new syntax node with span information
  pub fn new(contents: T, span: Option<NodeSpan>) -> Self {
    Self { contents, span }
  }

  /// Return the wrapped syntax node, discarding the span information
  pub fn into_inner(self) -> T {
    self.contents
  }

  /// Map this contents of this node into a new node
  pub fn map<U: NodeContents>(self, f: impl FnOnce(T) -> U) -> Node<U> {
    Node {
      contents: f(self.contents),
      span: self.span,
    }
  }
}

impl<T: NodeContents> std::ops::Deref for Node<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self.contents
  }
}

impl<T: NodeContents> std::ops::DerefMut for Node<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.contents
  }
}

// Trivial copy for the node if the wrapped contents are Copy
impl<T: NodeContents + Copy> Copy for Node<T> {}

// Display implementation for wrapped node
impl<T: NodeContents + fmt::Display> fmt::Display for Node<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    <T as fmt::Display>::fmt(&self.contents, f)
  }
}

/// Trait for comparing the contents of syntax nodes
pub trait NodeContentsEq {
  fn contents_eq(&self, other: &Self) -> bool;
}

impl<T: NodeContents + NodeContentsEq> NodeContentsEq for Node<T> {
  fn contents_eq(&self, other: &Self) -> bool {
    self.contents.contents_eq(&other.contents)
  }
}

impl<T: NodeContentsEq, U: PartialEq> NodeContentsEq for Result<T, U> {
  fn contents_eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Ok(a), Ok(b)) => a.contents_eq(b),
      (Err(a), Err(b)) => a.eq(b),
      _ => false,
    }
  }
}

impl<T: NodeContentsEq, U: PartialEq> NodeContentsEq for Result<(&str, T), U> {
  fn contents_eq(&self, other: &Result<(&str, T), U>) -> bool {
    match (self, other) {
      (Ok((a1, a2)), Ok((b1, b2))) => a2.contents_eq(b2) && a1 == b1,
      (Err(a), Err(b)) => a.eq(b),
      _ => false,
    }
  }
}

impl<T: NodeContentsEq> NodeContentsEq for Option<T> {
  fn contents_eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Some(a), Some(b)) => a.contents_eq(b),
      (None, None) => true,
      _ => false,
    }
  }
}

impl<T: NodeContentsEq> NodeContentsEq for Vec<T> {
  fn contents_eq(&self, other: &Self) -> bool {
    if self.len() != other.len() {
      return false;
    }

    for (a, b) in self.iter().zip(other.iter()) {
      if !a.contents_eq(b) {
        return false;
      }
    }

    true
  }
}

impl<T: NodeContentsEq> NodeContentsEq for Box<T> {
  fn contents_eq(&self, other: &Self) -> bool {
    (**self).contents_eq(&**other)
  }
}

macro_rules! impl_node_contents_eq {
  ($t:ty) => {
    impl NodeContentsEq for $t {
      fn contents_eq(&self, other: &Self) -> bool {
        *self == *other
      }
    }
  };
}

impl_node_contents_eq!(());
impl_node_contents_eq!(bool);
impl_node_contents_eq!(char);
impl_node_contents_eq!(u16);
impl_node_contents_eq!(i32);
impl_node_contents_eq!(u32);
impl_node_contents_eq!(f32);
impl_node_contents_eq!(f64);
impl_node_contents_eq!(usize);
impl_node_contents_eq!(&str);
impl_node_contents_eq!(String);
impl_node_contents_eq!(std::borrow::Cow<'_, str>);

#[macro_export]
/// Replacement for assert_eq but using [`NodeContentsEq`] instead of [`PartialEq`]
macro_rules! assert_ceq {
    ($left:expr, $right:expr) => ({
        match (&$left, &$right) {
            (left_val, right_val) => {
                use $crate::syntax::NodeContentsEq;
                if !left_val.contents_eq(right_val) {
                    // The reborrows below are intentional. Without them, the stack slot for the
                    // borrow is initialized even before the values are compared, leading to a
                    // noticeable slow down.
                    panic!(r#"assertion failed: `left.contents_eq(right)`
  left: `{:?}`,
 right: `{:?}`"#, &*left_val, &*right_val)
                }
            }
        }
    });
    ($left:expr, $right:expr,) => ({
        assert_ceq!($left, $right)
    });
    ($left:expr, $right:expr, $($arg:tt)+) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                use $crate::syntax::NodeContentsEq;
                if !left_val.contents_eq(right_val) {
                    // The reborrows below are intentional. Without them, the stack slot for the
                    // borrow is initialized even before the values are compared, leading to a
                    // noticeable slow down.
                    panic!(r#"assertion failed: `left.contents_eq(right)`
  left: `{:?}`,
 right: `{:?}`: {}"#, &*left_val, &*right_val,
                           format_args!($($arg)+))
                }
            }
        }
    });
}
