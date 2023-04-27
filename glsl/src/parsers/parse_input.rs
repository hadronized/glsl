use std::cell::RefCell;
use std::num::NonZeroUsize;

use crate::syntax;

pub type ContextComments<'s> =
  std::collections::BTreeMap<syntax::NodeSpan, syntax::Node<syntax::Comment<'s>>>;

#[derive(Debug, Clone)]
pub struct ParseContextData<'s> {
  comments: Option<ContextComments<'s>>,
  current_id: NonZeroUsize,
  current_source: usize,
  source_ends: Vec<usize>,
}

impl<'s> ParseContextData<'s> {
  pub fn new() -> Self {
    Self {
      comments: None,
      current_id: unsafe { NonZeroUsize::new_unchecked(1) },
      current_source: 0,
      source_ends: Vec::new(),
    }
  }

  pub fn with_comments() -> Self {
    Self {
      comments: Some(Default::default()),
      current_id: unsafe { NonZeroUsize::new_unchecked(1) },
      current_source: 0,
      source_ends: Vec::new(),
    }
  }

  pub fn comments(&self) -> Option<&ContextComments<'s>> {
    self.comments.as_ref()
  }

  pub fn get_source(&self) -> Option<usize> {
    if self.current_source == 0 {
      None
    } else {
      Some(self.current_source - 1)
    }
  }

  pub fn set_source(&mut self, source: usize) {
    assert!(
      self.current_source == 0,
      "set_source can only be called once per ParseContextData"
    );

    self.current_source = source + 1;
  }

  pub fn source_end(&self, source_id: usize) -> Option<syntax::NodeSpan> {
    if source_id < self.current_source {
      return Some(syntax::NodeSpan::new_end(
        source_id,
        self.source_ends[source_id],
      ));
    }

    None
  }

  fn rollback(&mut self) {
    let remove_source_id = self.current_source - 1;

    // Clear source_ends
    self.source_ends.pop();

    // Decrement current_source
    self.current_source -= 1;

    // Remove comments referring to this source
    if let Some(comments) = self.comments.as_mut() {
      let mut spans = Vec::new();
      for cmt in comments.iter() {
        if cmt.0.source_id == remove_source_id {
          spans.push(*cmt.0);
        }
      }

      for span in spans {
        comments.remove(&span);
      }
    }
  }

  fn next_source(&mut self) -> usize {
    let res = self.current_source;
    self.current_source += 1;
    self.source_ends.push(0);
    res
  }
}

#[derive(Debug)]
pub struct ParseContext<'s, 'd> {
  data: RefCell<&'d mut ParseContextData<'s>>,
  source_id: usize,
}

pub struct ContextData<'b, 's, 'd> {
  guard: std::cell::Ref<'b, &'d mut ParseContextData<'s>>,
}

impl<'s> std::ops::Deref for ContextData<'_, 's, '_> {
  type Target = ParseContextData<'s>;

  fn deref(&self) -> &Self::Target {
    &*self.guard
  }
}

impl<'s, 'd> ParseContext<'s, 'd> {
  pub fn new(data: &'d mut ParseContextData<'s>) -> Self {
    Self {
      data: RefCell::new(data),
      source_id: 0,
    }
  }

  pub fn parse<'e, T, E>(
    &'e mut self,
    input: &'s str,
    f: impl FnOnce(ParseInput<'s, 'd, 'e>) -> Result<T, E>,
  ) -> Result<T, E> {
    self.source_id = self.data.borrow_mut().next_source();

    let res = f(ParseInput::new_extra(input, Some(self)));

    // In case parsing failed, we need to discard the results
    if res.is_err() {
      self.data.borrow_mut().rollback();
    }

    // Return parsing result
    res
  }

  pub fn add_comment(&self, cmt: syntax::Node<syntax::Comment<'s>>) {
    if let Some(c) = self.data.borrow_mut().comments.as_mut() {
      // If we're tracking comments we are also tracking spans
      c.insert(cmt.span.unwrap(), cmt);
    }
  }

  pub fn current_source(&self) -> usize {
    self.source_id
  }
}

pub type ParseInput<'c, 'd, 'e> =
  nom_locate::LocatedSpan<&'c str, Option<&'e ParseContext<'c, 'd>>>;
