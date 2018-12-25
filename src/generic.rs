//! Variable-size versions of the 32-bit based CodeMap data structures. 
//!
//! The basic `CodeMap` fits all positions into 32 bits, but gives the limitation that the total
//! amount of parsed source code not exceed 4GiB. It is possible to exceed this bound on some
//! systems with enough memory, so it is desirable to sometimes trade a larger positional
//! representation for more possible indexes - using `u64`, or possibly `usize`.
//! # Example
//! ```
//! use codemap::generic::CodeMap;
//! let mut codemap : CodeMap<usize> = CodeMap::new();
//! let file = codemap.add_file("test.rs".to_string(), "fn test(){\n    println!(\"Hello\");\n}\n".to_string());
//! let string_literal_span = file.span.subspan(24, 31);
//!
//! let location = codemap.look_up_span(string_literal_span);
//! assert_eq!(location.file.name(), "test.rs");
//! assert_eq!(location.begin.line, 1);
//! assert_eq!(location.begin.column, 13);
//! assert_eq!(location.end.line, 1);
//! assert_eq!(location.end.column, 20);
//! ```

use std::cmp::{self, Ordering};
use std::ops::{Add, Sub, Deref};
use std::fmt;
use std::sync::Arc;
use std::hash::{Hash, Hasher};

use num_traits::{Zero, One};

/// A short-hand for the requirements of a value representing a position
pub trait Positional : Zero + One + Add<Self, Output=Self> + Sub<Self, Output=Self> + From<usize> + Into<usize> + Copy + Hash + Eq + Ord + fmt::Debug {}

impl<P: Zero + One + Copy + Add<P, Output=P> + Sub<P, Output=P> + From<usize> + Into<usize> + Hash + Eq + Ord + fmt::Debug> Positional for P {}

/// A generic value representing a position in a `CodeMap`'s file.
#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct Pos<P: Positional>(P);

impl<P: Positional> Add<P> for Pos<P> {
    type Output = Pos<P>;
    fn add(self, other: P) -> Pos<P> {
        Pos(self.0 + other)
    }
}

impl<P: Positional> Sub<Pos<P>> for Pos<P> {
    type Output = P;
    fn sub(self, other: Pos<P>) -> P {
        (self.0 - other.0)
    }
}

/// A range of text within a CodeMap.
#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct Span<P: Positional> {
    /// The position in the codemap representing the first byte of the span.
    low: Pos<P>,

    /// The position after the last byte of the span.
    high: Pos<P>,
}

impl<P: Positional> Span<P> {
    /// Makes a span from offsets relative to the start of this span.
    ///
    /// # Panics
    ///   * If `end < begin`
    ///   * If `end` is beyond the length of the span
    pub fn subspan(&self, begin: P, end: P) -> Span<P> {
        assert!(end >= begin);
        assert!(self.low + end <= self.high);
        Span {
            low: self.low + begin,
            high: self.low + end,
        }
    }

    /// Checks if a span is contained within this span.
    pub fn contains(&self, other: Span<P>) -> bool {
        self.low <= other.low && self.high >= other.high
    }

    /// The position in the codemap representing the first byte of the span.
    pub fn low(&self) -> Pos<P> {
        self.low
    }

    /// The position after the last byte of the span.
    pub fn high(&self) -> Pos<P> {
        self.high
    }

    /// The length in bytes of the text of the span
    pub fn len(&self) -> P {
        self.high - self.low
    }

    /// Create a span that encloses both `self` and `other`.
    pub fn merge(&self, other: Span<P>) -> Span<P> {
        Span {
            low: cmp::min(self.low, other.low),
            high: cmp::max(self.high, other.high),
        }
    }
}

/// Associate a Span with a value of arbitrary type (e.g. an AST node).
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub struct Spanned<T, P: Positional> {
    pub node: T,
    pub span: Span<P>,
}

impl<T, P: Positional> Deref for Spanned<T, P> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.node
    }
}

/// A data structure recording source code files for position lookup.
pub struct CodeMap<P: Positional> {
    files: Vec<Arc<File<P>>>,
    _allow_priv: ()
}

impl<P: Positional> CodeMap<P> {
    /// Creates an empty `CodeMap`.
    pub fn new() -> CodeMap<P> {
        CodeMap {
            files: vec![],
            _allow_priv: ()
        }
    }

    /// Adds a file with the given name and contents.
    ///
    /// Use the returned `File` and its `.span` property to create `Spans`
    /// representing substrings of the file.
    pub fn add_file(&mut self, name: String, source: String) -> Arc<File<P>> {
        let low = self.end_pos() + P::one();
        let high = low + P::from(source.len());
        let mut lines = vec![low];
        lines.extend(source.match_indices('\n').map(|(p, _)| { low + P::from(p) + P::one() }));

        let file = Arc::new(File {
            span: Span { low: low, high: high },
            name: name,
            source: source,
            lines: lines,
        });

        self.files.push(file.clone());
        file
    }

    fn end_pos(&self) -> Pos<P> {
        self.files.last().map(|x| x.span.high).unwrap_or(Pos(P::zero()))
    }

    /// Looks up the `File` that contains the specified position.
    pub fn find_file(&self, pos: Pos<P>) -> &Arc<File<P>> {
        self.files.binary_search_by(|file| {
            if file.span.high < pos {
                Ordering::Less
            } else if file.span.low > pos {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        }).ok().map(|i| &self.files[i]).expect("Mapping unknown source location")
    }

    /// Gets the file, line, and column represented by a `Pos`.
    pub fn look_up_pos(&self, pos: Pos<P>) -> Loc<P> {
        let file = self.find_file(pos);
        let position = file.find_line_col(pos);
        Loc { file: file.clone(), position }
    }

    /// Gets the file and its line and column ranges represented by a `Span`.
    pub fn look_up_span(&self, span: Span<P>) -> SpanLoc<P> {
        let file = self.find_file(span.low);
        let begin = file.find_line_col(span.low);
        let end = file.find_line_col(span.high);
        SpanLoc { file: file.clone(), begin, end }
    }
}

/// A `CodeMap`'s record of a source file.
pub struct File<P: Positional> {
    /// The span representing the entire file.
    pub span: Span<P>,

    /// The filename as it would be displayed in an error message.
    name: String,

    /// Contents of the file.
    source: String,

    /// Byte positions of line beginnings.
    lines: Vec<Pos<P>>,
}

impl<P: Positional> File<P> {
    /// Gets the name of the file
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Gets the line number of a Pos.
    ///
    /// The lines are 0-indexed (first line is numbered 0)
    ///
    /// # Panics
    ///
    ///  * If `pos` is not within this file's span
    pub fn find_line(&self, pos: Pos<P>) -> usize {
        assert!(pos >= self.span.low);
        assert!(pos <= self.span.high);
        (match self.lines.binary_search(&pos) {
            Ok(i) => i,
            Err(i) => i - 1,
        })
    }

    /// Gets the line and column of a Pos.
    ///
    /// # Panics
    ///
    /// * If `pos` is not with this file's span
    /// * If `pos` points to a byte in the middle of a UTF-8 character
    pub fn find_line_col(&self, pos: Pos<P>) -> LineCol {
        let line = self.find_line(pos);
        let line_span = self.line_span(line);
        let byte_col = pos - line_span.low;
        let col = self.source_slice(line_span)[..P::into(byte_col)].chars().count();

        LineCol{ line: line, column: col }
    }

    /// Gets the full source text of the file
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Gets the source text of a Span.
    ///
    /// # Panics
    ///
    ///   * If `span` is not entirely within this file.
    pub fn source_slice(&self, span: Span<P>) -> &str {
        assert!(self.span.contains(span));
        &self.source[(P::into(span.low - self.span.low))..(P::into(span.high - self.span.low))]
    }

    /// Gets the span representing a line by line number.
    ///
    /// The line number is 0-indexed (first line is numbered 0). The returned span includes the
    /// line terminator.
    ///
    /// # Panics
    ///
    ///  * If the line number is out of range
    pub fn line_span(&self, line: usize) -> Span<P> {
        assert!(line < self.lines.len());
        Span {
            low: self.lines[line],
            high: *self.lines.get(line + 1).unwrap_or(&self.span.high)
        }
    }

    /// Gets the source text of a line.
    ///
    /// The string returned does not include the terminating \r or \n characters.
    ///
    /// # Panics
    ///
    ///  * If the line number is out of range
    pub fn source_line(&self, line: usize) -> &str {
        self.source_slice(self.line_span(line)).trim_right_matches(&['\n', '\r'][..])
    }

    /// Gets the number of lines in the file
    pub fn num_lines(&self) -> usize {
        self.lines.len()
    }
}

impl<P: Positional> fmt::Debug for File<P> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "File({:?})", self.name)
    }
}

impl<P: Positional> PartialEq for File<P> {
    /// Compares by identity
    fn eq(&self, other: &File<P>) -> bool {
        self as *const _ == other as *const _
    }
}

impl<P: Positional> Eq for File<P> {}

impl<P: Positional> Hash for File<P> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.span.hash(hasher);
    }
}

/// A line and column.
#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct LineCol {
    /// The line number within the file (0-indexed).
    pub line: usize,

    /// The column within the line (0-indexed).
    pub column: usize,
}

/// A file, and a line and column within it.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Loc<P: Positional> {
    pub file: Arc<File<P>>,
    pub position: LineCol,
}

impl<P: Positional> fmt::Display for Loc<P> {
    /// Formats the location as `filename:line:column`, with a 1-indexed
    /// line and column.
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}:{}:{}", self.file.name, self.position.line+1, self.position.column+1)
    }
}

/// A file, and a line and column range within it.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct SpanLoc<P: Positional> {
    pub file: Arc<File<P>>,
    pub begin: LineCol,
    pub end: LineCol,
}

impl<P: Positional> fmt::Display for SpanLoc<P> {
    /// Formats the span as `filename:start_line:start_column: end_line:end_column`,
    /// or if the span is zero-length, `filename:line:column`, with a 1-indexed line and column.
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.begin == self.end {
            write!(f, "{}:{}:{}", self.file.name, self.begin.line+1, self.begin.column+1)
        } else {
            write!(f, "{}:{}:{}: {}:{}", self.file.name, self.begin.line+1, self.begin.column+1, self.end.line+1, self.end.column+1)
        }
    }
}

#[test]
fn test_codemap() {
    let mut codemap = CodeMap::new();
    let f1 = codemap.add_file("test1.rs".to_string(), "abcd\nefghij\nqwerty".to_string());
    let f2 = codemap.add_file("test2.rs".to_string(), "foo\nbar".to_string());

    assert_eq!(codemap.find_file(f1.span.low()).name(), "test1.rs");
    assert_eq!(codemap.find_file(f1.span.high()).name(), "test1.rs");
    assert_eq!(codemap.find_file(f2.span.low()).name(), "test2.rs");
    assert_eq!(codemap.find_file(f2.span.high()).name(), "test2.rs");

    let x = f1.span.subspan(5, 10);
    let f = codemap.find_file(x.low);
    assert_eq!(f.name, "test1.rs");
    assert_eq!(f.find_line_col(f.span.low()), LineCol { line: 0, column: 0 });
    assert_eq!(f.find_line_col(f.span.low() + 4), LineCol { line: 0, column: 4 });
    assert_eq!(f.find_line_col(f.span.low() + 5), LineCol { line: 1, column: 0 });
    assert_eq!(f.find_line_col(f.span.low() + 16), LineCol { line: 2, column: 4 });

    let x = f2.span.subspan(4, 7);
    assert_eq!(codemap.find_file(x.low()).name(), "test2.rs");
    assert_eq!(codemap.find_file(x.high()).name(), "test2.rs");
}

#[test]
fn test_issue2() {
    let mut codemap = CodeMap::new();
    let content = "a \nxyz\r\n";
    let file = codemap.add_file("<test>".to_owned(), content.to_owned());

    let span = file.span.subspan(2, 3);
    assert_eq!(codemap.look_up_span(span), SpanLoc {
        file: file.clone(),
        begin: LineCol { line: 0, column: 2 },
        end: LineCol { line: 1, column: 0 }
    });

    assert_eq!(file.source_line(0), "a ");
    assert_eq!(file.source_line(1), "xyz");
    assert_eq!(file.source_line(2), "");
}

#[test]
fn test_multibyte() {
    let mut codemap = CodeMap::new();
    let content = "65°00′N 18°00′W 汉语\n🔬";
    let file = codemap.add_file("<test>".to_owned(), content.to_owned());

    assert_eq!(codemap.look_up_pos(file.span.low() + 21), Loc { file: file.clone(), position: LineCol { line: 0, column: 15 } });
    assert_eq!(codemap.look_up_pos(file.span.low() + 28), Loc { file: file.clone(), position: LineCol { line: 0, column: 18 } });
    assert_eq!(codemap.look_up_pos(file.span.low() + 33), Loc { file: file.clone(), position: LineCol { line: 1, column: 1 } });
}
