//! A data structure for tracking source positions in language implementations, inspired by the
//! [CodeMap type in rustc's libsyntax](https://github.com/rust-lang/rust/blob/master/src/libsyntax/codemap.rs).
//!
//! The `CodeMap` tracks all source files and maps positions within them to linear indexes as if all
//! source files were concatenated. This allows a source position to be represented by a small
//! 32-bit `Pos` indexing into the `CodeMap`, under the assumption that the total amount of parsed
//! source code will not exceed 4GiB. The `CodeMap` can look up the source file, line, and column
//! of a `Pos` or `Span`, as well as provide source code snippets for error reporting.
//!
//! # Example
//! ```
//! use codemap::CodeMap;
//! let mut codemap = CodeMap::new();
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
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Deref, Sub};
use std::sync::Arc;

/// A small, `Copy`, value representing a position in a `CodeMap`'s file.
#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct Pos(u32);

impl Add<u64> for Pos {
    type Output = Pos;
    fn add(self, other: u64) -> Pos {
        Pos(self.0 + other as u32)
    }
}

impl Sub<Pos> for Pos {
    type Output = u64;
    fn sub(self, other: Pos) -> u64 {
        (self.0 - other.0) as u64
    }
}

/// A range of text within a CodeMap.
#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct Span {
    /// The position in the codemap representing the first byte of the span.
    low: Pos,

    /// The position after the last byte of the span.
    high: Pos,
}

impl Span {
    /// Makes a span from offsets relative to the start of this span.
    ///
    /// # Panics
    ///   * If `end < begin`
    ///   * If `end` is beyond the length of the span
    pub fn subspan(self, begin: u64, end: u64) -> Span {
        assert!(end >= begin);
        assert!(self.low + end <= self.high);
        Span {
            low: self.low + begin,
            high: self.low + end,
        }
    }

    /// Checks if a span is contained within this span.
    pub fn contains(self, other: Span) -> bool {
        self.low <= other.low && self.high >= other.high
    }

    /// The position in the codemap representing the first byte of the span.
    pub fn low(self) -> Pos {
        self.low
    }

    /// The position after the last byte of the span.
    pub fn high(self) -> Pos {
        self.high
    }

    /// The length in bytes of the text of the span
    pub fn len(self) -> u64 {
        self.high - self.low
    }

    /// Create a span that encloses both `self` and `other`.
    pub fn merge(self, other: Span) -> Span {
        Span {
            low: cmp::min(self.low, other.low),
            high: cmp::max(self.high, other.high),
        }
    }
}

/// Associate a Span with a value of arbitrary type (e.g. an AST node).
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.node
    }
}

/// A data structure recording source code files for position lookup.
#[derive(Default, Debug)]
pub struct CodeMap {
    files: Vec<Arc<File>>,
}

impl CodeMap {
    /// Creates an empty `CodeMap`.
    pub fn new() -> CodeMap {
        Default::default()
    }

    /// Adds a file with the given name and contents.
    ///
    /// Use the returned `File` and its `.span` property to create `Spans`
    /// representing substrings of the file.
    pub fn add_file(&mut self, name: String, source: String) -> Arc<File> {
        let low = self.end_pos() + 1;
        let high = low + source.len() as u64;
        let mut lines = vec![low];
        lines.extend(
            source
                .match_indices('\n')
                .map(|(p, _)| low + (p + 1) as u64),
        );

        let file = Arc::new(File {
            span: Span { low, high },
            name,
            source,
            lines,
        });

        self.files.push(file.clone());
        file
    }

    fn end_pos(&self) -> Pos {
        self.files.last().map(|x| x.span.high).unwrap_or(Pos(0))
    }

    /// Looks up the `File` that contains the specified position.
    pub fn find_file(&self, pos: Pos) -> &Arc<File> {
        self.files
            .binary_search_by(|file| {
                if file.span.high < pos {
                    Ordering::Less
                } else if file.span.low > pos {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            })
            .ok()
            .map(|i| &self.files[i])
            .expect("Mapping unknown source location")
    }

    /// Gets the file, line, and column represented by a `Pos`.
    pub fn look_up_pos(&self, pos: Pos) -> Loc {
        let file = self.find_file(pos);
        let position = file.find_line_col(pos);
        Loc {
            file: file.clone(),
            position,
        }
    }

    /// Gets the file and its line and column ranges represented by a `Span`.
    pub fn look_up_span(&self, span: Span) -> SpanLoc {
        let file = self.find_file(span.low);
        let begin = file.find_line_col(span.low);
        let end = file.find_line_col(span.high);
        SpanLoc {
            file: file.clone(),
            begin,
            end,
        }
    }
}

/// A `CodeMap`'s record of a source file.
pub struct File {
    /// The span representing the entire file.
    pub span: Span,

    /// The filename as it would be displayed in an error message.
    name: String,

    /// Contents of the file.
    source: String,

    /// Byte positions of line beginnings.
    lines: Vec<Pos>,
}

impl File {
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
    pub fn find_line(&self, pos: Pos) -> usize {
        assert!(pos >= self.span.low);
        assert!(pos <= self.span.high);
        match self.lines.binary_search(&pos) {
            Ok(i) => i,
            Err(i) => i - 1,
        }
    }

    /// Gets the line and column of a Pos.
    ///
    /// # Panics
    ///
    /// * If `pos` is not with this file's span
    /// * If `pos` points to a byte in the middle of a UTF-8 character
    pub fn find_line_col(&self, pos: Pos) -> LineCol {
        let line = self.find_line(pos);
        let line_span = self.line_span(line);
        let byte_col = pos - line_span.low;
        let column = self.source_slice(line_span)[..byte_col as usize]
            .chars()
            .count();

        LineCol { line, column }
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
    pub fn source_slice(&self, span: Span) -> &str {
        assert!(self.span.contains(span));
        &self.source[((span.low - self.span.low) as usize)..((span.high - self.span.low) as usize)]
    }

    /// Gets the span representing a line by line number.
    ///
    /// The line number is 0-indexed (first line is numbered 0). The returned span includes the
    /// line terminator.
    ///
    /// # Panics
    ///
    ///  * If the line number is out of range
    pub fn line_span(&self, line: usize) -> Span {
        assert!(line < self.lines.len());
        Span {
            low: self.lines[line],
            high: *self.lines.get(line + 1).unwrap_or(&self.span.high),
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
        self.source_slice(self.line_span(line))
            .trim_end_matches(&['\n', '\r'][..])
    }

    /// Gets the number of lines in the file
    pub fn num_lines(&self) -> usize {
        self.lines.len()
    }
}

impl fmt::Debug for File {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "File({:?})", self.name)
    }
}

impl PartialEq for File {
    /// Compares by identity
    fn eq(&self, other: &File) -> bool {
        self as *const _ == other as *const _
    }
}

impl Eq for File {}

impl Hash for File {
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
pub struct Loc {
    pub file: Arc<File>,
    pub position: LineCol,
}

impl fmt::Display for Loc {
    /// Formats the location as `filename:line:column`, with a 1-indexed
    /// line and column.
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}:{}:{}",
            self.file.name,
            self.position.line + 1,
            self.position.column + 1
        )
    }
}

/// A file, and a line and column range within it.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct SpanLoc {
    pub file: Arc<File>,
    pub begin: LineCol,
    pub end: LineCol,
}

impl fmt::Display for SpanLoc {
    /// Formats the span as `filename:start_line:start_column: end_line:end_column`,
    /// or if the span is zero-length, `filename:line:column`, with a 1-indexed line and column.
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.begin == self.end {
            write!(
                f,
                "{}:{}:{}",
                self.file.name,
                self.begin.line + 1,
                self.begin.column + 1
            )
        } else {
            write!(
                f,
                "{}:{}:{}: {}:{}",
                self.file.name,
                self.begin.line + 1,
                self.begin.column + 1,
                self.end.line + 1,
                self.end.column + 1
            )
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
    assert_eq!(
        f.find_line_col(f.span.low()),
        LineCol { line: 0, column: 0 }
    );
    assert_eq!(
        f.find_line_col(f.span.low() + 4),
        LineCol { line: 0, column: 4 }
    );
    assert_eq!(
        f.find_line_col(f.span.low() + 5),
        LineCol { line: 1, column: 0 }
    );
    assert_eq!(
        f.find_line_col(f.span.low() + 16),
        LineCol { line: 2, column: 4 }
    );

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
    assert_eq!(
        codemap.look_up_span(span),
        SpanLoc {
            file: file.clone(),
            begin: LineCol { line: 0, column: 2 },
            end: LineCol { line: 1, column: 0 }
        }
    );

    assert_eq!(file.source_line(0), "a ");
    assert_eq!(file.source_line(1), "xyz");
    assert_eq!(file.source_line(2), "");
}

#[test]
fn test_multibyte() {
    let mut codemap = CodeMap::new();
    let content = "65°00′N 18°00′W 汉语\n🔬";
    let file = codemap.add_file("<test>".to_owned(), content.to_owned());

    assert_eq!(
        codemap.look_up_pos(file.span.low() + 21),
        Loc {
            file: file.clone(),
            position: LineCol {
                line: 0,
                column: 15
            }
        }
    );
    assert_eq!(
        codemap.look_up_pos(file.span.low() + 28),
        Loc {
            file: file.clone(),
            position: LineCol {
                line: 0,
                column: 18
            }
        }
    );
    assert_eq!(
        codemap.look_up_pos(file.span.low() + 33),
        Loc {
            file: file.clone(),
            position: LineCol { line: 1, column: 1 }
        }
    );
}
