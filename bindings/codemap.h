#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct Arc_File Arc_File;

typedef struct String String;

typedef struct Vec_Arc_File Vec_Arc_File;

typedef struct Vec_Pos Vec_Pos;

/**
 * A small, `Copy`, value representing a position in a `CodeMap`'s file.
 */
typedef struct Pos {
  uint32_t _0;
} Pos;

/**
 * A range of text within a CodeMap.
 */
typedef struct Span {
  /**
   * The position in the codemap representing the first byte of the span.
   */
  struct Pos low;
  /**
   * The position after the last byte of the span.
   */
  struct Pos high;
} Span;

/**
 * A data structure recording source code files for position lookup.
 */
typedef struct CodeMap {
  struct Vec_Arc_File files;
} CodeMap;

/**
 * A line and column.
 */
typedef struct LineCol {
  /**
   * The line number within the file (0-indexed).
   */
  uintptr_t line;
  /**
   * The column within the line (0-indexed).
   */
  uintptr_t column;
} LineCol;

/**
 * A file, and a line and column within it.
 */
typedef struct Loc {
  struct Arc_File file;
  struct LineCol position;
} Loc;

/**
 * A file, and a line and column range within it.
 */
typedef struct SpanLoc {
  struct Arc_File file;
  struct LineCol begin;
  struct LineCol end;
} SpanLoc;

/**
 * A `CodeMap`'s record of a source file.
 */
typedef struct File {
  /**
   * The span representing the entire file.
   */
  struct Span span;
  /**
   * The filename as it would be displayed in an error message.
   */
  struct String name;
  /**
   * Contents of the file.
   */
  struct String source;
  /**
   * Byte positions of line beginnings.
   */
  struct Vec_Pos lines;
} File;

/**
 * Makes a span from offsets relative to the start of this span.
 *
 * # Panics
 *   * If `end < begin`
 *   * If `end` is beyond the length of the span
 */
struct Span subspan(const struct Span *self, uint64_t begin, uint64_t end);

/**
 * Checks if a span is contained within this span.
 */
bool contains(const struct Span *self, struct Span other);

/**
 * The position in the codemap representing the first byte of the span.
 */
struct Pos low(const struct Span *self);

/**
 * The position after the last byte of the span.
 */
struct Pos high(const struct Span *self);

/**
 * The length in bytes of the text of the span
 */
uint64_t len(const struct Span *self);

/**
 * Create a span that encloses both `self` and `other`.
 */
struct Span merge(const struct Span *self, struct Span other);

/**
 * Creates an empty `CodeMap`.
 */
struct CodeMap new(void);

/**
 * Adds a file with the given name and contents.
 *
 * Use the returned `File` and its `.span` property to create `Spans`
 * representing substrings of the file.
 */
struct Arc_File add_file(struct CodeMap *self, struct String name, struct String source);

/**
 * Looks up the `File` that contains the specified position.
 */
const struct Arc_File *find_file(const struct CodeMap *self, struct Pos pos);

/**
 * Gets the file, line, and column represented by a `Pos`.
 */
struct Loc look_up_pos(const struct CodeMap *self, struct Pos pos);

/**
 * Gets the file and its line and column ranges represented by a `Span`.
 */
struct SpanLoc look_up_span(const struct CodeMap *self, struct Span span);

/**
 * Gets the name of the file
 */
const str *name(const struct File *self);

/**
 * Gets the line number of a Pos.
 *
 * The lines are 0-indexed (first line is numbered 0)
 *
 * # Panics
 *
 *  * If `pos` is not within this file's span
 */
uintptr_t find_line(const struct File *self, struct Pos pos);

/**
 * Gets the line and column of a Pos.
 *
 * # Panics
 *
 * * If `pos` is not with this file's span
 * * If `pos` points to a byte in the middle of a UTF-8 character
 */
struct LineCol find_line_col(const struct File *self, struct Pos pos);

/**
 * Gets the full source text of the file
 */
const str *source(const struct File *self);

/**
 * Gets the source text of a Span.
 *
 * # Panics
 *
 *   * If `span` is not entirely within this file.
 */
const str *source_slice(const struct File *self, struct Span span);

/**
 * Gets the span representing a line by line number.
 *
 * The line number is 0-indexed (first line is numbered 0). The returned span includes the
 * line terminator.
 *
 * # Panics
 *
 *  * If the line number is out of range
 */
struct Span line_span(const struct File *self, uintptr_t line);

/**
 * Gets the source text of a line.
 *
 * The string returned does not include the terminating \r or \n characters.
 *
 * # Panics
 *
 *  * If the line number is out of range
 */
const str *source_line(const struct File *self, uintptr_t line);

/**
 * Gets the number of lines in the file
 */
uintptr_t num_lines(const struct File *self);
