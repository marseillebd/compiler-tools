Common Concrete Syntax
v0.1 Draft Specification
by Marseille Bouchard
06 Dec 2025

Notes to Self
=============

Some "draft" standards have been in use for decades.
This document is truly a draft standard.

What I'm proposing is to separate parser from recognizer.

FIXME I was thinking about `::` token, and also `..` and `...` tokens. (Hmmm, how about a `.` symbol also?)
Ofc, I could just say "oh, you can only have one colon/dot at a time as a token", but I think I want ellipses at least.
Standard ellipses would come in handly for ranges in switch statements, which are quite popular.
Personally, I prefer Haskell's `::` type annotation to type theory's `:`, because `:` is already quite overloaded as a key-value separator in more familiar languages.
Ofc, one could simply disallow more than one colon at a time (and I might still!), except I want to promote type-theory-like syntax as an option.
So! I'm going to allow sequences of dots or colons up to length 3.
One dot/colon is punctuation, two and three are symbols, longer sequences are errors not interpreted as adjacent smaller tokens.
Why up to 3? Subitization.

Proofread: CSS -> CCS

Apparently, a conforming C++ implementation cannot disable features.
I suppose it makes sense, but diabling proven-dangerous (or even just questionable) features is necessary for growth.
How does a codebase moderninze without automated tools to at least identify rubbish, and the compiler is a great place to do this.
Conforming CCS source may use all CCS features, but it might also intentionally use only a subset.
A conforming CCS system must be able to process conforming CCS source in its default configuration, but it may also provide options to disallow various CCS features.

Roadmap
-------

At time of writing, this is v0.1.
It's meant to just get the basics out there.
Unicode support outside of comments and strings will be ignored.

I need first drafts of the following:
- [ ] preliminaries
  - [x] abstract
  - [ ] motivation
  - [x] goals
  - [ ] what to expect if you use CCS
  - [x] alternatives
  - [ ] referened standards
  - [ ] definitions (CCS source, CCS system...)
- [x] serialization
- [ ] covering lexer:
  - [ ] token stream data type
  - [ ] grammar
- [ ] cleaned lexer
  - [ ] token stream data type
  - [ ] grammar
- [ ] combine covering and cleaned lexers?
- [ ] concrete syntax tree data type
- [ ] cst grammar
- [ ] plan for growth
  - [ ] who hosts the source of truth, where; establishment of a governing body
  - [ ] a process for extensions (incl vendor-specific, and stanards-body from experiment to accepted), deprecation and removal
  - [ ] release process
  - we don't merely vote on acceptance/rejection of extensions/features: we also provide an "opinion", for both sides, much like the US Supreme Court does. That let's us see if previous decisions are still applicable based on recorded reasoning
- [ ] samples implementations
  - [ ] ofc the Haskell one
  - [ ] an awk script that can at least tokenize compliant source
- [ ] (v0.3?) suggested combinators for recognizer libraries

I already know I want to support a limited set of unicode characters in identifiers.
This is left for v0.2 (unless another major revision is needed in the meantime).

V1.0 won't be released until
- there's a documented review process
- the standard meets that review process
- we're ready to lock-in the features for good (probably after I've built at least one real language, and prolly stared on a second, though if we start to get more users, we'll lock-in faster)

Fragments
---------

### parts of the concrete syntax tree

atoms are numbers, symbols, string and multiline literals
combinations are enclosed by brackets/indentation or separated by commas/semicolons/dots/colons
words are atoms and combinations separated by whitespace
clauses are the "logical line": everything starting from the beginning of a line, continutin past indented block started on the line, through the end of the line the block ends on and any indents it starts, and so on unti a proper `Nextline` token

### codepoints, characters, unicode, blahblahblah

> **Rationale**
>
> The set of allowed codepoints has been chosen to make character-oriented manipulation and display abide by common (wrong) assumptions among programmers.
> Ambiguous glyphs in symbols, numbers, and punctuation
>
> Within strings and comments, these assumptions may be broken, and lead to incorrect rendering by CCS consumers.
> The risk of incorrect rendering here is offset by the ability to write comments in one's native language, and to allow CCS source to encode multilingual string literals.
> The risk of incorrect or ambiguous rendering of symbols and punctuation has much greater consequence to software produced with CCS which is not offset my multilingual advantage.
> While we could merely point at and blame text rendering software for its inadequacies and ascii-centric assumptions, we would rather specify a system that behaves well in the real world, where text rendering is fraught.
> Thus, we are liberal inside of strings and comments, and careful outside of them.
>
> A notable exception is the tab character, which breaks ubiquitous monospace assumptions.
> Its width is dependent not only on the configuration of several software systems, but also its placement within the rendering, as determined by the (potentially buggy) rendering system.
> Nevertheless, it seems to be significantly popular, and even possibly a means for accessibility.
> (I do not personally understand the accessibility claim, as I find myself often disoriented by tabs transmitted between systems with different tabstops, and I am aware of the existence fully-blind programmers. Nevertheless, I am in the habit of believing people's reports of their experience even when they differ from my own, especially when founded on a demonstable difference such as sightedness.)
> I have decided to allow it for indentation, which must not be mixed spaces/tabs (and within comments and multiline literals without restriction).
> This mitigates the width-variation, though you may still have to ensure that your editor, command line, and CCS system all agree on the tabstops.
> This allowance seems to address the concerns of those who prefer it.
> Basically, this standard does not wade into the tabs vs spaces debate.
>
> Someday, English will fail to be the lingua franca of technology and software.
> When that happens, this standard should be updated to allow migration to the new lingua franca.

### versioning

Downstream should pin a particular version+feature set for their implementations.
We want to change CCS over time (perhaps very slowly, but still), but we don't want the syntax of a program to change without the language giving their approval.

Introduction
============

The primary goal of "Common Concrete Syntax" (CCS) is to settle the most troublesome design choices for implementing parsers.
**In English**: define what source code "looks like", or perhaps, _should_ look like.
This then allows for generic implementations of CCS to be re-used across (potentially very-different) programming languages.
Such an implementation can also take care of routine but tricky work such as error reporting and recovery that separate toy from production languages.
(Rant: Not naming names, but even in my professional career, I have used popular language implementations with abysmal error reporting, such as reporting nothing but a line number which may not actually be accurate to where the fix must be made! Perhaps you have encountered similar toolchain troubles, and you certainly have if you have implemented any languages.)

> **Example**
>
> Consider the folowing code, in some unknown programming language.
> As humans, we can see that it defines a function for checking that some value is aligned.
> CCS does not recognize that much.
> Instead, it sees that e.g. there are two parenthesized comma-separated lists,
>   within each of the two logical lines (the `type` and `def` declarations),
>   that the second logical line includes a curly-bracketed indented list of lines,
>   and that there are number of symbols (e.g. `getLowMask`, `isAligned?`, `==`).
>
> ```
> type isAligned? :: (Size, Align) -> Bool
> def isAligned?(x, a) {
>    let lo = x & getLowMask(a)
>    lo == 0
> }
> ```
>
> The language implementor would still need to detect keywords, special syntax (e.g. `def <name>(<arglist>) {<stmt...>}`), and other forms of structure within the generic syntax tree.
> However, they would not need to concern themselves with the immense field of parsing algorithms.
> They simply traverse the concrete syntax tree the same way you would validate some JSON.
> (In fact, my Haskell implementation (TODO will) provides a very similar interface for "recognizing"

Usage
-----

The basic usage of CCS is summarized in the digram below.
First, recall that most programming language are implemented by sending source code through a pipeline of
 lexing, parsing, optionally static analysis, and generation of an intermediate representation.

A full CCS system would provide lexing and parsing into a generic CCS syntax tree type, the "concrete syntax tree".
From here, we expect the usuer to translate this generic representation into a language-specific "abstract syntax tree".
This translation is the "recognizer", and a full CCS system should provide a combinator library that supports writing these recognizers at a high level.
While the recognizer terminology is new, the process itself is much the same as validating JSON and loading it into an application-specific in-memory representation.

```
  User Code                            CCS System
-------------            ---------------------------------------

source code
            \___________________________> Lexer
             (calls)                        |
                                            V
                                       token stream
                                            |
                                            V
                          Combinator     Parser
Recognizer <____________/  Library          |
    |       (dependency)                    V
    V                                   concrete
  (call)  <_________________________/ syntax tree
    |      (argument)
    V
 abstract
syntax tree
    |
    V
 Rest of
 Frontend
```

Design Goals
------------

The design of CCS also attempts to satisfy a number of secondary goals:
- **Familiar**:
  The vast majority of programmers accept what is familiar, and reject what is unfamiliar.
  One might consider this a sad fact, but it is not so different from any other aspect of life, from food preferences to high art.
  This standard accepts that fact, and attempts to stay within the boundaries of what is already accepted in popular languages.
- **Ergonomic**:
  Ergonimic means that it is relatively easy to read/write the code correctly rather than incorrectly.
  If even novices don't struggle, the experts should truly have a smooth experience.
  This drives decisions to allow different meanings to also _look_ different, if only a little.
  (Eg a block of statements is understood differently from a list of parameters, so we allow both curly- and round-brackets for this purpose.)
  It also drives decisions like strict indentation rules;
    if the indentation doesn't match the brackets, something has likely gone wrong, and historically that issue has led to at least one major security vulnerability.

  Ergonomic is superficially similar to "familiar".
  To be sure, familiarity can aid ergonomics, but ergonomics refines mere familiarity.
  E.g. why not allow numbers in CCS to be directly followed by a dot like they can in a familiar language like Ruby?
  Consider `2.e8`: does this represent the number 200 million, or accessing the field named "e8" from the object representing the number 2?
  If you don't already know that CCS floating point literals must have digits on either side of the decimal point, you cannot reliably predict the answer.
  If you _do_ know, then you probably made a typo, and may need to address it in a way that CCS cannot reliably predict, so it should be flagged as an error.
- **Simple**:
  While the complexity of a generic syntax _could_ be hidden behind a simple interface, we want to keep the _required_ implementation complexity to a minimum.
  We expect third-party tools will need to process CCS syntax at a surface level, and may not have access to a full CCS library implementation.
  Examples include native syntax highlighting in editors, tools like `cloc`, code search tools (ie search for an identifier, ignoring its appearance in strings or comments), and so on.
  Further, humans have an easier time understanding CCS code when the implementation can be simpler: they sould be able to effectively simulate a parser in their head.
  (TODO one metric would be: can I at least lex CCS with plain bash or even sh?)
- **Extensible**:
  A software system that is not designed for growth is doomed to fracturing into dialects, confusion, and obsolecence.
  There must be room within the CCS standard to allow for features to be added or removed over time as experience reveals unexpected truths.
  That said, this growth must be principled: "move fast and break things" has the consequence that sometimes it feels like everything is always broken.
  TODO there must be a way to propose extensions, turn them on/off within the source file, and perhaps even specify the desired standard version in the source


Consequences
------------

If you adopt CCS as a basis of youre programming language syntax, there are a number of consequences.
We are not here to judge them good or bad; even though we thing CCS is good overall, there are obviously tradeoffs.

Based on my own implementation, CCS can be lexed with only a three-character lookahead, with the exception of multiline literals.
Actual lookahead in practice is bounded by the maximum size of multiline literal delimiters, which can also be as small as three characters.
Additionally, the parser state (apart from input text and position) consists of only a handful of states, one of which must hold the multiline literal delimiter.
It is very reasonable to persist this state between lines of source code so that tokenization can be updated online.
TODO Most of the context-sensitive part of the parser (really part of the lexer) only requires a one-token lookaround, and can be implemented as a chain of iterators/streams.
  Again, multiline literals are the exception, and require knowledge of the current indentation level so it can determine how much leading whitespace is part of the indentation within CCS and how much is part of the literal.
TODO The final parsing is context-free over the tokens the lexer generates.

You will have limited room for bikeshedding.
If you disagree with the standard's choices, that could perhaps be frustrating.
However, I suggest that a) you will not look back on your life and wish you had done more bikeshedding, and b) you'll get used to an understand our choices with experience.
Part (a) is perhaps self-evident, but to support (b), I suggest you look at the variety of design choices that you disagree with, but nevertheless no longer think about.
You already know a dozen syntaxes, and how may of them are so bad that you don't use them, compared to the number you do use?
Is CCS more similar to YAML, or a sudoers file? You'll be fine.

Related Work
------------

There is a similarity with s-expressions (sepxrs) here.
Sexprs define a few types of syntax (comments, atoms, lists, improper lists), for which a parser is generally provided.
A language implementor then traverses the sexpr tree and interprets the structure with programming language semantics, either directly, or via a special-purpose syntax tree type.

Since s-exprs already fulfill the primary goal, why invent something new?
- Sexprs have been roundly rejected by the programming community at-large in favor of a menagerie of annoyingly-different Algol-derived syntaxes.
  Where lispers commonly look down on this rejection, I also reject sexprs for source code, despite spending my childhood writing Scheme.
  What a lisper might see as "line noise" (different kinds of brackets and punctuation), I see as landmarks that help me orient myself within the code.
- It has been difficult for me to identify a well-accepted standard fo sexprs.
  The Lisp ecosystem appears to be both highly decentralized and ready to create incompatible new variants.
  I suspect this makes it difficult for any other programmer to take advantage of their work.
- Don't even get me started on reader macros. TODO

By emphasizing familiar constructs, ergonomics, and a unified standard, I hope CCS will address the deficiencies of sexprs for source code.
I nevertheless endorse sexpr-based formats for "intermetiate" formats such as WebAssembly text formatch are primarily for machine comsumption, but also human-readable in a pinch.

Several other one-off languages have been embedded into generic text-based data serialization formats.
Obsidian is a knowledge-management system with a fist-party plugin, Bases, that can query its notes database and present information in tabular format.
The Obsidian GUI provides a way to write simple queries, but advanced usage requires editing the `.base` file, which is just a YAML file.
Of course, it's fairly easy to edit the YAML, but I still prefer to use the GUI.
In my professinoal work, I've had to implement a query language against an event stream, which would be written by cybersec experts.
I shipped it quickly by embedding the language in JSON, as we already had high-performance parser and validation libraries.
The issue we faced was that it was annoying to have to type a lot of quote marks and curly braces, but that's not a problem that would happen if I had access to CCS.
I'm sure readers can offer their own examples.

Preliminaries
=============

TODO
- uses [RFC2119](https://datatracker.ietf.org/doc/html/rfc2119) - Key words for use in RFCs to Indicate Requirement Levels
- also, RFC2119 doesn't define it, but I'm gonna use DEPRECATED
- I'm thinking about hewing close to standards language, but I think I'd like to favor a more mathematical description as the source of truth (normative).
- I'll need some BNF-alike, but I probably want to use PCRE regexes for their relative familiarity to average coders
- I reference Unicode, Utf-{8,16,32}, so I need links

TODO:
There's a distinction between a compliant CCS system and a compliant CCS source.
While the source is required to do things, a system processing that source might not be required to reject the input if error recovery is performed.
CCS systems include producers and consumers (transformers are just the composition of consumer and producer).
They may also include a combinator library, but this standard doesn't specify that interface (yet).

I want to accompany all normative text with a rationale.
Truly, why don't standards do this as a matter of course?
It takes more work, yes, but it also is immesurably helpful for
  a) the reader to contextualize the dry specifications with real-world consequences of the standard body's decisions,
  b) the writer to detect specifications that are badly- or not fully-motivated,
  c) the editor to not redo from scratch the work the writer did to make the normative decision in the first place.

Data Types
==========



TODO explain/adjust what I've described here

The sign is kept for floating point literals because IEE754 says negative zero is a thing.
It is kept for integer literals because a) they could be silently converted to floating point by downstream, and b) we may for some reason want a signed zero for simulating sign-magnitude arithmetic or just some obscure math concept like the line with two origins; I suspect most downstreams will ignore the sign for zero magnitude
The radix is kept for floating point literals because it may be an important part of indicating precision: eg scientific notation encodes uncertainty in how many (or rather, how few) significant figures are present, for which we must know how big a jump each digit is.
Note that this also means we cannot throw away trailing zeros by compressing floats that are multiples of the radix: `1.00e4 /= 1e2` because the significant figures are different, and this is represented with `Flo(100, base10, 4) /= Flo(1, base10, 2)`.
Of course, specific downstream types may _later_ performa set quotient operation.

```
CST ::= Atom
     |  Enclose Encloser CST
     |  Pair CST CST // key, value
     |  List Separator CST*
     |  Template Text (CST Text)+

Atom ::= Symbol Text // but the text has requirements: matches /[:id:]+|:{2,3}|.{2,3}/ - /[+-]?\d.*/
      |  IntegerLiteral {+,-} ℕ
      |  FloatingLiteral {+,-} ℕ {2,8,16} ℤ // ±a * r^b
      |  DecimalFloatingLiteral {+,-} ℕ ℤ // ±a * 10^b
      |  StringLiteral Text
      |  MultilineLiteral Text*

Encloser ::= Round
          |  Square
          |  Curly
          |  Indent

Separator ::= Semicolon
           |  Comma
           |  Space
           |  Dot // which may also include a(b) a[b], and so on
           |  Colon // qualified names `a:b`, and qualified literals `i32:-1`
```

Grammar
=======

The Structure of Input Lines
----------------------------

CCS source is line-oriented (see section "Serialization").
There are two types of lines: standard and embedded.
Embedded lines are used to build multi-line literals, while standard lines cover everything else.

### Standard Lines

Each standard line has an internal structure, in order: leading indentation, content, and trailing whitespace &/ comment.
We use the names "indentation", "content", and "backmatter" for these structures.

CSS source lines MAY end in text matching the regex `/\s*(#.*)?$/`.
If a line does end in this way, the largest matching input sequence is defined as the backmatter.
We can see it is split into trailing whitespace and a line-delimited comment.
Systems MUST NOT map two source inputs to different outputs when the inputs differ only in their backmatter.
Systems MAY allow downstream consumers to process backmatter (e.g. to enable language pragmas, or warn about trailing whitespace).
We say that a (standard) line is "trivial" when it consists only of backmatter.

> **Rationale**
>
> Note that CSS only has line-delimited comments, not block comments.
> Nesting block comments are context-free, which excludes simple lexers (see the Simple principle).
> Non-nesting block comments can be a source of confusion and busywork when attempting to comment out a lot of code but get interrupted by an existing block comment (see the Ergonomic principle).
>
> Line-delimited comments are sufficient, easy to parse, already support nesting (line comments trivially embed within themselves), and have good editor support.
> Many languages, especially scripting languages already eschew block comments without attracting complaints.

Compliant source MUST have every non-trivial line begin with zero or more repetitions of the "indent sequence".
This is the indentation, and the "indentation level" of the line is the number of repetitions.
The indent sequence MUST be either a non-zero number of space characters or a single tab character.
The indent sequence MUST NOT vary between lines, but MAY vary between separate sources.
Compliant source MUST have its first non-empty content line be unindented (zero indentation level).
Compliant source MUST NOT have whitespace at the start (or end) of each line's content.

> **Rationale**
>
> As far as I can see, there is no reason to vary the size of an indent within a file.
> The only widely-used ways to indent are either with tabs or some number spaces.
> I don't know of any reason why one would want _multiple_ tabs to indicate a single indent level.
>
> I am willing to deprecate tab as an indent sequence due to its unpredictability in text rendering (especially for manipulated text).
> However, I also see no oreason to get into tabs vs spaces, and so it is included from the outset.
> Perhaps someday, we'll have our entire tool suite just render indentation semantically.

A compliant system MUST begin parsing CSS source as standard lines.
When (and only when) a line's contents contains a multi-line string delimiter, the subsequent lines will be parsed as embedded, up to but noto including a line matching the end delimiter.
Multi-line string delimiters are described as part of the tokenization process.

### Embedded Lines

Embedded lines also have internal structure, in order: leading indentation, and content.
An embedded line which matches `/^\s*$/` is called "trivial".

A non-trivial embedded line must begin with a number of indent sequences equal to the indentation level of the line holding the multi-line string's open delimiter.
This is the indentation part of the line, and the rest is content.
The rest of the line is content, and unlike standard line content, MAY begin with whitespace.
The content of embedded lines may also contain tabs anywhere.

A trivial line has fewer restrictions on its indentation.
Either the expected indentation (as for non-trivial embedded lines) is a prefix of the trivial line,
    in which case that prefix forms the indent part and the remainder the content,
    or else the entire line is a prefix of the expected indentation,
    in which case it is valid indentation and the content is empty.
Once indentation has been stripped from embedded lines, it is no longer necessary.

> **Ratinoale**
>
> We want to be able to embed arbitrary text into CCS for later processing.
> Arbitrary text may contain leading whitespace, often not at all what is expected (consider makefiles, which must have tab-indentation).
> We also don't want to break the "offsides rule": that semantic objects cannot contain syntax less-indented than its start/end delimiters.
> The combination of these constraints means we need a rule to determine what part of the embedded line is indented and which is not.
>
> The limitation to fixed multiples of an indent sequence gives us an easy solution: just use the last indent level.
> We relax this for trivial lines, since we would like to avoid trailing whitespace characters in all contexts: it should not alter the semantics.
>
> It might be possible to require an additional indent level for the embedded lines beynod the level of the open delimiter.
> I decided against this because I would like unindented multiline literals (eg for documentation strings) to not need indentation on the embedded lines.
> I suspect in real languages, it will not be an issue that these embedded lines are not further indented, as most literals will simply be bound to identifiers immediately, rather than being part of a larger expression.

Any line beginning with the matching multi-line string delimiter, possibly after whitespace, ends the sequence of embedded lines.
While such a series of inputs MUST end the literal regardless, the line of source that terminates the literal MUST have the same indentation level as the rest of the literal and the line it started on.
The contents of the multi-line string literal are just the contents of the embedded lines within it.
Because the content ofo the lines are kept separate, it is left to downstream to decide how (or whether!) to glue these lines together (with what newline characters, and whether there's a newline at the end are reasonable variations).
Wherever possible, the unmoodified structure of the lines, including source positions, SHOULD be sent downstream.

> **Rationale**
>
> In the interest of making quick-and-dirty lexers easy to write, we have formulated these rules so that a system need not track indentation levels merely to classify lines as either standard or embedded.
> This comes with the added benefit that it is difficult to confuse a human reader by (accidentally) altering indentation level of the end delimiter:
>   the reader need not care about the details of whitespace, the system will correct any writers.

Tokens
------

### Basic Tokens

Each line of standard content is broken into tokens according to the following table.
Matching is according to regex rules, each beginning at the start of unprocessed input and extending as far as possible.
We use the syntax `<foo>` in these rules to serve as points for longer, less redable regexes to be substituted.
Those helper regexes are defined in the list below.
Note that some listed tokens (comment, indentation) cannot appear in the content of a line, but we have listed them anyway for quick reference.

| name                    | regex                              | notes |
|-------------------------|------------------------------------|-------|
| enclosing punctuation   | `[()[\]{}]`                        |       |
| separating punctuation  | `[,;]|\.+|:+`                      |       |
| whitespace              | `[ ]+`                             |       |
| indentation             | `^[ \t]+`                          |       |
| comment                 | `#.*`                              |       |
| symbol                  | `[:id:]+` - `[+-]?[0-9].*`         |       |
| backslash               | `\\`                               |       |
| decimal int             | `<sign><dec>`                      |       |
| binary int              | `<sign>0[bB]<bin>`                 |       |
| octal int               | `<sign>0[oO]<oct>`                 |       |
| hexadecimal int         | `<sign>0[xX]<hex>`                 |       |
| deciaml float           | `<sign><dec>\.<dec>(<dExp>)?`      |       |
| binary float            | `<sign>0[bB]<bin>\.<bin>(<bExp>)?` |       |
| octal float             | `<sign>0[oO]<oct>\.<oct>(<bExp>)?` |       |
| hexadecimal float       | `<sign>0[xX]<hex>\.<hex>(<bExp>)?` |       |
| "sql" string            | `'([^']+|'')*'`                    |       |
| string literal          | `"<str>*"`                         |       |
| open string template    | `` "<str>*` ``                     |       |
| middle string template  | `` `<str>*` ``                     |       |
| close string template   | `` `<str>*" ``                     |       |
| multiline delimiter     | `"""+[A-Za-z]*`                    |       |

Regex Abbreviations:
- for symbols:
    - `[:id:] === [-_a-zA-Z0-9!$%&*+/<=>?@^|~]`
- for numbers:
    - `<sign> === [+-]?`
    - `<bin> === [01]+(_[01]+)*`
    - `<oct> === [0-7]+(_[0-7]+)*`
    - `<dec> === [0-9]+(_[0-9]+)*`
    - `<hex> === [0-9a-fA-F]+(_[0-9a-fA-F]+)*`
    - `<dExp> === [eE]<sign><dec>`
    - `<bExp> === [pP]<sign><dec>`
- for strings:
    - `<str> === <strChar>+|\\(<cEsc>|<xEsc>|<uEsc>)`
    - ``<strChar> === [^"`\\]`
    - ``<cEsc> === \\[0abefnrt'"`\\]``
    - `<xEsc> === x[0-9a-fA-F]{2}`
    - `<uEsc> === u\{[0-9a-fA-F]{1,6}\}`

> **Rationale**: Numerical Literals
>
> Binary, octal, deciaml, and hexadecimal literals are all in common use.
> Of these, octal is perhaps the least used, as 3n-bit groups usually do not align well with 8-bit byte computation.
> However, there is at least one thing (file permissions) that still group naturally in threes, so I'm loath to throw it out.
>
> Integer and floating point literals are generally very different numerical representations.
> Making a type or especially confusing them while skimming could lead to bugs.
> Additioally, the dot operator is often used in programming languages, but we would like to keep it clearly separate (again for typ/skimming reasons) from the decimal point.
> We have chosen that all floating point literals contain a decimal point, and that there must be digits on either side of that decimal point.
>
> Only floating point literals are allowed to have exponents.
> It is expected that floats have limited precision which can slide across a range of magnitures, and that they may be rouonded or converted to non-number values (+/- inf, NaN).
> The same does not hold true for ordinary integer data, which we expect may be stored in a precise format.
> Allowing large exponents for integers literals may even consume all available memory when the parser represents them.
> Thus, only floating piont literals are allowed to have an exponent.
> In the future, I may have `<dec>^<dec>` for large, simple, positive numbers like `2^21` or `10^12`, but I doubt I'd allow anything other than positive decimal integers represented this way.
>
> Only decimal expoonents are allowed, because their purpose is to encode how far to shift the decimal point, for which we need not care about bit representations.
> Binary, octal, and hexadeciaml are meant to easily represent bit-patterns, not express simple counts.

> **Rationale**: Strings
>
> Sql strings are included as a way to write strings that commonly have embedded backslashes (or other special characters), such as regular expressions or Windows filepaths.
> The only escape character is tick, which is its delimiter, and the only escapable character is the tick character.
> This makes them very easy to parse, and usuallyeasy to write, as long as it doesn't contain many contractions.
>
> Double-quote strings can be templated using backticks.
> The intention is that CCS inside backticks would be evaluated and spliced into the string.
> The escape sequences are drawn from major programming languages.
>
> The multiline delimiter changes the syntax of following lines, and so should be carefully detected.
> Thankfully, the triple-quote sequence it must contain cannot appear in any tokens other than comments and sql strings, making them simple to identify.

If a multiline delimiter is at the start of a line's content and the prior line is embedded, that is a closing delimiter.
Otherwise, it is an opening delimiter.
A single line can have at most one opening multiline delimiter and at most one closing multiline delimiter.

### Assembling Lines

TODO we join the tokenized lines together like so:
- emit tokens from the first line (which will be standard), then continue to the next line
- if the line is standard, emit indentation tokens, then the tokens from the line
    - if the indent of this line is one more than the last, emit an indent token
    - if it is the same as the last, emit a nextline token
    - if it is less than the last, emit dedent tokens in number equal to the difference
    - if it is more than one level more than the last, that is an error
- if the line has an open multiline delimiter, emit a multiline token
    - the token begins at the open delimiter and ends at the close delimiter (on the next standard line)
    - the token contains all the embedded lines between the delimiters
    - continue emitting tokens from the next line from after the close delimiter

### Tokens in Context

TODO
- atoms are symbols, numbers, and strings (incl multiline)
- atoms cannot appear adjacent to each other
- an "implicit chain" aka "subscript" token is inserted between
    - an atom followed by open punctuation
    - open punctuation followed by open punctuation
- a "chain" aka "access" token is a single dot sandwiched between
    - two atoms
    - close punctuation and an atom
    - ? an atom and open punctuation
    - ? open punctuation and open punctuation
- a single colon can be
    - a "pairing" token when there is whitespace on the right
    - a "block marker" when it is at the end of a line
    - a "qualifier" token when sandwiched between
        - ?? two symbols
        - ? a symbol and a literal
- other colons and dots, up to a length of three, are symbols
    - ? including single dot
    - ? including single colon
    - subject to the rule that atoms cannot be adjacent
- colons/dots with a length over three are illegal

Serialization
=============

Serialization describes how CCS input and output data structures are realized in physical media as "CCS source".
It includes storage (on hard drives, or other non-volitile media), transmission over a wire (both sending and receiving), or even reading from user input.
(That is, we don't refer to a source file, because the input might actually come from the keyboard, the network, and so on, and we might write to network or terminal rather than a file on disk.)

> **Aside**
>
> I consider data transmission and storage to be not just similar or related, but the exact same thing.
> Philosophically, data storage is equivalent to transmitting that data to your future self from the present.
> Scientifically, space and time are subjective views on objective, unified space time (with a conversion factor of about one foot per nanosecond);
>   if storage is supposed to extend over "time" and transmission between two points in "space", this is an illusiory distinction,
>   because both simply carry information along a time-like curve.
> Practically, sneakernet systems leverage data sotrage (hard drives, thumb drives and the like) to perform transmission.
> Historically, some early computers used transmission to store data, at least for short times:
>   EDSAC used mercury delay lines for its volitile memory, bits were transmitted along mercury-filled tubes, and routed electrically back to the start of the tube to store the data.
>
> If I had to pick a name that includes both, I suppose it would be "serialization".

Text Codec
----------

CCS source is a fundamentally line-based text format, but it must nevertheless be represented on modern computers.
CSS represents text as a sequence of Unicode codepoints (numbers in the inclusive range 0x0--0x10FFFF, excluding 0xD800--0xDFFF, written as `U+<hex number>`).

A compliant system MUST be able to process CSS source as a stream of unicode codepoints encoded in UTF-8.
A compliant system MAY process codepoints in other encodings, provided they are both ASCII-compatible and cover all unicode codepoints.
It it RECOMMENDED that source encoded in any way other than UTF-8 have an encoding comment, and that a system only process it if that comment is present, preferably early enough in the source to be preceded only by ASCII characters.
TODO: there's a standard for these encoding specifiers, yeah? Python uses it.
A compliant CCS source MUST NOT include a byte-order mark (BOM), nor mix different encodings.
Compliant CSS source MUST NOT be encoded with any encoding that does not cover the unicode codepoint space, or is not ASCII-compatible, and compliant CCS systems SHOULD NOT process them.
This includes, but is not limited to:
- ISO-8859-1
- Windows 1251
- Windows 1252

> **Rationale**
>
> Unicode has won the character set wars.
> It is a superset of every prior character set.
> Utf-8 has won the encoding wars; according to [Wikipedia](https://en.wikipedia.org/wiki/Popularity_of_text_encodings), Utf-8 is used in 95+% of web traffc.
> I don't know enough about CJK text usage to state a reliable stance on GB 18030 or other CJK-oriented encodings.
> Non-ASCII-compatible encodings seem to serve the sole purpose of rendering mojibake; they also complicate looking for an encoding comment.
>
> We assume that CSS systems are deployed on platforms with an 8-bit byte for memory, storage, and transmission.
> That's a reasonable assumption for any practical code written for servers and desktops/laptops/phones within a few decades of the writing of this standard.
> Otherwise, hobbyists with art projects can have fun with rolling their own Unicode encoding!

> **Aside**
>
> We strictly refer to unicode _codepoints_.
> The unicode standard does not specify the meaning of a "character", noting that it is commonly used ambiguously (sometimes causing bugs).
> The standard mentions glyphs, graphemes, and user-perceptible characters, but while they address how to display textual data, none of them are simple.
> A codepoint is _very_ well defined.
> Since this section is dedicated to _serialization_ rather than _display_, we stick to codepoints.
> Display is left to text editors to figure out (and continue to get wrong).

Line Endings
------------

A compliant system MUST recognize U+000A (Line Feed) as a line separator.
It MAY recognize U+000D (Carriage Return) and SHOULD recognize the sequence U+000D,U+000A (CRLF) as line separators, but compliant systems MAY also reject or ignore them.
These two (CR and CRLF) are "alternate line separators", wheras LF is the "canonnical line separator".
Compliant systems MUST NOT recognize any other codepoint or sequence as a line separator.
It is RECOMMENDED that CCS producers generate the cannonical line separator, and consumers emit warning diagnostics for alternate line separators.
The alternate line separators are DEPRECATED.

> **Rationale**
>
> It's easy to split a byte string on a single byte, and a bit harder to do the same on a byte sequence, and a bit harder still to split one one of several byte sequences.
> Windows is the only holdout against Unix-style line endings.
> This standard does not cater to special commercial interests.
> It merely recognizes that some very rich, very powerful actors
>   (who are, at time of writing, willing to spend billions of dollars a year on unproven and unsafe technologies in order to increase their revenue without increasing the collective wealth of humanity)
>   are not willing to spend some millions over the course of a decade to help make the human work of sofware devepment get easier and more reliable in this obvious, well-understood way.
> This stanard allows compliant systems to work around stubborn clients until such time as we can all move on.

Compliant CCS source MUST contain at most a single type of line separator.
Of course, compliant systems MAY perform error recovery if the source if non-compliant in this aspect.

> **Rationale**
>
> I can't imagine a text editor that would emit mixed line endings without special work from the user.
> I can't imagine why a user would go through such effort just to give a weird paint job to their line separators.
> There's no reason a (de)serializer would transform line endings non-uniformly.
> So, mixed line endings certainly indicate a corruption of the source.

While the end of the source (EOF) does mark the end of a line (EOL) just like line separators do, we have not addressed it above.
Compliant CCS source SHOULD end with a line separator if there is a line separator elsewhere in the source.
TODO yeah, but if it's one line, why not use CIL?

> **Rationale**
>
> It could be handy if the byte-wise concatenation of two sources would also be a line-wise concatenation.
> Joining the last line of one source with the first line of another would likely affect the resulting CST, and if not it would likely affect its interpretation downstream.
> The EOL-at-EOF restriction should not be onerous in any text editor that considers itself mature.

Normalization
-------------

TODO: I'm picking valid symbols&c to not be affected by unicode normalization.
Normalization in comments doesn't matter.
If normalization in strings does matter, that should be processed downstream (in the language, or its text libraries).
