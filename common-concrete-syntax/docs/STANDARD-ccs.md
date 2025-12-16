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

```
CST ::= Atom
     |  Enclose Encloser CST
     |  Pair CST CST // key, value
     |  List Separator CST*
     |  Template Text (CST Text)+

Atom ::= Symbol Text // but the text has requirements: matches /[:id:]+|:{2,3}|.{2,3}/ - /[+-]?\d.*/
      |  IntegerLiteral ℤ
      |  FloatingLiteral ℤ ℤ // a * 2^b
      |  DecimalFloatingLiteral ℤ ℤ // a * 10^b
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

```

Indent :: (Nat X (Positive / +/ \cup* '\t') // indentation is a multiple of one of a positive number of spaces or else a tab
inc(n*ty) = ((n+1)*ty)
dec((n+1)*ty) = (n*ty)

Start :: (Indent, CST)
CST :: (Indent, CST)
<Start(indent, x)> ::= <CST((0, 0*indent), x)>
<CST(_, Symbol(x))> ::= <Symbol(x)>


////// Atoms //////

// Symbols //
Symbol :: Str
<Symbol(\0)> ::= /[:id:]+/ - /[+-]?\d.*/
<Symbol(\0)> ::= /:{2,3}/
                 \ <before-sym> _ <after-sym>
<Symbol(\0)> ::= /.{1,3}/
                 \ <before-sym> _ <after-sym>

TODO ints, floats, decimal floats, strings, multilines

// help for atoms //
<before-sym> === <Ws> | <Dot> | <Colon> | /^/ | <Indent(_)>
<after-sym> === <Ws> | <Dot> | <Colon> | /$/ | <Enclose(_)>

////// Punctuation //////

<Enclose(indent, Round, inner)> ::= /(/ <CST(indent, inner)> /)/
<Enclose(indent, Square, inner)> ::= /[/ <CST(indent, inner)> /]/
<Enclose(indent, Curly, inner)> ::= /{/ <CST(indent, inner)> /}/
<Enclose(indent, Indent, inner)> ::= /{/ <CST(inc(indent), inner)> /}/
```

Grammar
=======

FIXME: the problem with the grammar so far is that it's prety loosy-goosy on what any of these symbols mean, and that's because I'm not really sure what tools/power I need to define it.
1. at the core, rules take the form $A {->} B / \alpha \underscore \beta$ for $A$ a non-terminal, $B, \alpha, \beta$ as a strings of terminals and non-terminals, with $\alpha, \beta$ serving as the context-sensitivity
1. In addition to the context-sensitivity, we'll also need attribute grammar things; I guess that each non-terminal has an arity (a set of math objects it can encode), and we're gonna write it inside the angle brackets like `<Flo i, z> ::= <int i> <mantissa z>` or smth
1. The attribute grammar is really specifying a map between an abstract representation of syntax trees and their concrete encoding: the attributes can be thought of as generating the set of strings that would parse into those attrs
1. e/abnf-like syntax in the rules (we already have concat, but alternation, grouping, and repetition are also safe)
1. regex syntax for convenient terminal specs, and also regex complement, intersection, and (but reall only) differece
1. multiple contexts for a rule indicate that it triggers on any of those contexts (I don't trust intersection here)

To specify the grammar, we use a few stages of definition.
First, we identify "tokens", which simply group related characters together, mostly with regex.
Second, we use a mostly context-sensitive (technically non-contracting) grammar to group these raw tokens into "lexemes".
Finally, we use a context-free grammar to organize lexemes into a "concrete syntax tree" or CST.

Throughout these definitions, we distinguish productions from synonyms.
Productions create nodes in a syntax tree, written like `LHS ::= RHS`.
Synonyms are a mere convenience for the specification, written like `LHS === RHS`.
Wherever the LHS of a synonym appears elsewhere in the grammar, it may be replaced with the RHS.


The first groups individual codepoints into low-level tokens using regular expressions.
These low-level tokens are labeled with lower-kebab-case inside angle brackets.
Low-level tokens serve as something like terminal symbols for a context-sensitive grammar.
The conceptual non-terminals of this context-sensitive grammar are written with Title-Kebab-Case, again inside angle brackets.

Linewise Grammar
----------------

### Overview

The raw tokens are comprised by only a small set of concepts.
- Numbers, in both integer and floating point varieties.
- Symbols, which includes the familiar identifiers, keywords, and operators of popular programming languages.
  One new feature here is support for mixing characters traditionally thought of as part of either an identifier or an operator.
  FIXME this is an example: Thus, symbols such as `` or `eqRef?` are valid symbols
  FIXME this is rationale: The CSS notion of symbol is liberal, but a language implementor may choose to reject unfamiliar symbols downstream.
- Strings and string templates.
  Double-quoted strings use familiar C-like escape sequences, and backticks indicate a string splice.
  Single-quoted strings are like SQL strings: the only escape sequence is two single-quotes to indicate a single-quote.
  FIXME this is rationale: There are no character literals, as I'd like to encourage _not_ thinking of text in terms of individual codepoints (or bytes!).
- Punctuation, including brackets, and also common delimiters such as comma and semicolon.
  In particular, we include period and colon, which we expect eill be used for field access, name qualification, key-value pairing, and the ilk.
- Whitespace, which may be tabs at the start of the line for indentation, but must otherwise be the space character U+0020.
- Comments, here considered alongside whitespace, are indicated with a hash character and extend to the end of the line.
  Multi-line comments, nesting or otherwise, are not supported, because they are either more error-prone or harder to parse and discard.

TODO: examples and rationales for all of these raw token types

> **Aside**
>
> The design of this stage of the grammar was essential to get right.
> There are many useful programs that can fruitfully process CCS line-by-line, maintaining only limited state between lines.
> One of particular note is syntax highlighting: the Language Server Protocol is widespread nowadays,
>   but it is still simpler and easier to set up a text editor's native highlighter if the syntax allows for it.
> Furthermore, programmer may wish to write simple scripts that analyze a code base in some small way.
> I would love to have a tool that scans Haskell codebases for any variables containing `/[uU]nsafe/`, but Haskell is not especially easy to parse correctly.
> So for now I instead rely on `grep` and work around the potential inaccuracies, but an accurate script could be integrated into CI systems, without needing to extend a linter.

### Preliminaries

Regular expressions are written inside slashes, using syntax recognized by PCRE.
If the closing slash of a regex literal if followed by an `i`, that indicates case-insensitivity (for ASCII codepoints only).
Additionally, the intersection and complement operators are closed over regular expressions.
Complement is written with an exclamation mark before the regex (outside the slashes, if present).
Intersection is written with an ampersand between two regular expressions.
With complement and intersection, it is easy to also define difference between two regexes, written with a minus sign.
Literal strings are written enclosed in single-ticks.

Regexes operate line-wise, so the regexes /^/ and /$/ match the beginning and end of a _line_, not the whole file.
We also use a few custom character classes, such as `[:id:]`.

An additional type of low-level token produced at this stage is <indent>.
Its definition is a parameter of the grammar, and must match wither the literal `'\t'`, or else `[ ]{n}` for some n >= 1.

The LHS of these rules consist of a single non-terminal, written with a lower-kebab-case name inside angle brackets.
The RHS consists of a sequence of regular expressions which must match within the line.
Note that the "non-terminals" may appear on the RHS, and may take on repetition operators.
Some of these productions have a RHS with an Upper-Kebab-Name; they make more sense to define here, despite strictly belonging to the next stage of parsing, which would otherwise just translate a lower-kebab into an Upper-Kabab.

### Symbols
### Numbers

Numbers may seem simple on the face of it --- just some digits --- but there is a variety of representations in popular use which complicate their description.
In addition, while CCS normally applies very little semantics or interpretation, it does specify that some representations numbers are equivalent.

CCS Supports numbers:
- positive or negative, marked with an optional plus or minus sign (default positive)
- in four different bases --- binary, octal, decimal, and hexadecimal --- indicated by a radix mark
- integral or floating point: integers have no decimal point, whereas floating point numbers have a decimal point with digits on both sides
- an optional exponent on floating point numbers (positive or negative, but always in decimal notation)
- with digits separated into groups by underscores

TODO: bitwise numbers vs decimal; binary is good for bitfields, hexadecimal is good for byte-oriented data with exactly two digits per byte, octal is mostly historical when multiples of 3-bit groupings were common, but might still find a little use in, say, unix permissions (they would be deprecated or removed if they weren't so easy to support)


### Strings
### Whitespace
### Punctuation

### FIXME break this up

```
////// whitespace //////

// recall from the text that <indent> is also a token which may appear at the start of a line

<ws> ::= /[ ]+/
      |  /#.*$/

////// punctuation //////

<Open-Round> ::= '('
<Close-Round> ::= ')'
<Open-Square> ::= '['
<Close-Square> ::= ']'
<Open-Curly> ::= '{'
<Close-Curly> ::= '}'

<semi> ::= ';'
<comma> ::= ','
<colon> ::= ':'
<dot> ::= '.'

<bs> ::= '\'

////// symbols //////

[:id:] === // TODO
<symbol> ::= /[:id:]+/ - /[+-]?\d/

////// numbers //////

<num> ::= <int> | <flo>

<int> ::= /0b/i <bin-digits>
       |  /0o/i <oct-digits>
       |  /0x/i <hex-digits>
       |        <dec-digits> - /_.*/

<flo> ::= /0b/i <bin-digits>    '.' <bin-digits> <bin-exp>?
       |  /0o/i <oct-digits>    '.' <oct-digits> <bin-exp>?
       |  /0x/i <hex-digits>    '.' <hex-digits> <bin-exp>?
       |        (<dec-digits> - /_.*/) '.' <dec-digits> <dec-exp>?

// support //

<sign> ::= /[+-]/
<bin-digits> ::= /[0-1]+(_+[0-1]+)*/
<oct-digits> ::= /[0-7]+(_+[0-7]+)*/
<dec-digits> ::= /[0-9]+(_+[0-9]+)*/
<hex-digits> ::= /[0-9a-f]+(_+[0-9a-f]+)*/i

<bin-exp> ::= /p[+-]?/i <dec-digits>
<dec-exp> ::= /e[+-]?/i <dec-digits>

////// strings //////

// sql strings //

<sql-str> ::= <sq> (<sql-part> | <sql-escape>)* <sq>

<sql-part> ::= /[^']+/ // FIXME and illegal string characters
<sql-escape> ::= /''/

// standard strings //

<Str>           ::= <dq> (<str-part> | <str-escape>)+ <dq>

<Open-Tempate>  ::= <dq> (<str-part> | <str-escape>)+ <bt>
<Mid-Tempate>   ::= <bt> (<str-part> | <str-escape>)+ <bt>
<Close-Tempate> ::= <bt> (<str-part> | <str-escape>)+ <dq>

<str-part> ::= /[^"`\\]+/ // FIXME and illegal string characters
<str-escape> ::= // TODO

// support //

<sq> ::= /'/
<dq> ::= /"/
<bt> ::= /`/

////// multi-line literals //////

// FIXME here, we have to synthesize the delimiter in open, and analyze it in the close, so it's closer to an attribute grammar
// we represent this with capturing groups on the LHS, and parentheses on the LHS (inside the angle brackets for analyze, outside for synthesize)

<open-multiline>(1) ::= /("{3,}[a-zA-Z}*)/
<close-multiline(1)> ::= /\1/

<multiline-content> ::= /.*/ // FIXME except _very_ illegal codepoints
```

NOTE the digits regexes require at least one digit in amongst the underscores. Also, decimal numbers cannot start with an underscore.
A more restrictive regex could be given, requiring non-empty digit strings separated by underscores, and I might just.

> **Rationale**
>
> Why are numerical literals so restrictive?
> - Floating point numbers must have digits on both side of the decimal point?
>   Because the period is used both as a decimal point and as general punctuation (for eg field access), I thought it better that decimal points must be sandwiched between digits.
>   A CCS user may wish `foo.1` to access the first field of the tuple-like value `foo`; if number-like objects can have such elements, is `1.1` a single float, or a field access?
>   Perhaps `.1` is an illegal field, but other fields, such as `.e1` are allowed; then is `1.e1` represent 10.0, or the `e1` field of the number 1?
>   TODO I might not allow prefix-dot in this standard, but a language may wish `a .cmp(b)` to be an infix call, equivalent to `cmp(a, b)`;
>     then is `a .1` a symbol followed by a floating point literal, or equivalent to the call `1(a)`?
>   Some of these answers may seem obvious based on your subjective experience or ability to imagine new language features.
>   This standard strives for the answers to be obvious regardless of past programming experiences --- the ergonomic principle.
> - No power on integers?
>   Because even a small number of digits in the exponent might cause simple big integer libraries to exhaust memory.
>   I suggest using floats for things that require an exponent
> - Exponents can only be written in decimal?
>   Non-decimal numbers are generally used to determine bit patterns, which is not as useful for floating point numbers.
>   An exponent should just tell us how many places to shift the decimal point.
>   If there is a practical use case demonstrated, I may consider an extension.
>
> On the otherhand, these literals are also quite permissive.
> The support four different radices uniformly, with support for integral, fractional, and fractional+exponent forms.
> Additionally, digit separators are allowed, which should make it easier to understand `1_000_000_000` compared to `1e9` (not to mention `1000000000`).

Scratch Work - DELETE
---------------------

```
<Comment> ::= '#' /.*$/
<Eol> ::= <Lws>? <Comment>? /$/

!<ws> <Ws> !<ws> ::= !<ws> <ws> !<ws>
```


FIXME: this grammar needs to be parameterized by an indentation token, which must be of the form `/[ ]{n}|\t/` for some n >= 1.

TODO: I'm kinda separating raw tokens/character sequences with lowercase names, and committed tokens as Titlecase names

Tokens
```

////// Punctuation //////

// NOTE these are _equal_ signs, so they are _synonyms_, not prductions
<open> = <open-round> | <open-square> | <open-curly>
<close> = <close-round> | <close-square> | <close-curly>

    <non-dot>  <Dot>  <non-dot>
::= <non-dot>  <dot>  <non-dot>

    <non-colon>  <Colon>  (/$/ | <ws>)
::= <non-colon>  <colon>  (/$/ | <ws>)
    /[:id::quote:]/  <Qual>   /[:id::quote:]/ // FIXME uses :id: and :quote:, and might need more
::= /[:id::quote:]/  <colon>  /[:id::quote:]/

////// Symbols //////

<symbol> ::= /[:id:]+/ - /[+-]?[0-9].*/ // FIXME I need to be clear about regex subtraction

    <Symbol> /[^0-9]/
::= <symbol> /[^0-9:id:]/

    (/^/ | <ws> | <open>)  <Symbol>                  (/$/ | <ws> | <close>)
::= (/^/ | <ws> | <open>)  <dot> <dot> <dot>?        (/$/ | <ws> | <close>)
 |  (/^/ | <ws> | <open>)  <colon> <colon> <colon>?  (/$/ | <ws> | <close>)

```



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
