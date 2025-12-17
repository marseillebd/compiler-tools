

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

