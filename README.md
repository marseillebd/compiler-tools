I want to make a programming language: a full one.
There are a lot of moving peices to that, and this is where I'm putting my experiments and accumulating the successfult ones.

First off, I'm building this in Haskell, because it's great for making languages, and because I'm very familiar with the relevant packages.

# Notes to Self

Minimal next step: collect modules from files.
- I need a tokenizer/parser
- after any generic parsing, I'm only looking for `module <name>`, with no contents.
- generate a file per-module with its contents (empty for now)
- Use the unicode replacement character when decoding. That'll keep tokens separate when they're separated by an illegal encoding.
I've put some stuff off:
- fancier tokens, like floating point literals
- `import` decls, and the rest of the decls ofc, but import decls would let me generate a module dependency graph also
- module parameters, module instantiation, signatures

It's all in preparation for a scope checker/renamer.

# Ideas

## The Vision

```
 source locations            source file                                    ─────▶ast
 interval overlaps                │                         nanopass               │
                                  │                                                │
┌─────────────────────────────────│──────────┐                          ┌──────────│─────────┐
│CCS:                             │          │                          │          │         │
│                              Decode        │                          │     Scope Check    │
│                                 │          │                          │          │         │
│                                 │          │                          │          │         │
│                             Normalize      │                          │     Type Check     │
│                                 │          │                          │          │         │
│                                 │          │                          │          │         │
│                               Lexer        │                          │   Quantity Check   │
│                                 │          │                          │          │         │
│                                 │          │                          │          │         │
│                          Lexeme Cleanup    │                          │       Desugar      │
│                                 │          │                          │          │         │
│                                 │          │                          └──────────│─────────┘
│                              Parser        │                                     │
│                                 │          │                                     │
│  syntax spec mixfixes           │          │                               validated abt
│  recognizer arrow       Mixfix Rewriting   │                                     │
│                                 │          │                                     │
└─────────────────────────────────│──────────┘                             ┌───────└────────┐
                                  │                                        │                │
                                 cst                                       │                │
                                  │                                   ┌─────────┐       ┌───────┐
                                  │                                   │Interpret│       │Compile│
                            ┌────────────┐                            └─────────┘       └───────┘
                            │ Recognizer │
                            └────────────┘
                                  │
                                  │
                                 ast─────▶
```

Edit/view: https://cascii.app/39b54

## Unicode Syntax

A table that holds pairs of ascii and unicode strings that should be equivalent identifiers.
We can
- generate documentation that uses either encoding,
- suggest/execute replacements in the lsp or an auto formatter,
- and maybe even use the ascii form when name mangling (which would require every unicode id to have an ascii counterpart).
I would think it'd be defined with the mixfixes, but if we're name mangling, then it needs to apply to qualified names differently:
  pkg A exports a unicode for `=>`, and when pkg B uses that id, it needs to use A's xlation.


## Covering Lexer Free Monad (later, if at all)

What about a free monad for the raw lexer?
And then typed tagless representation so I can both evaluate the lexer against input, but also generate syntax files for editors?

## Incremental Compilation (delegate)

I'm just going to use `task` or something.
There's no reason for me to re-invent this, I think.
Perhaps `nix` would also give the appropriate functinoality, but it's not obvious to me.

## Compilation Planning (later)

While this language is small, I have no qualms about manually coordinating compilation steps.
That includes dependency solving/gathering/compiling/environment management.

I beleive that making the plan will be independent of the compiler itself, or at least it can be if I give access to separate compilation steps.
I'd have to give access to those (fine-grained?) steps anyway in order to manually coordinate them.

## Generic Tree Representation (later)

The abstract syntax is _way_ cleaner than JSON/Yaml/XML/&c, which is an advantage when writing queries, as I don't have to handle as many cases.
However, what would I be using it for?
(De)serializing intermediate representations, probably, but I could do that just as easily with Haskell's derived Read/Show instances.
Perhaps I'd get a prettier layout, but I don't want to be reading these much, and `pretty-simple` has done well enough so far.
Leave it for when I am trying to separate from Haskell-specific tooling.


```haskell
type Id = Text/ByteString
data Tree = Tree
  { name :: Id -- ^ used either to hold the leaf id, or the id of the object tag
  , attrs :: Map Id [Tree] -- ^ as with html query params, multiple entries for the same attribute name is allowed
  }
name :: Tree -> Id
one :: Tree -> Id -> Maybe Tree
some :: Tree -> Id -> Maybe (NonEmpty Tree)
many :: Tree -> Id -> [Tree]
```

```ebnf
obj ::= id
     |  id '{' entry '}'
entry ::= ( id '=' )? id
id ::= [a-zA-Z0-9,.:()<>_-]+
    # ^ alphanumerics, separators `_-`, punctuation `,.:()<>`
    # ^ might add more later
    |  '\"' strPart* '\"'
strPart ::= # unescaped chars, maybe only ascii printing
         |  # c escapes
         |  # unicode escapes \x{10FFFF}
```

## Symbol Tables (later)

Just use `String` (or its synonyms).
Ofc symbol tables are nice and fast, but not necessary for the compiler to function.
The `symbol` package has the basics, but I'd like something more polymorphic:

```haskell
newtype Symbol s (i :: TYPE IntRepr) a = Symbol !i
data Symtab s (i :: TYPE IntRepr) a
withSymtab :: (forall s. Symtab s i a -> SymbolMonad s i a b) -> b
symtab :: SymbolMonad s i a (Symtab s i a)
intern :: Symtab s i a -> a -> SymbolMonad s i a i
extern :: Symtab s i a -> i -> SymbolMonad s i a a
```
