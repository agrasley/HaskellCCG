# HaskellCCG
A CCG parser library written in Haskell with a CYK parsing algorithm.

## Usage
I hope to make the interface a bit more intuitive in the future, but for the time being, this is a rough guide to using the library.

### Lexicons

Use the createLexicon function imported from Lex to create a new lexicon. Lexicons are maps from string keys to list of categories as values.

Example:

    let l = createLexicon [
        ("I", [CmplxLeaf NP]),
        ("am", [CmplxTree (CmplxTree (CmplxLeaf S) Back (CmplxLeaf NP)) Forw (CmplxLeaf NP)]),
        ("a",[CmplxTree (CmplxLeaf NP) Forw (CmplxLeaf N)]),
        ("parser",[CmplxLeaf N])
    ]

### Parsing

Once you have a lexicon, simply call the parse function imported from Parser on your lexicon and a list of string input tokens:

    parse l ["I", "am", "a", "parser"]

The parser will return an empty list if it fails to find a valid parse or it will return the list of valid parse trees.

## Acknowledgements

The CYK parsing algorithm used here is an adaptation of the algorithm developed by [Peter Ljunglöf](https://github.com/heatherleaf).
See his [original repo](https://github.com/heatherleaf/haskell-functional-parsing) for the more generalized algorithm.
Those further interested in this algorithm or other Haskell parsing algorithms are encouraged to consult his excellent [thesis](http://www.cse.chalmers.se/~peb/pubs/Ljunglof2002a%20-%20Pure%20Functional%20Parsing%20--%20an%20advanced.pdf) on the subject.
