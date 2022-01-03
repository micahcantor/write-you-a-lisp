# writing-a-lisp

This is a work-in-progress repository for a small lisp written in Haskell. It is written for clarity and concision, to show the expressive power of writing interpreters in Haskell using monad transformers.

The language is mostly a core subset of Scheme, with a few differences:

- As in Clojure, there is just one equality test, `=`.
- Probably others when the language is more complete.

The implementation currently using [relude](https://hackage.haskell.org/package/relude) (with the NoImplicitPrelude extension), but that might change if I decided to write about the code.