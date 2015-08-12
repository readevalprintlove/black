# Black

This is the source code for [Kenichi Asai](http://pllab.is.ocha.ac.jp/~asai/)'s Black programming language as described in the paper *Duplication and Partial Evaluation - For a Better Understanding of Reflective Languages*. Please see [the original README](/README) from the [black.tar.gz](http://pllab.is.ocha.ac.jp/~asai/papers/black.tar.gz) code archive accompanying the paper.

# Getting Started

```scheme
(load "env.scm")
(load "stream.scm")
(define scheme-apply apply)
(load "black.scm")
(black)
```

See the [examples](examples/transcript.scm).
Play in your browser at [io.livecode.ch](http://io.livecode.ch/learn/readevalprintlove/black)!
