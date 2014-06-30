μKanren
=======
Haskell implementation of Jason Hemann ([@jasonhemann](https://github.com/jasonhemann)) and  Daniel P. Friedman's ([@dfried00](https://github.com/dfried00)) "microKanren: A Minimal Functional Core for Relational Programming".

“In [this paper](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf), we present μKanren, a _‘featherweight’_ implementation for a pure relational (logic) programming language.”

Original Scheme implementation: https://github.com/jasonhemann/microKanren

In action
---------
```haskell
appendo l s out = mplus
    (do l === LVal empty
        s === out)
    (do h ← fresh
        t ← fresh
        l === LVal (cons h t)
        res ← fresh
        out === LVal (cons h res)
        appendo t s res)
```
See [Example](https://github.com/Oregu/featherweight/blob/master/Example.hs) and [ExampleM](https://github.com/Oregu/featherweight/blob/master/ExampleM.hs) files.

To do
-----
- [x] Implement appendo, custom unify
- [x] Logic as a state monad
- [x] Tests
- [x] Use Map for substitutions
- [ ] Polymorphic substitutions
- [ ] Reification (do not reifies for structs, like lcons)
- [ ] Nominal
- [ ] Benchmarks

Add examples
------------
- [ ] Membero
- [ ] Oleg's numbers
- [ ] Quines generator
