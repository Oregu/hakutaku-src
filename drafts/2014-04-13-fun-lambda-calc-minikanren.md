---
title: Fun with Lambda calculus Interpreters in miniKanren
author: Oleg Prophet
---

_Work in progress_

A year back I came in front of Dan Friedman and Will Byrd presentation called [Fun with Relational Interpreters in miniKanren](http://2013.flatmap.no/danwill.html). Which stroke me momentally. They presented a technique which allows you to generate infinite amount of quines. And not just that, but twines, thrines and other automatically generated programs with properties you want them to have. Why? How is that? What type of magic is this? And you don't have to use special language for that. It's right in your host language: in Racket as miniKanren or in Clojure as core.logic.

Quines generator
----------------
Being a clojure programmer I wanted to undertand how all this works. So I started translating original [quines generator](https://github.com/webyrd/quines) to Clojure. You can find result in my [clojure quines](https://github.com/Oregu/clj-quines) repo. That was pretty painful. I didn't new anything of Racket, especially commas for symbol substitution and `define-syntax` stuff.

So, after translating quines generator I wanted to proceed. I wanted to generate things and next interesting puzzle I found for miniKanren is lambda calculus/combinatory logic terms generation. At the time I was reading wonderful [Introduction to Lambda Calculus](http://www.cse.chalmers.se/research/group/logic/TypesSS05/Extra/geuvers.pdf). And there was an exercise: write a combinator such that `Fx=F`. Which is an interesting task on it's own. But with miniKanren… this can be really adventurous.

Lambda culculus interpreter
---------------------------
So I started writing lambda culculus interpreter which is able to run backwards producing terms. First attempt was to simply adapt [Will](https://github.com/webyrd)'s interpreter to work with Lambda-Calculus. If you only new how I was wrong… This attempt deserves it's [branch](https://github.com/Oregu/untyped/tree/naive) on Github. That's the hard way I knew about capture-avoiding substitutions. And only after that I realized what [alphaKanren](https://github.com/webyrd/alphaKanren) is really for.

Church numerals
---------------
Let's see what we can do with the interpreter. First things you get to know in λ-calculus are Church numerals. Let's define some numerals and do fun stuff with them:
```clojure
(use 'untyped.core)

(defn ch-succ [n f x]
  (lam n (lam f (lam x (app f (app (app n f) x))))))

(defn ch3 [f x]
  (lam f (lam x (app f (app f (app f x))))))

(first (run* [q] (nom/fresh [n f x]
  (eval-expo
    (app (ch-succ n f x) (ch3 f x))
    q))))              ;; What is the result applying successor to three?

=> (fn [a_0] (fn [a_1] (a_0 (a_0 (a_0 (a_0 a_1))))))
```
In this example we defined Church successor as `λnfx.f(nfx)` and number three `λfx.f(f(fx))`. And evaluating application gives us a term which can be translated back as `λfx.f(f(f(fx)))` which is a Church four indeed. Here `a_0` and `a_1` are distinct noms.

Running backwards
-----------------
Ok, that was straitforward, let's try something trickier. It's easy to obtain successor for Church numeral, but not predecessor. There are couple of predecessor functions defined in terms of predicates or even pairs, involving complex reasoning… But we won't go that path, let's decrease using increaser:
```clojure
(first (run* [q] (nom/fresh [n f x]
  (eval-expo
    (app (ch-succ n f x) q) ;; To what λ-term should I apply successor
    (ch3 f x)))))           ;; to obtain number three?

=> (fn [a_0] (fn [a_1] (a_0 (a_0 a_1))))
```
Doesn't that answer remind you of something? Maybe it looks like term `λfx.f(fx)`? This is a Church two. Exactly!

You can find other experiments with Church numerals in [this file](https://github.com/Oregu/untyped/blob/master/src/untyped/church.clj) on Github. For example we can ask miniKanren to produce successor term and it will do so in 41 seconds! Even though I can't generate summator function without heavy hinting! That's a problem I'm working on.

Combinatory logic
-----------------
Of course we can do Combinatory logic. Let's try. SKI combinator calculus is indeed Turing complete, but I heard oneday that even S and K are enough to fully represent λ-calculus. Let's try:
```clojure
(run 1 [q] (nom/fresh [x y xx yy a b c]
  (eval-expo
    (app (app (app (S a b c) (K x y)) (K xx yy)) q) ;; SKKq ?= q
    q)))

=> (_0)
```
We asked it whether applying `SKKq` will result in `q`. And indeed it answered positive. Which means that `S` and `K` combinators are enough.

Now back to fun again: how does quine looks like in λ-calculus?
```clojure
(run 2 [q] (eval-expo q q))

=> ((fn [a_0] _1)
    (((fn [a_0] a_0) (fn [a_1] (a_1 a_1)))
     ((fn [a_0] a_0) (fn [a_1] (a_1 a_1)))))
```
We asked interpreter to produce expression which on evaluation will produce itself. We asked it to produce two answers. First one is a lambda abstraction which is a normal form and indeed will be evaluated to itself (I wonder whether it is a correct behavior). And the second one, what is this? Big omega is a second answer: `Ω=(λx.xx)(λx.xx)`. But I should admit, that it's not in normal form. It looks more like `(Iλx.xx)(Iλx.xx)`, but after normalizing terms in brackets we have our `Ω` which is still cool.

Recursion
---------
[Generating Y.]

Where it all started: a greedy function
---------------------------------------
[K infinity.]

Resources
------------
- [Introduction to Lambda calculus](http://www.cse.chalmers.se/research/group/logic/TypesSS05/Extra/geuvers.pdf).
- [Relational Programming in miniKanren: Techniques, Applications, and Implementations](http://gradworks.umi.com/3380156.pdf).
- [Fun with Relational Interpreters in miniKanren](http://2013.flatmap.no/danwill.html).