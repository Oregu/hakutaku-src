---
title: Fun with Lambda calculus Interpreters in miniKanren
author: Oleg Prophet
started: 2014-04-13
finished: 2014-05-05
---

A year back I came in front of Dan Friedman and Will Byrd presentation called [Fun with Relational Interpreters in miniKanren](http://2013.flatmap.no/danwill.html). Which stroke me momentally. They presented a technique which allows you to generate infinite amount of quines. And not just that, but twines, thrines and other automatically generated programs with properties you want them to have. Why? How is that? What type of magic is this? And you don't have to use special language for that. It's right in your host language: in Racket as miniKanren or in Clojure as core.logic.

## Quines generator

Being a Clojure programmer I started translating original [quines generator](https://github.com/webyrd/quines) to Clojure. You can find result in my [clojure quines](https://github.com/Oregu/clj-quines) repo. That was pretty painful. I didn't new anything of Racket, especially commas for symbol substitution and `define-syntax` stuff.

So, after translating quines generator I wanted to proceed. I wanted to generate things and next interesting puzzle I found for miniKanren is lambda calculus/combinatory logic terms generation. At the time I was reading wonderful [Introduction to Lambda Calculus](http://www.cse.chalmers.se/research/group/logic/TypesSS05/Extra/geuvers.pdf) and there was an exercise: write a combinator such that `Ex=E`. Which is an interesting task on it's own. But with miniKanren… this can be really adventurous.

## Lambda calculus interpreter

So I started writing λ-calculus interpreter which is able to run backwards producing terms. First attempt was to simply adapt [Will](https://github.com/webyrd)'s interpreter to work with λ-calculus. If you only new how I was wrong… This attempt deserves it's [branch](https://github.com/Oregu/untyped/tree/naive) on Github. That's the hard way I knew about capture-avoiding substitutions. And only after that I realized what [alphaKanren](https://github.com/webyrd/alphaKanren) is really for.

I enhanced Will's relation interpreter with noms and added Will's (slightly modified) capture-avoiding `substo` function (which in turn adapted from [αProlog](http://homepages.inf.ed.ac.uk/jcheney/programs/aprolog/)). You can find `eval-expo` function under [untyped.core](https://github.com/Oregu/untyped/blob/master/src/untyped/core.clj) namespace.

## Church numerals

Now let's see what we can do with the interpreter. First things you get to know in λ-calculus are Church numerals. Let's define some numerals and do fun stuff with them:
```clojure
(use 'untyped.core)

(defn ch-succ [n f x]
  (lam n (lam f (lam x (app f (app (app n f) x))))))

(defn ch3 [f x]
  (lam f (lam x (app f (app f (app f x))))))

(first (run* [q] (nom/fresh [n f x]
  ;; What is the result applying successor to three?
  (eval-expo
    (app (ch-succ n f x) (ch3 f x))
    q))))

=> (fn [a_0] (fn [a_1] (a_0 (a_0 (a_0 (a_0 a_1))))))
```
In this example we defined Church successor as `λnfx.f(nfx)` and number three `λfx.f(f(fx))`. Evaluating application gives us a term which can be translated back as `λfx.f(f(f(fx)))` which is a Church four indeed. Here `a_0` and `a_1` are distinct noms, which can be replaced with substituted with any names as long as they are different, e.g. `f` and `x` correspondingly.

## Running backwards

Ok, that was straitforward, let's try something trickier. It's easy to obtain successor for Church numeral, but not predecessor. There are couple of predecessor functions defined in terms of predicates or even pairs, involving complex reasoning… But we won't go that path, let's decrease using increaser:
```clojure
(first (run* [q] (nom/fresh [n f x]
  (eval-expo
    (app (ch-succ n f x) q) ;; To what λ-term should I apply successor
    (ch3 f x)))))           ;; to obtain number three?

=> (fn [a_0] (fn [a_1] (a_0 (a_0 a_1))))
```
Doesn't that answer remind you of something? Maybe it looks like term `λfx.f(fx)`? This is a Church two. Exactly!

You can find other experiments with Church numerals in [this file](https://github.com/Oregu/untyped/blob/master/src/untyped/church.clj) on Github. For example we can ask miniKanren to produce successor function and it will do so in 41 seconds! Even though I can't generate summator function without heavy hinting! That's a problem I'm working on.

## Combinatory logic

Of course we can do Combinatory logic. Let's try. SKI combinator calculus is indeed Turing-complete, but I heard oneday that even S and K are enough to fully code λ-calculus. Let's try:
```clojure
(run 1 [q] (nom/fresh [x y xx yy a b c]
  (eval-expo
    (app (app (app (S a b c) (K x y)) (K xx yy)) q)
    q)))      ;; SKKq ?= q

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
We asked interpreter to produce expression which on evaluation will produce itself. We asked it to produce two answers. First one is a lambda abstraction which is a normal form and indeed will be evaluated to itself (I wonder whether it is a correct behavior). And the second one, what is this? Big omega is a second answer: `Ω=(λx.xx)(λx.xx)`. But I should admit, that it's not in normal form. It looks more like `I(Iλx.xx)(Iλx.xx)`, but after normalizing terms in brackets we have our `Ω` which is still cool.

## The point of it all: An Eater Function

My initial idea was to produce such function, that will eat it's arguments one by one and never satisfies. In “Introduction to Lambda Calculus” it's called eater function. Another names I found are K~∞~ or K~\*~ combinators and even [A Hopelessly Egocentric Bird](http://en.wikipedia.org/wiki/To_Mock_a_Mockingbird) `B`. (I tried to find where I saw K~∞~ and K~\*~ names but couldn't, so if you now where are they came from, [let me know](mailto:thehakutaku@gmail.com).)

An eater function is a term defined by following expression: `Ex=E`. It's easy definable through fixed point combinator as `K` combinator applied to `Y`: `YK ≡ K(YK) ≡ (λxk.x)(YK) ≡ λk.(YK)`

Now let's ask miniKanren to produce an eater with a simple equation `eval (E x) = E`:
```clojure
(run 1 [E] (nom/fresh [x]
  (eval-expo (app E x) E)))

=> ((((fn [a_0] a_0) (fn [a_1] (fn [a_2] a_1)))
     ((fn [a_3] (a_3 _4)) (_5 (fn [a_6] (fn [a_7] a_6)))))
  :-
  (!= (((fn [a_3] (a_3 _4)) (_5 (fn [a_6] (fn [a_7] a_6)))) a_8))
  (!= (_8 a_9))
  not-fn?
  nom?
  (!= ((fn [a_0] a_0) a_8))
  a_2#_4
  a_2#_5
  (!= ((fn [a_0] a_0) fn))
  a_2#clojure.lang.LazySeq@c6fe0969
```
It spits a lot of stuff, where function is before `:-` sign. Let's translate: `((λi.i)(λxk.x))((λa.(a _4))(_5 λyz.y))` where `_4` and `_5` are some terms, which I suppose are related to [CLP](http://en.wikipedia.org/wiki/Constraint_logic_programming). It just returns constrainted terms, which can be anything but those after `:-`. Well, it looks like a nonsense.

Now I should dissapoint you, apparently my interpreter is not capable of handling recursively defined terms (and eater function is definitely recursive). But, in twitter discussion with [Will](https://twitter.com/webyrd) I found out that he already has [an interpreter](https://github.com/webyrd/alphaKanren/blob/master/tests.scm#L980) able to produce well known Y-combinator. So, I translated it to Clojure (again) et voilà!
```clojure
(run 1 [E] (fresh [U]
  (nom/fresh [f z]
    (lamo f (app U U) E)
    (nom/hash z E)
    (step-equalo (app E z) E))))

=> (fn [a_0]
    ((fn [a_1] (fn [a_2] (a_1 a_1)))
     (fn [a_1] (fn [a_2] (a_1 a_1)))))
```
This can be translated into following term: `λe.(λab.aa)(λab.aa)` (or compactly as `(λw.ww)(λab.aa)`). Indeed this is a function we was looking for and much shorter then the one defined through `Y` above!

Summing up: the term `λab.aa` is [called](http://www.angelfire.com/tx4/cus/combinator/birds.html) Crossed Konstant Mocker, so if you call out Crossed Konstant Bird to Mocking bird you will get A Hopelessly Egocentric Bird back! Pure joy.

## More recursion

No λ-calculus article is complete without Y combinator. As I already mentioned, I borrowed Y-enabled interpreter from [WIll](https://github.com/webyrd/), so I'll just send you to [his repo](https://github.com/webyrd/alphaKanren/blob/master/tests.scm#L980). His test 68 produces `Y` after 7 minutes. And he, in turn, borrowed that interpreter from [alphaProlog](http://homepages.inf.ed.ac.uk/jcheney/programs/aprolog/).

## Conclusion

Logic programming is hard. However I find writing in miniKanren simpler than Prolog since you do it in the host language you are already familiar with, like Scheme or Clojure or [any other](http://minikanren.org/#implementations). Plus, miniKanren itself is implemented in your host language, so you can get full understanding of inner workings by browsing source (or even better, read [book](http://mitpress.mit.edu/books/reasoned-schemer) and [dissertation](http://gradworks.umi.com/3380156.pdf)).

In the end I get my eater function produced by machine, since I was lazy enough to produce it on paper. And machine was pretty good in finding the answer, thanks to miniKanren and Clojure.

## Resources

- [Fun with Relational Interpreters in miniKanren](http://2013.flatmap.no/danwill.html).
- [Introduction to Lambda calculus](http://www.cse.chalmers.se/research/group/logic/TypesSS05/Extra/geuvers.pdf).
- [Relational Programming in miniKanren: Techniques, Applications, and Implementations](http://gradworks.umi.com/3380156.pdf).
