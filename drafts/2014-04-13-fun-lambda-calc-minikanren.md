---
title: Fun with Lambda calculus Interpreters in miniKanren
author: Oleg Prophet
---

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
[Describing successor and summator.]

Running backwards
-----------------
[Subtracting with sucessor and summator.]

Recursion
---------
[Generating Y.]

Where it all started: a greedy function
---------------------------------------
[K infinity.]

Bibliography
------------
- [Introduction to Lambda calculus](http://www.cse.chalmers.se/research/group/logic/TypesSS05/Extra/geuvers.pdf).
- [Relational Programming in miniKanren: Techniques, Applications, and Implementations](http://gradworks.umi.com/3380156.pdf).