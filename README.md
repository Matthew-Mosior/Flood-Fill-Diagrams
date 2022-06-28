# Flood-Fill-Diagrams:  A Haskell scanline stack flood-fill algorithm implementation

## Introduction

**Flood-Fill-Diagrams (FFD)** is a software tool for generating 2D grid images that visualizes an functional implementation of the scanline stack flood-fill algorithm.

## Purpose

Flood-fill algorithms are used heavily in graphics programming and painting programs.  Haskell is a wonderful language, but not often used to solve these kinds of problems.

Implementing an efficient scanline stack based flood-fill algorithm would be an interesting way to apply Haskell to this problem domain, and see what kind of performance is achievable using a purely functional language.

## Theory and Implementation

**FFD** seeks to maximize performance of the scanline stack flood fill algorithm employed by utilizing ST Arrays (https://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array-ST.html).

ST Arrays are mutable boxed and unboxed arrays in the ST monad.  The ST monad is a pure interface into mutation by providing support for strict state threads.

Haskell IRC user TuringTest provides a great explanation as to how this works internally (https://wiki.haskell.org/Monad/ST):

"ST lets you implement algorithms that are much more efficient with mutable memory used internally. But the whole "thread" of computation cannot exchange mutable state with the outside world, it can only exchange immutable state."

Generating and printing the final 2D grid visualization into a Scalable Vector Graphics (SVG) file happens via functionality provided by the powerful and easy-to-use Diagrams library (https://hackage.haskell.org/package/diagrams).

### Algorithm

1) **FFD** first reads in and parsed/sanitizes the configuration YAML file which specifies the following:
   - A filepath the output svg file. 
   - The number of columns in the output 2D grid.
   - The number of rows in the output 2D grid.
2) A 2D grid is randomly generated using the number of columns and number of rows specified in the configuration YAML.
   - **1** represents an un-filled square.
   - **2** represents a filled squaure.
3) The initial x-y coordinate is chosen, this coordinate will always be un-filled.
4) The scanline stack flood-fill algorithm is run on the 2D grid.
5) The ST Array is converted into a list for easier use going forward.
6) The final list holding the coordinates of the scanline stack filled grid is amalgamated with the original un-filled randomly generated 2D grid.
7) The SVG file is generated from the amalgamated 2D grid via the diagrams library.
