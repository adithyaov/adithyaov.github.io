---
title: Introduction of Acyclic graphs in Alga
category: alga pinned
---

**Alga** is a library for algebraic construction and manipulation of graphs in Haskell. See
[this Haskell Symposium paper](https://github.com/snowleopard/alga-paper) and the
corresponding [talk](https://www.youtube.com/watch?v=EdQGLewU-8k) for the motivation
behind the library, the underlying theory and implementation details. There is also a
[Haskell eXchange talk](https://skillsmatter.com/skillscasts/10635-algebraic-graphs), 
and a [tutorial](https://nobrakal.github.io/alga-tutorial) by Alexandre Moine.
Please visit the [wiki](https://github.com/snowleopard/alga/wiki) for more information.

# The Module

According to the naming conventions in Alga, the module that contained
the adjacency map for the acyclic graph is named 
[Algebra.Graph.Acyclic.AdjacencyMap][Acyclic.AdjacencyMap].
There is also a module named [Algebra.Graph.Acyclic.AdjacencyMap.Ord][Acyclic.AdjacencyMap.Ord]
which is an extension of [Algebra.Graph.Acyclic.AdjacencyMap][Acyclic.AdjacencyMap].
described later in this blog.

# Motivation behind acyclic graphs

Acyclic graphs are both common and heavily used in dependency
management. Improvements in this area would therefore directly
benefit downstream packages like [build](https://github.com/snowleopard/build),
[plutus](https://github.com/input-output-hk/plutus) or
[aura](https://github.com/aurapm/aura),
as well as a few commercial users of the library.

In particular, the result should be a type-safe abstraction,
that makes it easier to work with algorithms like `scc` or `topSort`
as has been remarked in [some](https://github.com/snowleopard/alga/issues/152)
[issues](https://github.com/snowleopard/alga/issues/154).

# Construction methods

Below are a few ways one could construct an acyclic graph. In the [final draft](Acyclic.AdjacencyMap)
few construction methods have been removed for being unsafe.

## SCC algorithm

One straightforward way to construct an acyclic graph was by using
the SCC algorithm. We all know that a graph of SCC is acyclic and
hence can be used to construct acyclic graphs.

The following is the signature for `scc` in Alga,
```
scc :: Ord a => AdjacencyMap a -> Acyclic.AdjacencyMap (NonEmpty.AdjacencyMap a)
```

## Construction from algebraic graph and a partial order

The basic idea of this kind of construction is that if the edges are
restricted on the basis of ordering of the elements then it is
impossible to construct a back edge and hence it is impossible to
construct an acyclic graph. This order can be visualized as somewhat
similar to the topological sorting itself.

```
import Algebra.Graph (Graph)

type PartialOrder a = a -> a -> Bool

fromGraph :: Ord a => PartialOrder a -> Graph a -> Acyclic.AdjacencyMap a
```

The problem with this is the fact that the construction of the acyclic
graph is dependent on the partial order provided. One could potentially
make cyclic graphs given an improper partial order such as,
`partialOrder _ _ = True`. It is not possible to restrict the user to
use a **strict partial order** and hence is removed from the [final draft][Acyclic.AdjacencyMap].

One could also create an evil `Ord` instance which might lead to the
creation of an inconsistent acyclic graph (or maybe even an
inconsistent graph itself). This, being an extreme case, is ignored.

# The Ord module

[Algebra.Graph.Acyclic.AdjacencyMap.Ord][Acyclic.AdjacencyMap.Ord] is
created for the completeness of [Algebra.Graph.Acyclic.AdjacencyMap][Acyclic.AdjacencyMap].
This module highly depends on the `Ord` instance of the vertices.
Every operation in this module makes sure that for any 2 vertices
`x` and `y` in the graph, an edge can exist between `x` and `y` if
and only if `x < y`. This is similar to the partial order construction
method but uses the `Ord` instance of the element itself.


# The Shrink Operator

An amazing observation made by [Andrey Mokhov](https://github.com/snowleopard)
was that one can use `scc` to create a method of acyclic graph
construction which is safe and complete.

We can define `shrink` as,
```
shrink :: AdjacencyMap a -> Acyclic.AdjacencyMap a
shrink am = AAM (Map.map (NonEmpty.head . NonEmpty.vertexList1) aam)
  where
    AAM aam = scc am 
```

One could potentially replace `Ord` instance completely with `shrink`
operations. This has been demonstrated in [this gist](https://gist.github.com/adithyaov/bf3bf5d595bc1bb587d0aea03f1b3412).

Unfortunately, with the current implementation of `shrink`,
few important mathematical properties will be broken. 
For example, as Andrey Mokhov pointed out,

If we define `x == y` as `shrink x == shrink y`.
Then `edge 1 2 + edge 2 1 == vertex 1` but one cannot replace
`vertex 1` with `edge 1 2` in an expression without changing its
meaning. Because `vertex 1 + edge 2 3` will suddenly become 
`edge 1 2 + edge 2 1 + edge 2 3` which is equal to `edge 1 3`.

# Conclusion

`shrink` is a beautiful and a powerful operator which provides a
safe and a complete way to construct acyclic graphs. But it cannot
be used with the current implementation due to its side effects
on basic algebraic properties. Coming up with a proper implementation
of `shrink` would increase the usability and effectiveness of the
library. A proper implementation of `shrink` would replace the `Ord`
module and many other functions completely.

[Acyclic.AdjacencyMap]: https://github.com/snowleopard/alga/blob/master/src/Algebra/Graph/Acyclic/AdjacencyMap.hs
[Acyclic.AdjacencyMap.Ord]: https://github.com/snowleopard/alga/blob/master/src/Algebra/Graph/Acyclic/AdjacencyMap/Ord.hs
