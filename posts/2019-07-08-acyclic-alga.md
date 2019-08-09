---
title: Experimenting with acyclic graphs in Alga
category: alga pinned
---

The following post describes the ideas and experiments conducted to work
safely with acyclic graphs in [Alga][alga]. Some of these are in the final
draft and some aren't included due to issues with safety.

# Motivation behind acyclic graphs

The basic idea is that the user should be able to use acyclic graphs without
many restrictions. Ideally the user should not be able to compile programs
that result in an acyclic graph. The avenue of a strict type safe acyclic graphs
was explored but due to the limitations of type system this was not possible.
 
Acyclic graphs are both common and heavily used in dependency
management. This is demonstrated in the section 
**Construction from algebraic graph and a partial order** but unfortunately
even this is removed from the final draft due to being unsafe.

It would also makes it easier to work with algorithms like `scc` or `topSort`
as has been remarked in [some][issue#152] [issues][issue#154].

# Construction methods

Below are a few ways one could construct an acyclic graph. In the [final draft][Acyclic.AdjacencyMap]
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

Example (The following is similar to dependency management):

Here is a simple example where we do have a partial order in advance:
* Every object file depends only on C source files: `file.c` < `file.o`.
* Every executable depends only on object files: `file.o` < `file.exe`.

Here we could just use `<` from the derived `Ord` instance for the extension data type:

```
data Extension = C | O | Exe deriving (Eq, Ord)

type File = (FilePath, Extension)

partialOrder :: PartialOrder File
partialOrder (_, x) (_, y) = x < y
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
As mentioned earlier, ease of use is an important motivation for the library.
The `Ord` module makes it easier to use acyclic graphs by providing operators like
`connect`, `overlay`, etc.

This module highly depends on the `Ord` instance of the vertices.
Every operation in this module makes sure that for any 2 vertices
`x` and `y` in the graph, an edge can exist between `x` and `y` if
and only if `x < y`. This is similar to the partial order construction
method but uses the `Ord` instance of the element itself.

# The Shrink Operator

An amazing observation made by [Andrey Mokhov][snowleopard]
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
operations. This has been demonstrated in [this gist][shrink gist].

Unfortunately, with the current implementation of `shrink`,
few important mathematical properties will be broken. 
For example, as Andrey Mokhov pointed out,

If we define `x == y` as `shrink x == shrink y`.
Then `edge 1 2 + edge 2 1 == vertex 1` but one cannot replace
`vertex 1` with `edge 1 2` in an expression without changing its
meaning. Because `vertex 1 + edge 2 3` will suddenly become 
`edge 1 2 + edge 2 1 + edge 2 3` which is equal to `edge 1 3`.

# Conclusion

Although the `Ord` module makes it easier to work with acyclic graphs,
it is not complete.
`shrink` is a beautiful and a powerful operator which provides a
safe and a complete way to construct acyclic graphs. But it cannot
be used with the current implementation due to its side effects
on basic algebraic properties. Coming up with a proper implementation
of `shrink` would increase the usability and effectiveness of the
library. A proper implementation of `shrink` would replace the `Ord`
module and many other functions completely.

[paper]: https://github.com/snowleopard/alga-paper
[talk1]: https://www.youtube.com/watch?v=EdQGLewU-8k
[talk2]: https://skillsmatter.com/skillscasts/10635-algebraic-graphs
[tutorial]: https://nobrakal.github.io/alga-tutorial
[wiki]: https://github.com/snowleopard/alga/wiki
[issue#152]: https://github.com/snowleopard/alga/issues/152
[issue#154]: https://github.com/snowleopard/alga/issues/154
[snowleapord]: https://github.com/snowleopard 
[shrink gist]: https://gist.github.com/adithyaov/bf3bf5d595bc1bb587d0aea03f1b3412
[Acyclic.AdjacencyMap]: https://github.com/snowleopard/alga/blob/master/src/Algebra/Graph/Acyclic/AdjacencyMap.hs
[Acyclic.AdjacencyMap.Ord]: https://github.com/snowleopard/alga/blob/master/src/Algebra/Graph/Acyclic/AdjacencyMap/Ord.hs
