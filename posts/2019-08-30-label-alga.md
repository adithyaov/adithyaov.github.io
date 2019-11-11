---
title: The Label Module
category: alga pinned
---

*NOTE*: This article is structured improperly which leads to confusion.
I'll rewrite this article in the near future.  

The initial plan for this post was to explain how the [Label][label]
module worked in [Alga][alga] but instead I think it is easier to
develop a mini version of the module from scratch. This would help
in a deeper understanding of how the module is built.

Before we dive into the creation of the module itself we will look
at requirements of some basic [algebraic structures][aSWiki]. In
particular we will look at the definition of [Monoid][monWiki] and
[Semiring][semiWiki].

# Monoid

A semiring is a set `S` equipped with a binary operation `<>`
satisfying the following two axioms:

1. Associativity:
  For all `a`, `b` and `c` in `S`, the equation
  `(a <> b) <> c = a <> (b <> c)` holds.
2. Identity element:
  There exists an element `e` in `S` such that for every element `a`
  in `S`, the equations `e <> a = a <> e = a` hold.

Some examples of monoids in Haskell are `(Int, +, 0)`, `(Int, *, 1)`,
`([a], ++, [])` (list data type), etc. It is easy to see that both
the required axioms are followed for all the mentioned data types.

This algebraic structure is very common in programming and fortunately,
there is already a `Monoid` type class implemented in the base library 
as [Data.Monoid][monHack].

## Commutative Monoid

A commutative monoid is a monoid that also satisfies the 
commutative property. That is, for all `a`  and `b` in `S`, the
equation `a <> b = b <> a` holds.

It can be seen that `(Int, +, 0)` and `(Int, *, 1)` are commutative
monoids whereas `([a], ++, [])` is not a commutative monoid.

# Semiring

A semiring is a set `R` equipped with two binary operations `<+>` and 
`<.>`, called addition and multiplication, and two elements in `R`,
`zero` and `one` such that:

1. `(R, <+>, zero)` is a commutative monoid:
```
(a + b) + c = a + (b + c)
zero + a = a + zero = a
a + b = b + a
```
2. `(R, <.>, one)` is a monoid:
```
(a <.> b) <.> c = a <.> (b <.> c)
one <.> a = a <.> one = a
```
3. Multiplication (`<.>`) left and right distributes over Addition (`<+>`):
```
a <.> (b <+> c) = (a <.> b) <+> (a <.> c)
(a + b)⋅c = (a⋅c) + (b⋅c)
```
4. Multiplication (`<.>`) by `zero` annihilates `R`:
```
zero <.> a = a <.> zero = zero
```

The most natural semiring that one can think of is `(Int, +, *, 0, 1)`.
It is easy to see that it follows all the semiring laws.

# Implementation of semiring

Unfortunately, the base library does not provide a type class for
semirings. Let's create a simple implementation of the semiring type
class.

```
class (Monoid a) => Semiring a where
    (<+>) :: a -> a -> a
    (<.>) :: a -> a -> a
    zero  :: a
    one   :: a
```

We could simplify the above implementation. Since we already know that
`a` is already a monoid we can abstract out `<+>` and `zero`. We
could rewrite the above as:

```
class (Monoid a) => Semiring a where
    (<.>) :: a -> a -> a
    one   :: a

(<+>) = (<>)
zero = mempty
```

**Note:** Note that just because a data type has an instance of
`Semiring` does not guarantee that it follows all the semiring laws.

# The Distance semiring

The `Distance` semiring is implemented on a set of non-negative numbers.
By numbers, I mean any data type that implements the `Num` type class.

We have to first come up with a way to capture the notion of a
non-negative number. Non-negative numbers can be finite or infinite,
so before we create a data type for capturing Non-negative numbers
we need to create a data type to capture finite and infinite numbers.

## The Extended data type

This data type is intended to capture finiteness and infinity of a type.
```
data Extended a = Finite a | Infinite
    deriving (Eq, Functor, Ord, Show)
```

Now let's create a `Num` instance for `Extended`.

```
instance Num a => Num (Extended a) where
    fromInteger = Finite . fromInteger

    Infinite + _ = Infinite
    _ + Infinite = Infinite
    Finite x + Finite y = Finite $ x + y

    Finite 0 * _ = Finite 0
    _ * Finite 0 = Finite 0
    Finite x * Finite y = Finite $ x * y

    negate = fmap negate
    signum = fmap signum
    abs    = fmap abs
```

Please note that when we use `0`, it is not the integer `0`. We can
rely on the Haskell ecosystem to generate the proper `0` using the
given `Num` instance.

## The NonNegative data type

Now that we have created the `Extended`, we can proceed to create a
datatype to capture Non-negative numbers.

```
newtype NonNegative a = NonNegative (Extended a)
    deriving (Applicative, Eq, Functor, Ord, Monad)
```

Similar to `Extended` lets create the `Num` instance.

```
instance (Num a, Ord a) => Num (NonNegative a) where
    fromInteger x | f < 0     = error "NonNegative values cannot be negative"
                  | otherwise = unsafeFinite f
      where
        f = fromInteger x

    (+) = coerce ((+) :: Extended a -> Extended a -> Extended a)
    (*) = coerce ((*) :: Extended a -> Extended a -> Extended a)

    negate _ = error "NonNegative values cannot be negated"

    signum (NonNegative Infinite) = 1
    signum x = signum <$> x
    abs = id
```

From here on, whenever we see `NonNegative` data type we can be sure
that it is a non-negative number.

Finally, lets create the `Distance` semiring. 

```
newtype Distance a = Distance (Min (NonNegative a))
    deriving (Bounded, Eq, Monoid, Num, Ord, Semigroup)
```

Notice the `Min` wrapping `NonNegative`. `Min` can be found in
the base library in [Data.Semigroup][sgHack]. It has an instance of
`Monoid` that naturally finds the minimum (`(<>) = min`).
This means that this semiring is suitable to find the minimum naturally.
As we will see later, this semiring can be used to find minimum
distance in a graph. We could have also written this as
`Distance (Max (NonNegative a))` which is naturally built for finding
the maximum.

The `Semiring` instance would be something like:

```
instance (Num a, Ord a) => Semiring (Distance a) where
    one   = 0
    (<.>) = (+)
```

Let's try a few equations with `Distance`.

```
(4 :: Distance Int) <+> 5 = 4
(4 :: Distance Int) <.> 5 = 9
```

How would this help us while using a labelled graph? Let's say we
have 2 vertices `x` and `y` with current `Distance` values `v1` and
`v2` attached to `x` and `y` and let `e` be the `Distance` value of
the edge between `x` and `y`. Let's also define a function on an edge
`(e, v1, v2)` as ` f v2 = v2 <+> (v1 <.> e)`. If we update the value
at `y` to `f v2`, we can see that `f` is equivalent to relaxation of
the edge between `x` and `y`.

Notice that in the distance semiring, `<+>` selects one of its
arguments. That is, `x <+> y == x || x <+> y == y`.
This kind of semiring is called a selective semiring.

Lets define another semiring called `Count`.

```
newtype Count a = Count (Sum (NonNegative a))
    deriving (Bounded, Eq, Monoid, Num, Ord, Semigroup)
```

`Sum` similar to `Min` is a monoid that naturally find the
sum of elements (`(<>) = (+)`)

The `Semiring` instance would be something like:

```
instance (Num a, Ord a) => Semiring (Count a) where
    one   = 1
    (<.>) = (*)
```

Let's try a few equations with `Count`.

```
(4 :: Count Int) <+> 5 = 9
(4 :: Count Int) <.> 5 = 20
```

Unlike `Distance`, `Count` is not a selective semiring.

With the previous idea of relaxation in mind, let's define a simple
relaxation algorithm for labelled graphs which relaxes all the
edges `n` times, where `n` is the number of vertices. This is similar
to Bellman-Ford algorithm. Note that for keeping things simple, the
following is a psuedo code and not real haskell. Assume that `insert`
and `find` are operations that somehow update the `Map`.

```
insert :: a -> e -> Map a e

find :: a -> Map a e -> e

relax :: (Semiring e) => a -> Labelled.AdjacencyMap e a -> Map a e
relax s g =
  map init (vertices g)
  insert s one
  for i = 1 to (length (vertices g)):
    map relax (edges g)

init v =
  insert v zero

relax (e, v1, v2) =
  insert v2 ((find v2) <+> ((find v1) <.> e))
```

Basically, we take a source vertex and make it's value `one` then
relax all the edges `n` times, where `n` is the number of vertices.
Hopefully tand he algorithm is somewhat understandable.

We can now use this algorithm with the `Distance` semiring. When
we use the `Distance` semiring, this is nothing but a single source
shortest path algorithm.

Now, say we want to somehow find the shortest distance to each vertex and count
the number of shortest paths. We can either make a different type
of semiring or we can just somehow combine `Distance` with `Count`.
Since we all love programming with combination lets make a way to
combine two semirings.

# The Optimum data type

Lets define `Optimum` with a semiring `o` that has an optimizing
criterion and a semiring `a` that keeps track of some argument.
Although I say that `a` should be a semiring we can relax few 
properties on the semiring depending on `a`. For example in
[Algebra.Graph.Label][label] the data type `Minimum`, although it
has an instance of `Semiring`, is not a semiring but a near left ring.

```
data Optimum o a = Optimum { getOptimum :: o, getArgument :: a }
    deriving (Eq, Ord, Show)

instance (Eq o, Monoid a, Monoid o) => Monoid (Optimum o a) where
    zero = Optimum zero zero 
    Optimum o1 a1 <> Optimum o2 a2
        | o1 == o2  = Optimum o1 (mappend a1 a2)
        | otherwise = Optimum o a
            where
              o = mappend o1 o2
              a = if o == o1 then a1 else a2

instance (Eq o, Semiring a, Semiring o) => Semiring (Optimum o a) where
    one = Optimum one one
    Optimum o1 a1 <.> Optimum o2 a2 = Optimum (o1 <.> o2) (a1 <.> a2)
```

Please note that `<>` is actually an operation of `Semigroup` type
class but I'm using `<>` and `mappend` interchangeably.

Now consider the semiring `Optimum (Distance Int) (Count Int)`.
If possible, take out a paper and try the above relaxation algorithm
with a simple graph using this semiring as the edge type.
You will see that we have achieved the required task finding shortest
distance along with counting the number of shortest paths.

# Conclusion

We made a simple prototype of the library [Algebra.Graph.Label][label].
Hopefully this post helped you gain a deeper understanding of the module.


[label]: https://hackage.haskell.org/package/algebraic-graphs-0.4/docs/Algebra-Graph-Label.html
[alga]: https://hackage.haskell.org/package/algebraic-graphs-0.4
[aSWiki]: https://en.wikipedia.org/wiki/Algebraic_structure
[monWiki]: https://en.wikipedia.org/wiki/Monoid
[semiWiki]: https://en.wikipedia.org/wiki/Semiring
[monHack]: https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html
[sgHack]: https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Semigroup.html




















