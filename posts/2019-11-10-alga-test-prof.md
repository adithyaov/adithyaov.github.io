---
title: Profiling Alga's test suite
category: alga pinned
---

The following post describes how I profiled the Alga test suite. This is not a
tutorial on profiling. Please refer to the GHC manual if you want to learn
about profiling. This post is basically documenting the process I've used.
Please note that there could be a better method and a lot more that I can
improve on. If you find any mistakes or if you think there is scope for
improvement, please kindly let me know.

I strongly suggest going through profiling in the GHC manual before proceeding
further.

```
> tree test

test
├── Algebra
│   └── Graph
│       ├── Test
│       │   ├── Acyclic
│       │   │   └── AdjacencyMap.hs
│       │   ├── AdjacencyIntMap.hs
│       │   ├── AdjacencyMap.hs
│       │   ├── API.hs
│       │   ├── Arbitrary.hs
│       │   ├── Bipartite
│       │   │   └── AdjacencyMap.hs
│       │   ├── Export.hs
│       │   ├── Generic.hs
│       │   ├── Graph.hs
│       │   ├── Internal.hs
│       │   ├── Label.hs
│       │   ├── Labelled
│       │   │   ├── AdjacencyMap.hs
│       │   │   └── Graph.hs
│       │   ├── NonEmpty
│       │   │   ├── AdjacencyMap.hs
│       │   │   └── Graph.hs
│       │   ├── Relation
│       │   │   └── SymmetricRelation.hs
│       │   ├── Relation.hs
│       │   └── RewriteRules.hs
│       └── Test.hs
├── Data
│   └── Graph
│       └── Test
│           └── Typed.hs
└── Main.hs

11 directories, 21 files
```

Above is the directory structure for the test suite in Alga. You don't need to
remember anything, it's just for reference.  We can see that the main entry
point is `Main.hs`.  `Main.hs` runs a test defined in other modules. The `main`
functions looks like the following:

```
main :: IO ()
main = do
    selected <- getArgs
    let go current = when (null selected || current `elem` selected)
    go "Acyclic.AdjacencyMap"   testAcyclicAdjacencyMap
    go "AdjacencyIntMap"        testAdjacencyIntMap
    go "AdjacencyMap"           testAdjacencyMap
    go "Bipartite.AdjacencyMap" testBipartiteAdjacencyMap
    go "Export"                 testExport
    go "Graph"                  testGraph
    go "Internal"               testInternal
    go "Label"                  testLabel
    go "Labelled.AdjacencyMap"  testLabelledAdjacencyMap
    go "Labelled.Graph"         testLabelledGraph
    go "NonEmpty.AdjacencyMap"  testNonEmptyAdjacencyMap
    go "NonEmpty.Graph"         testNonEmptyGraph
    go "Relation"               testRelation
    go "Symmetric.Relation"     testSymmetricRelation
    go "Typed"                  testTyped
```

The main test function runs 14 separate tests defined in other modules. 
The names are self explanatory, for example, `testLabel` is defined in 
`Algebra.Graph.Test.Label` and so on.

We want to start by profiling the outermost module, that is, `test/Main.hs`.
Let's start by annotating the `Main` module with `{-# SCC ...`.

Our `Main` module will now look like the following:

```
main :: IO ()
main = do
    selected <- getArgs
    let go current = when (null selected || current `elem` selected)
    {-# SCC "Ann.Acyclic.AdjacencyMap"   #-} go "Acyclic.AdjacencyMap"   testAcyclicAdjacencyMap
    {-# SCC "Ann.AdjacencyIntMap"        #-} go "AdjacencyIntMap"        testAdjacencyIntMap
    {-# SCC "Ann.AdjacencyMap"           #-} go "AdjacencyMap"           testAdjacencyMap
    {-# SCC "Ann.Bipartite.AdjacencyMap" #-} go "Bipartite.AdjacencyMap" testBipartiteAdjacencyMap
    {-# SCC "Ann.Export"                 #-} go "Export"                 testExport
    {-# SCC "Ann.Graph"                  #-} go "Graph"                  testGraph
    {-# SCC "Ann.Internal"               #-} go "Internal"               testInternal
    {-# SCC "Ann.Label"                  #-} go "Label"                  testLabel
    {-# SCC "Ann.Labelled.AdjacencyMap"  #-} go "Labelled.AdjacencyMap"  testLabelledAdjacencyMap
    {-# SCC "Ann.Labelled.Graph"         #-} go "Labelled.Graph"         testLabelledGraph
    {-# SCC "Ann.NonEmpty.AdjacencyMap"  #-} go "NonEmpty.AdjacencyMap"  testNonEmptyAdjacencyMap
    {-# SCC "Ann.NonEmpty.Graph"         #-} go "NonEmpty.Graph"         testNonEmptyGraph
    {-# SCC "Ann.Relation"               #-} go "Relation"               testRelation
    {-# SCC "Ann.Symmetric.Relation"     #-} go "Symmetric.Relation"     testSymmetricRelation
    {-# SCC "Ann.Typed"                  #-} go "Typed"                  testTyped
```

Please note that the text after `SCC` can be anything. I prefixed the text with
`Ann` as it would be easier to filter it out.

Now compile the test suite (`test-alga`) with profiling enabled. We can set the
`profiling-detail` to `None`. Once compiled run the test suite with `-p`
enclosed within `RTS` options, like so: `./test-alga +RTS -p -RTS`. This should
start running the test suite. After execution is finished, a file named
`test-alga.prof` would be available in the same directory. This has all the
profiling information.

There is a lot of information in `test-alga.prof`. We are going to ignore most
of it for now. We are only interested in `Ann.` expressions.

```
> grep Ann\. test-alga.prog

 Ann.Acyclic.AdjacencyMap      Main    test/Main.hs:31:46-96      468   1    0.0    0.0     4.6    5.7
 Ann.AdjacencyIntMap           Main    test/Main.hs:32:46-92     1024   1    0.0    0.0     3.9    6.7
 Ann.AdjacencyMap              Main    test/Main.hs:33:46-89     1735   1    0.0    0.0     5.5    8.7
 Ann.Bipartite.AdjacencyMap    Main    test/Main.hs:34:46-98     2384   1    0.0    0.0    26.3   23.2
 Ann.Export                    Main    test/Main.hs:35:46-83     2711   1    0.0    0.0     0.1    0.2
 Ann.Graph                     Main    test/Main.hs:36:46-82     2968   1    0.0    0.0     1.9    3.0
 Ann.Internal                  Main    test/Main.hs:37:46-85     3919   1    0.0    0.0     0.0    0.0
 Ann.Label                     Main    test/Main.hs:38:46-82     4005   1    0.0    0.0    28.0   11.0
 Ann.Labelled.AdjacencyMap     Main    test/Main.hs:39:46-97     7821   1    0.0    0.0     3.7    5.2
 Ann.Labelled.Graph            Main    test/Main.hs:40:46-90     8468   1    0.0    0.0     0.9    1.0
 Ann.NonEmpty.AdjacencyMap     Main    test/Main.hs:41:46-97     9106   1    0.0    0.0     3.1    4.7
 Ann.NonEmpty.Graph            Main    test/Main.hs:42:46-90     9805   1    0.0    0.0     1.1    2.2
 Ann.Relation                  Main    test/Main.hs:43:46-85    10702   1    0.0    0.0     8.0   11.3
 Ann.Symmetric.Relation        Main    test/Main.hs:44:46-94    11480   1    0.0    0.0    12.7   16.8
 Ann.Typed                     Main    test/Main.hs:45:46-82    12133   1    0.0    0.0     0.3    0.5
```

Let's look into the last 2 parameters for now.  The last parameter is the
percentage of the total memory allocations (excluding profiling overheads) of
the program made by this call and all of its sub-calls.  The last but one
parameter is percentage of the total run time of the program spent below this
point in the call tree.

If we were to sort these lines based on the last parameter we would see that
the functions corresponding to `Ann.Label`, `Ann.Bipartite.AdjacencyMap` are 
very heavy. Both, together add up to over 55% of the total run time which is
over 6 minutes. Now we know that `testLabel` and `testBipartiteAdjacencyMap`
consume a lot of time.

We will now add `SCC` annotations to expressions in those functions.

`testLabel` looks like the following:

```
testLabel :: IO ()
testLabel = do
    putStrLn "\n============ Any ============"
    {-# SCC "Any_Semiring" #-}     test "Semiring"     $ \(a :: Any) b c -> testSemiring a b c
    {-# SCC "Any_StarSemiring" #-} test "StarSemiring" $ \(a :: Any) b c -> testStarSemiring a b c
    {-# SCC "Any_Dioid" #-}        test "Dioid"        $ \(a :: Any) b c -> testDioid a b c

    putStrLn "\n============ Distance ============"
    test "Semiring"     $ \(a :: Distance Int) b c -> testSemiring a b c
    test "StarSemiring" $ \(a :: Distance Int) b c -> testStarSemiring a b c
    test "Dioid"        $ \(a :: Distance Int) b c -> testDioid a b c

    putStrLn "\n============ Capacity ============"
    test "Semiring"     $ \(a :: Capacity Int) b c -> testSemiring a b c
    test "StarSemiring" $ \(a :: Capacity Int) b c -> testStarSemiring a b c
    test "Dioid"        $ \(a :: Capacity Int) b c -> testDioid a b c

    putStrLn "\n============ Minimum ============"
    test "LeftNearRing" $ \(a :: Minimum (Path Int)) b c -> testLeftNearRing a b c

    putStrLn "\n============ PowerSet ============"
    test "Semiring"     $ \(a :: PowerSet (Path Int)) b c -> testSemiring a b c
    test "Dioid"        $ \(a :: PowerSet (Path Int)) b c -> testDioid a b c

    putStrLn "\n============ Count ============"
    test "Semiring"     $ \(a :: Count Int) b c -> testSemiring a b c
    test "StarSemiring" $ \(a :: Count Int) b c -> testStarSemiring a b c
```

After adding proper annotations, it looks like:

```
testLabel :: IO ()
testLabel = do
    putStrLn "\n============ Any ============"
    {-# SCC "Any_Semiring" #-}     test "Semiring"     $ \(a :: Any) b c -> testSemiring a b c
    {-# SCC "Any_StarSemiring" #-} test "StarSemiring" $ \(a :: Any) b c -> testStarSemiring a b c
    {-# SCC "Any_Dioid" #-}        test "Dioid"        $ \(a :: Any) b c -> testDioid a b c

    putStrLn "\n============ Distance ============"
    {-# SCC "Ann1.Distance_Semiring" #-}     test "Semiring"     $ \(a :: Distance Int) b c -> testSemiring a b c
    {-# SCC "Ann1.Distance_StarSemiring" #-} test "StarSemiring" $ \(a :: Distance Int) b c -> testStarSemiring a b c
    {-# SCC "Ann1.Distance_Dioid" #-}        test "Dioid"        $ \(a :: Distance Int) b c -> testDioid a b c

    putStrLn "\n============ Capacity ============"
    {-# SCC "Ann1.Capacity_Semiring" #-}     test "Semiring"     $ \(a :: Capacity Int) b c -> testSemiring a b c
    {-# SCC "Ann1.Capacity_StarSemiring" #-} test "StarSemiring" $ \(a :: Capacity Int) b c -> testStarSemiring a b c
    {-# SCC "Ann1.Capacity_Dioid" #-}        test "Dioid"        $ \(a :: Capacity Int) b c -> testDioid a b c

    putStrLn "\n============ Minimum ============"
    {-# SCC "Ann1.Minimum_LeftNearRing" #-} test "LeftNearRing" $ \(a :: Minimum (Path Int)) b c -> testLeftNearRing a b c

    putStrLn "\n============ PowerSet ============"
    {-# SCC "Ann1.PowerSet_Semiring" #-} test "Semiring"     $ \(a :: PowerSet (Path Int)) b c -> testSemiring a b c
    {-# SCC "Ann1.PowerSet_Dioid" #-}    test "Dioid"        $ \(a :: PowerSet (Path Int)) b c -> testDioid a b c

    putStrLn "\n============ Count ============"
    {-# SCC "Ann1.Count_Semiring" #-}     test "Semiring"     $ \(a :: Count Int) b c -> testSemiring a b c
    {-# SCC "Ann1.Count_StarSemiring" #-} test "StarSemiring" $ \(a :: Count Int) b c -> testStarSemiring a b c
```

This time we prefixed the text with `Ann1`

```
> grep Ann1\. test-alga.prog

Ann1.Any_Dioid               Algebra.Graph.Test.Label    test/Algebra/Graph/Test/Label.hs:125:41-96     4550    1    0.0    0.0     0.0    0.0
Ann1.Any_Semiring            Algebra.Graph.Test.Label    test/Algebra/Graph/Test/Label.hs:123:41-99     4006    1    0.0    0.0     0.0    0.0
Ann1.Any_StarSemiring        Algebra.Graph.Test.Label    test/Algebra/Graph/Test/Label.hs:124:41-103    4265    1    0.0    0.0     0.0    0.0
Ann1.Capacity_Dioid          Algebra.Graph.Test.Label    test/Algebra/Graph/Test/Label.hs:135:46-110    6137    1    0.0    0.0     0.0    0.0
Ann1.Capacity_Semiring       Algebra.Graph.Test.Label    test/Algebra/Graph/Test/Label.hs:133:46-113    5601    1    0.0    0.0     0.0    0.0
Ann1.Capacity_StarSemiring   Algebra.Graph.Test.Label    test/Algebra/Graph/Test/Label.hs:134:46-117    5873    1    0.0    0.0     0.0    0.0
Ann1.Count_Semiring          Algebra.Graph.Test.Label    test/Algebra/Graph/Test/Label.hs:145:43-107    7285    1    0.0    0.0     0.0    0.0
Ann1.Count_StarSemiring      Algebra.Graph.Test.Label    test/Algebra/Graph/Test/Label.hs:146:43-111    7557    1    0.0    0.0     0.0    0.0
Ann1.Distance_Dioid          Algebra.Graph.Test.Label    test/Algebra/Graph/Test/Label.hs:130:46-110    5337    1    0.0    0.0     0.0    0.0
Ann1.Distance_Semiring       Algebra.Graph.Test.Label    test/Algebra/Graph/Test/Label.hs:128:46-113    4801    1    0.0    0.0     0.0    0.0
Ann1.Distance_StarSemiring   Algebra.Graph.Test.Label    test/Algebra/Graph/Test/Label.hs:129:46-117    5073    1    0.0    0.0     0.0    0.0
Ann1.Minimum_LeftNearRing    Algebra.Graph.Test.Label    test/Algebra/Graph/Test/Label.hs:138:45-122    6401    1    0.0    0.0     0.0    0.0
Ann1.PowerSet_Dioid          Algebra.Graph.Test.Label    test/Algebra/Graph/Test/Label.hs:142:42-113    7010    1    0.0    0.0    19.7    7.8
Ann1.PowerSet_Semiring       Algebra.Graph.Test.Label    test/Algebra/Graph/Test/Label.hs:141:42-116    6712    1    0.0    0.0    21.8    7.5
```

If we were to sort these lines based on the last parameter we would see that
the functions corresponding to `Ann1.PowerSet_Semiring` and
`Ann1.PowerSet_Dioid` are very heavy. Please note that the readings may be
different since the test cases are generated randomly. In this case to reduce
the runtime, we will choose to control the random test cases generated. We
could choose to use smaller `PowerSet` or lesser number of cases.

`testLabel` is a less than 20 lines long and hence it is easy for us to add
`SCC` annotations to almost every expression. On the other hand
`testBipartiteAdjacencyMap` is over 750 lines long. It becomes very difficult
to add annotations to every expression when profiling huge files. We will group
a set of expressions and add an annotation to the combined expression.

For example, this:

```
x = do
  test exp1
  test exp2
  test exp3
```

Will become:

```
x = do
  {-# SCC "Annotation" #-} do
    test exp1
    test exp2
    test exp3
```

`testBipartiteAdjacencyMap` looks like:

```
testBipartiteAdjacencyMap :: IO ()
testBipartiteAdjacencyMap = do
  putStrLn "\n============ Bipartite.AdjacencyMap.consistent ============"
  test "consistent empty            == True" $
      consistent (empty :: BAII)
  test "consistent (vertex x)       == True" $ \x ->
      consistent (vertex x :: BAII)
  ...

  putStrLn "\n============ Bipartite.AdjacencyMap.toBipartite ============"
  test "leftAdjacencyMap (toBipartite empty) == Map.empty" $
      (leftAdjacencyMap $ toBipartite (AM.empty :: AII)) == Map.empty
  ...
```

I choose to make the block of similar code below a `putStrLn` statement into
a single expression. Hence, it would look like:

```
testBipartiteAdjacencyMap :: IO ()
testBipartiteAdjacencyMap = do
  {-# SCC "Ann2.consistent" -#} do
    putStrLn "\n============ Bipartite.AdjacencyMap.consistent ============"
    test "consistent empty            == True" $
        consistent (empty :: BAII)
    test "consistent (vertex x)       == True" $ \x ->
        consistent (vertex x :: BAII)
    ...

  {-# SCC "Ann2.toBipartite " -#} do
    putStrLn "\n============ Bipartite.AdjacencyMap.toBipartite ============"
    test "leftAdjacencyMap (toBipartite empty) == Map.empty" $
        (leftAdjacencyMap $ toBipartite (AM.empty :: AII)) == Map.empty
    ...
```

This way, I'll have to add about 40 annotations manually. Compile and `grep` 
`Ann2.`.

We can reduce some annotations with some domain specific knowledge, but for
now, we'll profile everything.


```
> grep Ann2\. test-alga.prog

Ann2.Eq                 A    B    C    1    0.0    0.0     3.3    2.8
Ann2.Num                A    B    C    1    0.0    0.0     0.0    0.0
Ann2.Show               A    B    C    1    0.0    0.0     0.0    0.0
Ann2.biclique           A    B    C    1    0.0    0.0     0.0    0.0
Ann2.circuit            A    B    C    1    0.0    0.0     0.0    0.0
Ann2.connect            A    B    C    1    0.0    0.0     1.3    1.2
Ann2.connects           A    B    C    1    0.0    0.0    21.0   10.5
Ann2.consistent         A    B    C    1    0.0    0.0     0.2    0.2
Ann2.detectParts        A    B    C    1    0.0    0.0     0.4    0.4
Ann2.edge               A    B    C    1    0.0    0.0     0.0    0.0
Ann2.edgeCount          A    B    C    1    0.0    0.0     0.0    0.0
Ann2.edgeList           A    B    C    1    0.0    0.0     0.0    0.0
Ann2.edgeSet            A    B    C    1    0.0    0.0     0.0    0.0
Ann2.edges              A    B    C    1    0.0    0.0     0.0    0.0
Ann2.empty              A    B    C    1    0.0    0.0     0.0    0.0
Ann2.fromBipartite      A    B    C    1    0.0    0.0     0.1    0.1
Ann2.fromGraph          A    B    C    1    0.0    0.0     0.0    0.0
Ann2.hasEdge            A    B    C    1    0.0    0.0     0.1    0.1
Ann2.hasLeftVertex      A    B    C    1    0.0    0.0     0.0    0.0
Ann2.hasRightVertex     A    B    C    1    0.0    0.0     0.0    0.0
Ann2.hasVertex          A    B    C    1    0.0    0.0     0.1    0.1
Ann2.isEmpty            A    B    C    1    0.0    0.0     0.1    0.1
Ann2.leftAdjacencyMap   A    B    C    1    0.0    0.0     0.0    0.0
Ann2.leftVertex         A    B    C    1    0.0    0.0     0.0    0.0
Ann2.leftVertexCount    A    B    C    1    0.0    0.0     0.0    0.0
Ann2.leftVertexList     A    B    C    1    0.0    0.0     0.1    0.1
Ann2.leftVertexSet      A    B    C    1    0.0    0.0     0.0    0.0
Ann2.overlay            A    B    C    1    0.0    0.0     1.0    0.8
Ann2.overlays           A    B    C    1    0.0    0.0     5.8    5.1
Ann2.rightAdjacencyMap  A    B    C    1    0.0    0.0     0.0    0.0
Ann2.rightVertex        A    B    C    1    0.0    0.0     0.0    0.0
Ann2.rightVertexCount   A    B    C    1    0.0    0.0     0.0    0.0
Ann2.rightVertexList    A    B    C    1    0.0    0.0     0.1    0.1
Ann2.rightVertexSet     A    B    C    1    0.0    0.0     0.0    0.0
Ann2.swap               A    B    C    1    0.0    0.0     0.1    0.1
Ann2.toBipartite        A    B    C    1    0.0    0.0     0.0    0.0
Ann2.toBipartiteWith    A    B    C    1    0.0    0.0     0.2    0.2
Ann2.vertex             A    B    C    1    0.0    0.0     0.0    0.0
Ann2.vertexCount        A    B    C    1    0.0    0.0     0.1    0.1
Ann2.vertexList         A    B    C    1    0.0    0.0     0.1    0.1
Ann2.vertexSet          A    B    C    1    0.0    0.0     0.0    0.0
Ann2.vertices           A    B    C    1    0.0    0.0     0.0    0.0
Ann2.Num                A    B    C    0    0.0    0.0     0.0    0.0
Ann2.Show               A    B    C    0    0.0    0.0     0.0    0.0
Ann2.biclique           A    B    C    0    0.0    0.0     0.0    0.0
Ann2.circuit            A    B    C    0    0.0    0.0     0.0    0.0
Ann2.connect            A    B    C    0    0.0    0.0     0.0    0.0
Ann2.connects           A    B    C    0    0.0    0.0     0.0    0.0
Ann2.consistent         A    B    C    0    0.0    0.0     0.0    0.0
Ann2.detectParts        A    B    C    0    0.0    0.0     0.0    0.0
Ann2.edge               A    B    C    0    0.0    0.0     0.0    0.0
Ann2.edgeList           A    B    C    0    0.0    0.0     0.0    0.0
Ann2.edgeSet            A    B    C    0    0.0    0.0     0.0    0.0
Ann2.edges              A    B    C    0    0.0    0.0     0.0    0.0
Ann2.fromBipartite      A    B    C    0    0.0    0.0     0.0    0.0
Ann2.fromGraph          A    B    C    0    0.0    0.0     0.0    0.0
Ann2.hasEdge            A    B    C    0    0.0    0.0     0.0    0.0
Ann2.isEmpty            A    B    C    0    0.0    0.0     0.0    0.0
Ann2.leftAdjacencyMap   A    B    C    0    0.0    0.0     0.0    0.0
Ann2.leftVertex         A    B    C    0    0.0    0.0     0.0    0.0
Ann2.leftVertexCount    A    B    C    0    0.0    0.0     0.0    0.0
Ann2.leftVertexList     A    B    C    0    0.0    0.0     0.0    0.0
Ann2.leftVertexSet      A    B    C    0    0.0    0.0     0.0    0.0
Ann2.overlay            A    B    C    0    0.0    0.0     0.0    0.0
Ann2.overlays           A    B    C    0    0.0    0.0     0.0    0.0
Ann2.rightAdjacencyMap  A    B    C    0    0.0    0.0     0.0    0.0
Ann2.rightVertex        A    B    C    0    0.0    0.0     0.0    0.0
Ann2.rightVertexCount   A    B    C    0    0.0    0.0     0.0    0.0
Ann2.rightVertexList    A    B    C    0    0.0    0.0     0.0    0.0
Ann2.rightVertexSet     A    B    C    0    0.0    0.0     0.0    0.0
Ann2.swap               A    B    C    0    0.0    0.0     0.0    0.0
Ann2.toBipartite        A    B    C    0    0.0    0.0     0.0    0.0
Ann2.toBipartiteWith    A    B    C    0    0.0    0.0     0.0    0.0
Ann2.vertex             A    B    C    0    0.0    0.0     0.0    0.0
Ann2.vertexCount        A    B    C    0    0.0    0.0     0.0    0.0
Ann2.vertexList         A    B    C    0    0.0    0.0     0.0    0.0
Ann2.vertexSet          A    B    C    0    0.0    0.0     0.0    0.0
Ann2.vertices           A    B    C    0    0.0    0.0     0.0    0.0
```

A = Algebra.Graph.Test.Bipartite.AdjacencyMap

B = test/Algebra/Graph/Test/Bipartite/AdjacencyMap.hs:(647,27)-(682,28)

C = Id

We can see that the expression corresponding to `Ann2.connects` takes a huge
amount of time to run. We could go a level deeper to see which of the
expression in the set of expressions takes longer. But we have concluded that
`connects` is a heavy operation.

In this case we are working with pure functions and hence there is no memory
leak.  In some cases the function implementation can be improved, but in this
case it is no possible. The efficiency of `connects` directly correlates with
the efficiency of operations in the `Data.Map` module. This is the same case
for `Semiring` and `Dioid` instance of `PowerSet`. One way we can run the tests
faster is to reduce the test size, that is, either use smaller test elements or
decrease the number of test cases. We will be using the second method.

Consider,

```
size10 :: Testable prop => prop -> Property
size10 = mapSize (min 10)
```

This function basically reduces the number of test cases to a maximum of 10.

Consider the following changes to `testLabel`,

From:

```
...
    putStrLn "\n============ PowerSet ============"
    test "Semiring" $ \(a :: PowerSet (Path Int)) b c -> testSemiring a b c
    test "Dioid"    $ \(a :: PowerSet (Path Int)) b c -> testDioid a b c
...
```

To:

...
    putStrLn "\n============ PowerSet ============"
    test "Semiring" $ size10 $ \(a :: PowerSet (Path Int)) b c -> testSemiring a b c
    test "Dioid"    $ size10 $ \(a :: PowerSet (Path Int)) b c -> testDioid a b c
...
```

Consider the following changes to `testBipartiteAdjacencyMap`,

From:

```
...
  putStrLn "\n============ Bipartite.AdjacencyMap.connects ============"
  test "connects []           == empty" $
        connects []           == (empty :: BAII)
  test "connects [x]          == x" $ \(x :: BAII) ->
        connects [x]          == x
  test "connects [x, y]       == connect x y" $ \(x :: BAII) (y :: BAII) ->
        connects [x, y]       == connect x y
  test "connects xs           == foldr connect empty xs" $ \(xs :: [BAII]) ->
        connects xs           == foldr connect empty xs
  test "isEmpty (connects xs) == all isEmpty xs" $ \(xs :: [BAII]) ->
        isEmpty (connects xs) == all isEmpty xs
...
```

To:

```
...
  putStrLn "\n============ Bipartite.AdjacencyMap.connects ============"
  test "connects []           == empty" $
        connects []           == (empty :: BAII)
  test "connects [x]          == x" $ \(x :: BAII) ->
        connects [x]          == x
  test "connects [x, y]       == connect x y" $ size10 $ \(x :: BAII) (y :: BAII) ->
        connects [x, y]       == connect x y
  test "connects xs           == foldr connect empty xs" $ size10 $ \(xs :: [BAII]) ->
        connects xs           == foldr connect empty xs
  test "isEmpty (connects xs) == all isEmpty xs" $ \(xs :: [BAII]) ->
        isEmpty (connects xs) == all isEmpty xs
...
```

(Changed expression for `connect` and `connects`)

These changes reduces the run time of the test suit to about 300 sec (5 min).
That's a 50% reduction!

### Automation?

Doing this by hand is pretty tiresome. It would be great to automate this
process.  Considering that this is the structure for tests in the test suite,
the following is a simple solution.

1. Commit the current state.
2. `addAnn :: String -> String -> String`
   `addAnn a b` adds annotation to every expression of the function `a` in file
   (contents) `b`a.
3. Run tests and extract important info using some prefixed anchor points.
4. Discard all the changes bringing files back to normal. 

These steps can definately automated with a small script.

There could probably be a better solution for automation. Suggesstions are
always welcome.


