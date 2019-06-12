---
title: Kademlia - Part 2
sub: The protocol.
img: kademlia/kademlia.jpg
---

Now lets try and understand the protocol. The protocol's aim is basically to store/look-up (key, value) pairs.
We know that nodes are described with B-bit ids, similarly the keys are also B-bit in length. We will first define
the notion of closeness between a node and a (key, value) pair, this will allow us to talk about storing/looking-up
(key, value) pairs on k closest nodes to the key. Bear with me, we will understand why k closest nodes later.

### XOR Metric

The distance function used in Kademlia is the `XOR` distance between the binary values.
To assign (key, value) pairs to particular nodes, Kademlia relies on the `XOR` distance between the identifiers.
 
The `XOR` table

`1 XOR 0 = 0 XOR 1 = 1`

`1 XOR 1 = 0 XOR 0 = 0`

`XOR` distance follows all the properties of a valid distance,
1. `xor`(x, y) = `xor`(y, x)
2. `xor`(x, x) = 0
3. `xor`(x, y) <= `xor`(x, z) + `xor`(z, y) (Triangle property)

An interesting property of `XOR` is that it is unidirectional, ie. for any given distance d
and a binary value x, there exists a unique y such that `xor`(x, y) = d. This property ensures that
all look-ups for the same key converge along the same path, no matter which node the look-up originated from.

From here on `xor` is the function `xor :: Binary -> Binary -> Int`
and `XOR` is the operator.

### Node state

Every node has contact information to route query messages (look-up, store etc.)
For each `0 <= i < B` every node keeps a list of triplets (IP address, UDP port, ID) for nodes of a distance between `2^i` and `2^(i + 1)` from itself (inclusive of `2^i` and exclusive of `2^(i + 1)`)
These are called k-buckets.

For example consider the example before with the nodes: 1001, 1000, 0011, 0001, 1111 (B = 4).
Hence every node will have 4 (B) k-buckets.

Visually the buckets are the subtrees.

![Buckets Example](/img/kademlia/buckets-example.png)

Each k-bucket is sorted by time last seen with least-recently seen at the head and most recently at the tail.
Remember that in the previous post I said Kademlia is defined by 3 parameters. Bingo! `k` is the 2nd parameter.
`k` is bucket size (in general 20), it is chosen such that any given k nodes are unlikely to fail within an hour.

Therefore any node will have contact with a maximum of `k` nodes from every subtree.
In a way, k-bucket is like a subset of all the nodes in the corresponding subtree.

The node order in a k-bucket according to least-recently used at the head should be maintained at all times (descending order w.r.t time of contact).
When a node receives a message the corresponding k-bucket is updated accordingly.
If the node that sent the message already exists in its corresponding k-bucket it is moved to the tail. 
If the sender node does not exist in the bucket and the bucket is not full then the node is just inserted at the tail.
If the bucket is full then the least recently seen node is pinged in that bucket, 
if that node responds then the sender node is not inserted and that least recently seen node is moved to the tail of the list 
else if the least recently node does not respond, it is evicted and the sender node is inserted at the end.

Why does anyone want to keep the least recently seen node if it is active over a new node?
This is the result of a study which says that the longer a node has been up, the more likely it is to remain up another hour.
By keeping the oldest live contact around, k-buckets maximize the probability that the nodes they contain will remain online.

One nice benefit of k-buckets is that they provide resistance to certain DoS attacks. One cannot flush nodes' routing state by flooding the system with new nodes.
This is because Kademlia only inserts a new node only when old nodes become inactive.

We will talk more about the protocol in the part 3 of this series.

[Kademlia - Part 3](/posts/2019-01-17-kademlia-part-3.html)
