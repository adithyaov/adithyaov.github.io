---
title: Kademlia - Part 1
sub: A few simple definitions.
img: kademlia/kademlia.jpg
---

We try to understand Kademlia in detail.
This series of blog posts will have a title prefixed with `Kademlia -`.
This is going to be a long series so buckle up!

The Paper referred for the following article is [Kademlia: A Peer-to-peer Information System Based on the XOR Metric](https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf)

### What is Kademlia?
In simple words, Kademlia is a distributed hash table. For the people who don't understand the terms well
lets break them down. A hash table is similar to a python dictionary where in you give in a `key` and get back a `value`.
Distributed means that the entire hash table is not on a single node (any computing device/server) but is distributed on multiple nodes.

### How Kademlia is defined?
Now that we have got a very simple idea of what Kademlia is, lets move on to the structure and understand why it is used.
Kademlia is defined by 3 parameters but for the following post we care about only 1 parameter, namely `B`.
We assign a `B` bit id to every node on the network. In general `B = 160` where `B` is the SHA1 hash.
From here on every time you see the word 'node', implicitly assume a `B` bit ID associated with it.

We can treat the nodes as the leaves of a binary tree and each node's position is determined by the shortest unique prefix.
As of now don't worry about what happens when an additional node enters the network. For now, just assume that the network consists
of nodes in the binary tree where the position is determined by their shortest unique prefix. Different nodes can have different length
of their prefix considered for their position in the binary tree.

For example say we have 5 nodes with `B = 4` (Note that the maximum possible nodes for this `B` is 16),

- 1001 (position at tree = 1001)
- 1000 (position at tree = 1000)
- 0011 (position at tree = 001)
- 0001 (position at tree = 000)
- 1111 (position at tree = 11)

The tree will look like the following (I apologize for the art)

![Sample Tree](/img/kademlia/sample-tree.png)

For any node we divide the binary tree into a series of successively lower subtrees. 
The highest subtree consists of half of the binary tree that does not contain that node.
The next subtree consists of half of the remaining tree not containing the node and so on.
The following image will give the proper idea.

The following are the series of subtrees of associated with the node 1000:

![Subtrees of node 1000](/img/kademlia/subtrees-of-1000.png)

The Kademlia protocol ensures that every node knows at least 1 node in the subtrees associated with it.
Any node is hence able to locate any other node with just the ID. We will see how this happens in the next post.

[Kademlia - Part 2](/posts/2019-01-15-kademlia-part-2.html)
