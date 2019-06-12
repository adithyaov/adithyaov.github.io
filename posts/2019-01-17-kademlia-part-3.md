---
title: Kademlia - Part 3
sub: The protocol. continued.
img: kademlia/kademlia.jpg
---

### The Kademlia Protocol

#### Messages

Kademlia has 4 messages,
1. PING - used to verify that a node is still alive
2. STORE - Stores a (key, value) pair in one node
3. FIND\_NODE - The recipient of the request will return the k nodes in his own buckets that are the closest ones to the requested key.
If the recipient has less than k nodes it will return all the nodes it has information about.
4. FIND\_VALUE - Same as FIND\_NODE, but if the recipient of the request has the requested key in its store, it will return the corresponding value.

A node is simply a triplet (IP\_Addr, UDP\_Port, ID).
Each RPC message includes a random value from the initiator. This ensures that when the response is received it corresponds to the request previously sent.
The contents of the random value are opaque objects, ie. they cannot be modified as external elements don't have the concrete data structures to construct that object.
Take a look at [Magic Cookie](https://en.wikipedia.org/wiki/Magic_cookie) for more information.

#### Locating Nodes

Node look-ups is the most important procedure a Kademlia participant must perform. This procedure should return k closest nodes to some given node ID. The number of simultaneous look-ups is denoted by `α`, which is btw the 3rd parameter in Kademlia. `α` is typically 3 in most systems running Kademlia. 


The search begins by selecting α contacts from the non-empty k-bucket closest to the bucket appropriate to the key being searched on. If there are fewer than alpha contacts in that bucket, contacts are selected from other buckets. 

The first α contacts selected create a search set for the search.
The node then sends parallel, asynchronous FIND\_NODE to α contacts in the search set, getting back k triplets from each contact.
The search set is updated with the results of the search keeping the closest k nodes. The node again picks α nodes from the search set and the algorithm continues like before.
The only condition is that the selection of the α nodes is done in such a way that they have not been selected previously.
The iterations continue until no nodes are returned that are closer than the best previous results. When the iterations stop, the best k nodes in the results list are the ones in the whole network that are the closest to the desired key. 

**Note:** The algorithm given in the paper is recursive, the algorithm provided above is iterative. The algorithm in the paper is also unclear, one can tweak this algorithm accordingly.

Most of the operations are implemented using the above look-up procedure.


