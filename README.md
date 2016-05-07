# transducer-collections
A proof-of-concept collections library powered by transducers

## What is a transducer?

A **transducer** is a pattern for describing modifications to a collection or stream, like map, filter, etc. Unlike
traditional collection operations, the type of the collection does not need to be known at construction time. And, the
modifications are lazy; they can run on demand and be reused between different collections.

## See also

https://github.com/knutwalker/transducers-scala
