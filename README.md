# RxMongo
A reactive, non-blocking, asynchronous driver for MongoDB 2.8 with ReactiveStreams and Akka.

# Status [![Build Status](https://travis-ci.org/reactific/RxMongo.svg?branch=master)](https://travis-ci.org/reactific/RxMongo)
RxMongo is just getting started. Developers and testers are needed. If you want to help, please contact Reid.

# Quick Start

### Stable: Scala 2.11, SBT 0.13.7, Mongo 2.6

- Not available yet.

### Development: Scala 2.11, SBT 0.13.7, Mongo 2.8

Will be available in February 2015 with something like this:

```scala
resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
libraryDependencies := "org.rxmongo" %% "rxmongo-client" % "0.1.0-SNAPSHOT"
```

# Introduction
RxMongo is similar in purpose to [ReactiveMongo](https://github.com/ReactiveMongo/ReactiveMongo), but not in
implementation. RxMongo has the same aim, however: to provide a highly performant and scalable non-blocking asynchronous
driver in Scala for MongoDB. Some attempts were made to modify ReactiveMongo but when pull requests were ignored and
incremental modification got too difficult, we decided to start over from a fresh slate and let ReactiveMongo inform
our design. RxMongo differs in its approach by emphasizing performance over ease of use and minimizing dependencies.
It utilizes Akka for nearly everything (logging, I/O, streams, actors, configuration, etc.) and is reticent to
depend on other packages.

The goals of RxMongo is to provide best performing non-blocking, asynchronous, Scala driver for MongoDB that is
provably correct and production ready. This will be accomplished by using reactive programming principles, ensuring
that every necessary construct has a full test suite, and requiring commits to pass every test so the project never
regresses unnecessarily.

RxMongo also seeks to maintain a clean clean and stable API for interacting with MongoDB that matches the MongoDB
interface but also makes use of Scala's many features so the API is not tedious to use.

# Important Design Points

### Low Level BSON Interface

Many Scala implementations of BSON use a variety of case classes to model the contents of a bson document and then
serialize or deserialize accordingly. RxMongo takes a different approach. An RxMongo BSON Object simply wrap an
Akka ByteString which is a rope-like data structure that avoids buffer copying. RxMongo provides a builder for
constructing a BSON Object that directly constructs a ByteString with a ByteStringBuilder. At the end, you have
a buffer that is ready to be written to an I/O channel. Similarly, data streams read from the mongod will be
retained in place and BSON Object simply interprets that data instead of copying it into lots of case classes.
The goal of all this is to eliminate data copying to and from the BSON binary format which is one of the key
elements of a driver that performs well.

### Reactive Streams Based Interface

RxMongo is based on [akka-streams](http://doc.akka.io/docs/akka-stream-and-http-experimental/1.0-M2/scala.html) which
is an implementation of [Reactive Streams](http://www.reactive-streams.org/), and not much else. RxMongo uses the
akka-streams internally but also provides akka-stream concepts (Sink, Source, Flow) in its API so that RxMongo can
be included in larger [Flow Graphs](http://doc.akka.io/docs/akka-stream-and-http-experimental/1.0-M2/scala/stream-graphs.html).
For example, an RxMongo Cursor can be obtained as a `Source[BSON.Object]` which can then be processed with any Flow or
connected to a Sink of some sort. Becasue of this, the application writer need not worry about buffering as back
pressure is communicated all the way through and dealt with at the interface to mongod.


# Getting Started

### RxMongoClient
TBD

### BSON.Document
TBD

### rxmongo.client.Database
TBD

### rxmongo.client.Collection
TBD

### rxmongo.client.VariantCollection
TBD
