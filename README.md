# RxMongo
A reactive, non-blocking, asynchronous driver for MongoDB 2.8 with ReactiveStreams and Akka.

# Quick Start

### Stable: Scala 2.11, SBT 0.13.7, Mongo 2.6

- Not available yet.

### Development: Scala 2.11, SBT 0.13.7, Mongo 2.8

Will be available shortly with something like this:

```scala
resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
libraryDependencies := "org.rxmongo" %% "rxmongo-client" % "0.1.0-SNAPSHOT"
```

# Status
RxMongo is just getting started. Developers and testers are needed. If you want to help, please contact Reid.

# Introduction
RxMongo is a rewrite of [ReactiveMongo](https://github.com/ReactiveMongo/ReactiveMongo) that was necessitated when
that project fell into disrepair due to a lack of maintenance. Several design initiatives were suggested and requested
for ReactiveMongo but there was no response from that project's owners. Consequently, it was decided that this type of
driver for Mongo was too important to be allowed to dwindle so RxMongo was started to keep up to date with Mongo's
developments and changes in the Scalascape.

The goals of RxMongo is to provide best performing non-blocking, asynchronous, Scala driver for MongoDB that is
provably correct and production ready. This will be accomplished by using reactive programming principles, ensuring
that every necessary construct has a full test suite, and requiring commits to pass every test so the project never
regresses unnecessarily.

RxMongo also seeks to maintain a clean clean and stable API for interacting with MongoDB that matches the MongoDB
interface but also makes use of Scala's many features so the API is not tedious to use.

# Important Design Distinctions

### Low Level BSON Interface

Many Scala implementations of BSON use a variety of case classes to model the contents of a bson document and then
serialize or deserialize accordingly. RxMongo takes a differnet approach. BSON documents simply wrap a NIO ByteBuffer
so that serialization and deserialization is unnecessary. This eliminates one translation step to and from the
wire. When we get a BSON document as a byte array from MongoD, that byte array is wrapped in a BSON Document without
modification. The BSON Document class takes care of interpreting those bytes correctly.

### Reactive Streams Based Interface

RxMongo is based on akka-streams and akka-actor and not much else. RxMongo uses the constructs of Reactive Streams
(akka-streams) internally and also provides them through its API. For example, an RxMongo Cursor can be obtained as
a `Source[BSON.Document]` which can then be processed with any Flow or connected to a Sink of some sort. Becasue of this,
the application writer need not worry about buffering as back pressure is communicated all the way through and dealt
with at the interface to mongod.

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
