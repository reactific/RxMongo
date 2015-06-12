# RxMongo

[![Join the chat at https://gitter.im/reactific/RxMongo](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/reactific/RxMongo?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
A reactive, non-blocking, asynchronous driver for MongoDB 3.0 using ReactiveStreams as implemented by akka-streams.

# Status [![Build Status](https://travis-ci.org/reactific/RxMongo.svg?branch=master)](https://travis-ci.org/reactific/RxMongo)
RxMongo is nearing completion of a working pre-release. Current work centers on making the BSON interface perform
well, simplifying the construction of Codecs, adding a Codec code generator via macros, and adding higher level
interfaces for collections of homogenous and heterogenous documents. The current implementation uses Akka IO not
akka-streams. Conversion to akka-streams will occur once the first akka-streams release candidate is available (April?)

# Quick Start

### Stable: Scala 2.11, SBT 0.13.7, Mongo 3.0

- Not available yet. This is expected in May 2015 unless release of akka-streams is delayed late into April.

### Development: Scala 2.11, SBT 0.13.7, Mongo 3.0

A pre-release version, 0.1-SNAPSHOT, is available on Sonatype Snapshots repository. Use the following SBT
incantations to include it in your project. Please note that while this version works to some degree it is far from
being complete or stable. It is suggested that you wait for 0.1 to be completed before basing any work on it. By then
the basics of the API should be stabilized.

```scala
resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
libraryDependencies := "com.reactific" %% "rxmongo-client" % "0.1.0-SNAPSHOT"
```

Other packages you can access are `rxmongo-driver` (lower level driver interface), `rxmongo-examples` (sample programs
using RxMongo), and `rxmongo-bson` (Binary JSON interface for RxMongo).

# Introduction
RxMongo is similar in purpose to [ReactiveMongo](https://github.com/ReactiveMongo/ReactiveMongo), but not in
implementation. RxMongo has the same aim, however: to provide a highly performant and scalable non-blocking asynchronous
driver in Scala for MongoDB. Some attempts were made to modify ReactiveMongo but when it became apparent that the
desired changes amounted to a re-write, we decided to start over from a fresh slate and let ReactiveMongo inform
our design. RxMongo differs in its approach by emphasizing performance over ease of use and minimizing dependencies.
It utilizes Akka for nearly everything (logging, I/O, streams, actors, configuration, etc.) and is reticent to
depend on other packages.

The goals of RxMongo is to provide the best performing non-blocking, asynchronous Scala driver for MongoDB that is
provably correct and production ready. This will be accomplished by using reactive programming principles, ensuring
that every necessary construct has a full test suite, and requiring commits to pass every test so the project never
regresses.

RxMongo also seeks to maintain a clean and stable API for interacting with MongoDB that matches the documented MongoDB
interface but also makes use of Scala's many features so the API is not tedious to use.

# Important Design Points

### Low Level BSON Interface

Many Scala implementations of BSON use a variety of case classes to model the contents of a bson document and then
serialize or deserialize accordingly. RxMongo takes a different approach. An RxMongo BSONObject simply wraps an
Akka ByteString which is a rope-like data structure that avoids buffer copying. RxMongo provides a builder for
constructing a BSON Object that directly constructs a ByteString with a ByteStringBuilder. At the end, you have
a buffer that is ready to be written to an I/O channel. Similarly, data streams read from the mongod are interpreted
with a BSONDocument which is used as the underlying representation of a BSONObject or BSONArray.  BSONDocument simply
interprets the ByteString data instead of copying it into lots of case classes. Data can be extracted to other
forms, but that is always a choice, not automatic. The goal of all this is to eliminate data copying to and from
the BSON binary format which is one of the key requirements for a driver that performs well.


### Efficient Query Builder DSL
MongoDB suggests the use of various keywords (e.g. `$ne`, `$gte`, `$exists`) in BSONObjects to implement queries and
other features. This is supported directly in RxMongo by adding a DSL layer on top of the BSONBuilder class. This
remains efficient because what is generated directly from the DSL is a BSON binary buffer that can be sent to the
server without further copying.

### Reactive Streams Based Interface

RxMongo is based on [akka-streams](http://doc.akka.io/docs/akka-stream-and-http-experimental/1.0-M2/scala.html) which
is an implementation of [Reactive Streams](http://www.reactive-streams.org/), and not much else. RxMongo uses the
akka-streams internally but also provides akka-stream concepts (Sink, Source, Flow) in its API so that RxMongo can
be included in larger [Flow Graphs](http://doc.akka.io/docs/akka-stream-and-http-experimental/1.0-M2/scala/stream-graphs.html).
For example, an RxMongo Cursor can be obtained as a `Source[BSON.Object]` which can then be processed with any Flow or
connected to a Sink of some sort. Becasue of this, the application writer need not worry about buffering as back
pressure is communicated all the way through and dealt with at the interface to mongod.

### Two API Levels: Driver & Client

RxMongo has two ways to interact with it: Driver and Client. The Driver interface is flexible and highly performant,
while the Client interface is easy to use but has some overheads. Which level you pick depends on the nature of your
application.

In the Driver interface, you ask the Driver object to get you a connection to a MongoDB replica set. This gets you an
ActorRef to a Connection actor that you can use to communicate with the MongoDB server. The Connection actor can take
RequestMessage objects that encapsulate the wire protocol for Mongo. If the request is due a reply, you get back a
ReplyMessage. Creating RequestMessage objects is very efficient and minimizes buffer copies.

In the Client interface, you ask the Client object to open Database and Collection objects. Each of those classes have
methods that allow you to manipulate the databases and collections in your mongo server. These Client API classes
translate your method invocations into the corresponding Driver calls. This part of the API also provides a set of
abstractions for variant collections, named queries, and other utilities to make using Mongo simpler.

### Full Featured for Mongo 3.0
By the 1.0 release, RxMongo will fully support all Mongo 3.0 features and is aimed at the Mongo 3.0 release. Backwards
compatibility with 2.6 may be offered but at a lower priority. Compatibility with versions prior to 2.6 will not be
offered.

# Getting Started

### Working With BSONObject
Mongo uses a data format known as BSON (Binary JSON) which uses a dozen or so basic types to represent nested data
structures. Most often you will use a BSONObject which is often confused with a BSONDocument but documents include
BSONArray as well. RxMongo makes a distinction between these two by having both BSONObject and BSONArray derive from
BSONDocument. BSONDocuments are, essentially, a `Map[String,BSONValue]`. BSONObjects can be constructed quite simply,
however, since their constructor accepts a `Map[String,Any]` and does the corresponding translations from Any to
BSONValue. This works for most typical data types. Where you need a specialized translator, you can write a Codec
(see below).

```scala
val BSONObj = BSONObject(
    "double" -> 42.0D,
    "string" -> "fourty-two",
    "obj" -> Helper.anObject,
    "array" -> Helper.anArray,
    "map" -> map,
    "binary" -> Helper.data,
    "undefined" -> null,
    "boolean" -> true,
    "date" -> date,
    "regex" -> regex,
    "integer" -> 42,
    "long" -> 42L
)
```

### Connecting With RxMongo Client

To make a high-level connection to MongoDB, use the Client interface. The code below shows how to create the
client [1], obtain a database from the client [2], obtain a collection from the database [3], query the collection [4],
obtain the results [5], and handle query errors [6].

```scala
package rxmongo.examples

import rxmongo.bson.{RxMongoError, BSONObject}
import rxmongo.client.Client
import scala.concurrent.ExecutionContext.Implicits.global

object ClientBasics extends App {

  // [1] Make a connection to the mydb database on the local host
  val client = Client("mongodb://localhost/mydb")

  // [2] Obtain a database from the client
  val db = client.database("mydb")

  // [3] Obtain a collection from the database
  val coll = db.collection("mycoll")

  // [4] Query the collection for documents with the name "foo"
  coll.query("name" -> "foo") map {
    case results: Seq[BSONObject] =>
      // [5] We got an answer to our query, print the results
      println("Results: " + results)
  } recover {
    case xcptn: RxMongoError =>
      // [6] We got an error in our query, print the error message
      println("Error in query: " + xcptn)
  }
}
```

### Connecting With RxMongo Driver

Alternatively, you can make a low-level connection to MongoDB via the Driver interface. The code below shows how to
instantiate the driver[1], connect to a MongoDB instance using a Mongodb URL [2], create a query message [3], send a
query and get its response in a non-blocking fashion [4], and decode the reply from MongoDB [5].

```scala

package rxmongo.examples

import akka.actor.ActorRef
import akka.pattern.ask

import com.typesafe.config.ConfigFactory

import rxmongo.driver._
import rxmongo.bson._

import scala.concurrent.ExecutionContext.Implicits.global

/** Basic Driver Example
  *
  * This example shows how to instantiate the driver[1], connect to a MongoDB instance using a
  * Mongodb URL [2], create a query message [3], send a query and get its response in a non-blocking
  * fashion [4], and decode the reply from MongoDB [5].
  */
object DriverBasics extends App {

  // You can specify your own timeout for asynchronous calls instead of using this default value from Driver.
  implicit val timeout = Driver.defaultTimeout

  // [1] Instantiate with a specific configuration and a specific name for the driver
  val driver = Driver(ConfigFactory.load("rxmongo"), "MyDriver")

  // [2] Make a connection to database "mydb" on the local host at the default port and set the
  // maxPoolSize configuration value to 19 so at most 19 channels to the server will be open. This
  // will yield a Future[ActorRef] in "conn". If the connection succeeds, conn will have a value
  // that can be used to talk to the members of a MongoDB replica set.
  val conn = driver.connect("mongodb://localhost/mydb?maxPoolSize=19")

  // [3] Create a query message to find the documents with the name "foo" in the mydb.mycoll namespace.
  // You can also create all of the other kinds of messages that MongoDB supports.
  val msg = QueryMessage("mydb.mycoll", numberToSkip=0, numberToReturn=1, BSONObject("name" -> "foo"))

  // [4] When the connection is successful, extract the ActorRef and send it the query using the ask pattern
  // which will asynchronously deliver a ReplyMessage when the response comes back from MongoDB.
  conn.map {
    case connection: ActorRef =>
      connection.ask(msg).map {
        case reply: ReplyMessage =>
          // [5] We got a reply from MongoDB, now we need to decipher it. The numberReturned field tells us how
          // many documents were returned by the query. If we got at least one, just print out the head document.
          if (reply.numberReturned > 0) {
            println("Document returned: " + reply.documents.head)
          } else if (reply.QueryFailure) {
            // In this case, there was something wrong with the query.
            println("Query Failed")
          } else {
            // Chances are if you run this program, you will get this output because you don't have a
            // database named "mydb.mycoll" and if you do, it probably doesn't have a document whose
            // name field is "foo".
            println("No results: " + reply)
          }
      } recover {
        case xcptn: Throwable =>
          // If the query fails for any reason, you can recover from it here.
          println("Error from MongoDB: " + xcptn)
      }
  } recover {
    case xcptn: Throwable =>
      // If the connection fails for any reason, you can recover from it here.
      println("Could not connect to MongoDB: " + xcptn)
  }
}

```

### Writing A Codec

A Codec is a small object that can encode and decode values into and out of a ByteString. This allows all manner of
Scala objects to be placed into a byte buffer with the conversions handled implicitly.

*example TBD*
