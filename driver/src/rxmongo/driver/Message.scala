/*
 * Copyright © 2015 Reactific Software LLC. All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package rxmongo.driver

import java.util.concurrent.atomic.AtomicInteger

import akka.util.{ ByteStringBuilder, ByteString }
import rxmongo.bson._
import rxmongo.driver.Message.OP_NOT_A_MESSAGE

object Message {

  /** An enumeration of the op codes that Mongo defines for its messages. */
  sealed trait OpCode { val id : Int }
  case object OP_NOT_A_MESSAGE extends OpCode { val id = 0 }
  case object OP_REPLY extends OpCode { val id = 1 }
  case object OP_MSG extends OpCode { val id = 1000 }
  case object OP_UPDATE extends OpCode { val id = 2001 }
  case object OP_INSERT extends OpCode { val id = 2002 }
  case object OP_RESERVED extends OpCode { val id = 2003 }
  case object OP_QUERY extends OpCode { val id = 2004 }
  case object OP_GET_MORE extends OpCode { val id = 2005 }
  case object OP_DELETE extends OpCode { val id = 2006 }
  case object OP_KILL_CURSORS extends OpCode { val id = 2007 }

  private val _requestId = new AtomicInteger(0)
  def nextRequestId : Int = _requestId.incrementAndGet()

  val Upsert_Flag = 1
  val MultiUpdate_Flag = 2
}

trait Message {
  val opcode : Message.OpCode
  def build : ByteStringBuilder = ByteString.newBuilder
  /** {{{
    * struct MsgHeader {
    * int32   messageLength; // total message size, including this
    * int32   requestID;     // identifier for this message
    * int32   responseTo;    // requestID from the original request (used in reponses from db)
    * int32   opCode;        // request type - see table below
    * }
    * }}}
    * @return
    */
  lazy val requestId = Message.nextRequestId
  lazy val finish : ByteString = {
    val content = build.result()
    val header = ByteString.newBuilder
    header.putInt(content.length + 16)
    header.putInt(requestId)
    header.putInt(0)
    header.putInt(opcode.id)
    header ++= content
    header.result()
  }
}

/** MessageHeader
  *
  * This header is standard for all messages sent to MongoDB and so it is the base class of all mongo messages.
  */
class RequestMessage(val opcode : Message.OpCode) extends Message {
  def requiresResponse : Boolean = false
}

/** The Mongo Update Message
  * The OP_UPDATE message is used to update a document in a collection.
  * @param fullCollectionName The full name of the collection, including database name, like "dbName.collectionName".
  * The full collection name is the concatenation of the database name with the collection
  * name, using a . for the concatenation. For example, for the database foo and the
  * collection bar, the full collection name is foo.bar.
  * @param selector The document selector used to match which documents will be updated
  * @param update The update to apply to the selected documents
  * @param upsert If set, the database will insert the supplied object into the collection if no matching
  * document is found
  * @param multiUpdate if set, the database will update all matching objects in the collection, not just the first one.
  */
case class UpdateMessage(
  fullCollectionName : String,
  selector : BSONObject,
  update : BSONObject,
  upsert : Boolean = false,
  multiUpdate : Boolean = false) extends RequestMessage(Message.OP_UPDATE) {

  val flags : Int = 0 |
    (if (upsert) 1 else 0) |
    (if (multiUpdate) 2 else 0)

  // struct OP_UPDATE {
  //   MsgHeader header;             // standard message header
  //   int32     ZERO;               // 0 - reserved for future use
  //   cstring   fullCollectionName; // "dbname.collectionname"
  //   int32     flags;              // bit vector. see below
  //   document  selector;           // the query to select the document
  //   document  update;             // specification of the update to perform
  // }
  override def build = super.build.
    putInt(0).
    putCStr(fullCollectionName).
    putInt(flags).
    putDoc(selector).
    putDoc(update)
}

/** The Mongo Insert Message
  * The OP_INSERT message is used to insert one or more documents into a collection.
  * @param fullCollectionName The full name of the collection, including database name, like "dbName.collectionName".
  * The full collection name is the concatenation of the database name with the collection
  * name, using a . for the concatenation. For example, for the database foo and the
  * collection bar, the full collection name is foo.bar.
  * @param documents A sequence of documents to be inserted into the collection
  * @param continueOnError If set, the database will not stop processing a bulk insert if one fails (eg due to
  * duplicate IDs). This makes bulk insert behave similarly to a series of single inserts,
  * except lastError will be set if any insert fails, not just the last one. If multiple
  * errors occur, only the most recent will be reported by getLastError. (new in 1.9.1)
  */
case class InsertMessage(
  fullCollectionName : String,
  documents : Seq[BSONObject],
  continueOnError : Boolean = false) extends RequestMessage(Message.OP_INSERT) {

  val flags : Int = 0 |
    (if (continueOnError) 1 else 0)

  // struct {
  //   MsgHeader header;             // standard message header
  //   int32     flags;              // bit vector
  //   cstring   fullCollectionName; // "dbname.collectionname"
  //   document* documents;          // one or more documents to insert into the collection
  // }
  override def build = super.build.
    putInt(flags). // bit vector - see arguments
    putCStr(fullCollectionName). // "dbname.collectionname"
    putDocs(documents) // one or more documents to insert into the collection
}

/** Base class of the QueryMessage and Command
  *
  */
abstract class GenericQueryMessage extends RequestMessage(Message.OP_QUERY) {

  /** The fully qualified collection name: <database> . <collection>    */
  val fullCollectionName : String

  /** The document selector which selects which documents will be returned */
  val selector : BSONObject

  /** The document projection which determines which document fields will be returned */
  val returnFieldsSelector : Option[BSONObject]

  /** Options that control the cursor and other aspects of the result set */
  val options : QueryOptions

  // struct OP_QUERY {
  //   MsgHeader header;                   // standard message header
  //   int32     flags;                    // bit vector of query options.  See below for details.
  //   cstring   fullCollectionName ;      // "dbname.collectionname"
  //   int32     numberToSkip;             // number of documents to skip
  //   int32     numberToReturn;           // number of documents to return in the first OP_REPLY batch
  //   document  query;                    // query object.
  //   [ document  returnFieldsSelector; ] // Optional. Selector indicating the fields to return.
  // }
  override def build = {
    val bs = super.build
    options.writeToByteString(bs, fullCollectionName)
    bs.putDoc(selector).putDoc(returnFieldsSelector)
  }

  override val requiresResponse : Boolean = true
}

/** The Mongo Query Message
  * The OP_QUERY message is used to query the database for documents in a collection.
  * @param fullCollectionName The full name of the collection, including database name, like "dbName.collectionName".
  * The full collection name is the concatenation of the database name with the collection
  * name, using a . for the concatenation. For example, for the database foo and the
  * collection bar, the full collection name is foo.bar.
  * @param selector The query object. BSON document that represents the query. The query will contain one or more
  * elements, all of which must match for a document to be included in the result set. Possible elements
  * include $query, $orderby, $hint, $explain, and $snapshot.
  * @param returnFieldsSelector Optional. Selector indicating the fields to return. BSON document that limits the
  *   fields in the returned documents. The returnFieldsSelector contains one or more
  *   elements, each of which is the name of a field that should be returned, and and the
  *   integer value 1. In JSON notation, a returnFieldsSelector to limit to the fields
  *   a, b and c would be: `{ a : 1, b : 1, c : 1}`.
  * @param options The options for the query. See [[rxmongo.driver.QueryOptions]].
  *
  * The database will respond to an OP_QUERY message with an OP_REPLY message.
  */
case class QueryMessage(
  fullCollectionName : String,
  selector : BSONObject,
  returnFieldsSelector : Option[BSONObject] = None,
  options : QueryOptions = QueryOptions()) extends GenericQueryMessage

/** The Mongo GET MORE Message
  * The OP_GET_MORE message is used to query the database for documents in a collection. T
  * @param fullCollectionName The full name of the collection, including database name, like "dbName.collectionName".
  * The full collection name is the concatenation of the database name with the collection
  * name, using a . for the concatenation. For example, for the database foo and the
  * collection bar, the full collection name is foo.bar.
  * @param numberToReturn Limits the number of documents in the first OP_REPLY message to the query. However, the
  * database will still establish a cursor and return the cursorID to the client if there are
  * more results than numberToReturn. If the client driver offers ‘limit’ functionality (like
  * the SQL LIMIT keyword), then it is up to the client driver to ensure that no more than the
  * specified number of document are returned to the calling application. If numberToReturn is 0,
  * the db will used the default return size.
  * @param forReply The ReplyMessage for an OP_QUERY to which this GetMoreMessage is requesting more data. This is used
  * instead of a cursorID value so that it is not possible to mix up the cursorID. This means the
  * original reply message will stick around while we're still getting more messages. We're okay with
  * that.
  */
case class GetMoreMessage(
  fullCollectionName : String,
  numberToReturn : Int,
  forReply : ReplyMessage) extends RequestMessage(Message.OP_GET_MORE) {

  // struct {
  //   MsgHeader header;             // standard message header
  //   int32     ZERO;               // 0 - reserved for future use
  //   cstring   fullCollectionName; // "dbname.collectionname"
  //   int32     numberToReturn;     // number of documents to return
  //   int64     cursorID;           // cursorID from the OP_REPLY
  // }
  override def build = super.build.
    putInt(0).
    putCStr(fullCollectionName).
    putInt(numberToReturn).
    putLong(forReply.cursorID)

}

/** Mongo Delete Message
  * The OP_DELETE message is used to remove one or more documents from a collection.
  *
  * @param fullCollectionName The full name of the collection, including database name, like "dbName.collectionName".
  * The full collection name is the concatenation of the database name with the collection
  * name, using a . for the concatenation. For example, for the database foo and the
  * collection bar, the full collection name is foo.bar.
  * @param selector BSON document that represent the query used to select the documents to be removed. The selector
  * will contain one or more elements, all of which must match for a document to be removed from the
  * collection.
  * @param singleRemove If set, the database will remove only the first matching document in the collection.
  * Otherwise all matching documents will be removed.
  */

case class DeleteMessage(
  fullCollectionName : String,
  selector : BSONObject,
  singleRemove : Boolean = false) extends RequestMessage(Message.OP_DELETE) {

  /** {{{
    * bit 	  name	        description
    * 0	  SingleRemove	If set, the database will remove only the first matching document in the collection.
    * Otherwise all matching documents will be removed.
    * 1-31	  Reserved	    Must be set to 0.
    * }}}
    */
  val flags = 0 |
    (if (singleRemove) 1 else 0)

  // struct {
  //   MsgHeader header;             // standard message header
  //   int32     ZERO;               // 0 - reserved for future use
  //   cstring   fullCollectionName; // "dbname.collectionname"
  //   int32     flags;              // bit vector - see below for details.
  //   document  selector;           // query object.  See below for details.
  // }
  override def build = super.build.
    putInt(0).
    putCStr(fullCollectionName).
    putInt(flags).
    putDoc(selector)
}

/** Mongo Kill Cursors Message
  *
  * The OP_KILL_CURSORS message is used to close an active cursor in the database. This is necessary to ensure
  * that database resources are reclaimed at the end of the query. If a cursor is read until exhausted (read until
  * OP_QUERY or OP_GET_MORE returns zero for the cursor id), there is no need to kill the cursor.
  *
  * @param cursorIDs The cursorIDs to be removed from the mongo server (i.e. we're done fetching results for them)
  */
case class KillCursorsMessage(
  cursorIDs : Seq[Long]) extends RequestMessage(Message.OP_KILL_CURSORS) {

  // struct {
  //   MsgHeader header;            // standard message header
  //   int32     ZERO;              // 0 - reserved for future use
  //   int32     numberOfCursorIDs; // number of cursorIDs in message
  //   int64*    cursorIDs;         // sequence of cursorIDs to close
  // }
  override def build = super.build.
    putInt(0).
    putInt(cursorIDs.length).
    putLongs(cursorIDs.toArray)

}

/** Mongo Reply Message
  * The OP_REPLY message is sent by the database in response to an OP_QUERY or OP_GET_MORE message.
  * @param buffer The ByteString buffer received from mongo from which the values for this object are extracted
  */
case class ReplyMessage private[driver] (private val buffer : ByteString) extends Message {
  val opcode = Message.OP_REPLY

  /** {{{
    * struct {
    * MsgHeader header;         // standard message header
    * int32     responseFlags;  // bit vector - see details below
    * int64     cursorID;       // cursor id if client needs to do get more's
    * int32     startingFrom;   // where in the cursor this reply is starting
    * int32     numberReturned; // number of documents in the reply
    * document* documents;      // documents
    * }
    * }}}
    */
  val itr = buffer.iterator

  /** {{{
    * struct MsgHeader {
    * int32   messageLength; // total message size, including this
    * int32   requestID;     // identifier for this message
    * int32   responseTo;    // requestID from the original request (used in reponses from db)
    * int32   opCode;        // request type - see table below
    * }
    * }}}
    * @return
    */

  val messageLength = itr.getInt
  val requestID = itr.getInt
  val responseTo = itr.getInt
  val opCode = itr.getInt

  /** {{{
    * bit   name	description
    * 0	 CursorNotFound	  Set when getMore is called but the cursor id is not valid at the server. Returned with zero results.
    * 1  QueryFailure	    Set when query failed. Results consist of one document containing an “$err” field describing the failure.
    * 2  ShardConfigStale	Drivers should ignore this. Only mongos will ever see this set, in which case, it needs to update config from the server.
    * 3  AwaitCapable     Set when the server supports the AwaitData Query option. If it doesn’t, a client should sleep a little between getMore’s of a Tailable cursor. Mongod version 1.6 supports AwaitData and thus always sets AwaitCapable.
    * 4-31	 Reserved	    Ignore
    * }}}
    */
  val responseFlags = itr.getInt
  val CursorNotFound : Boolean = (responseFlags & 1) == 1
  val QueryFailure : Boolean = (responseFlags & 2) == 2
  val ShardConfigStale : Boolean = (responseFlags & 4) == 4
  val AwaitCapable : Boolean = (responseFlags & 8) == 8

  /** The cursorID that this OP_REPLY is a part of. In the event that the result set of the query fits into one
    * OP_REPLY message, cursorID will be 0. This cursorID must be used in any OP_GET_MORE messages used to get more
    * data, and also must be closed by the client when no longer needed via a OP_KILL_CURSORS message.
    */
  val cursorID = itr.getLong
  val startingFrom = itr.getInt
  val numberReturned = itr.getInt
  val documents = itr.getObjs(numberReturned)

  override def build = throw new IllegalStateException("Attempt to build a ReplyMessage")
  override lazy val finish = buffer
  override def toString() : String = {
    val b = new StringBuilder
    b.append("ReplyMessage(messageLength=").append(messageLength).append(", requestID=").append(requestID).
      append(", responseTo=").append(responseTo).append(", opCode=").append(opCode).append(", CursorNotFound=").
      append(CursorNotFound).append(", QueryFailure=").append(QueryFailure).append(", ShardConfigStale=").
      append(ShardConfigStale).append(", AwaitCapable=").append(AwaitCapable).append(", cursorID=").append(cursorID).
      append(", startingFrom=").append(startingFrom).append(", numberReturned=").append(numberReturned).
      append(", documents={ ")
    for (doc ← documents) { b.append(doc.toString()).append(", ") }
    b.setLength(b.length - 2)
    b.append(" }")
    b.toString()
  }

  def error : Option[String] = {
    if (QueryFailure && documents.nonEmpty) {
      documents.head.getOptionalString("$err")
    } else {
      None
    }
  }
}

/** A Psuedo Messsage For Batching Messages Together
  *
  * This allows a batch of distinct messages to be grouped together and sent in a single write to the MongoDB
  * server. This is not a valid Mongo message so it doesn't increment the message id counter nor have its own
  * message header. It simply bundles the contained messages together so they can be delivered in a single
  * write. The contained messages will each have their own reply, if they warrant one.
  * @param msgs The batch of messages
  */
case class MessageBatch private[driver] (private val msgs : Seq[Message]) extends Message {
  val opcode = OP_NOT_A_MESSAGE
  override def build : ByteStringBuilder = {
    msgs.foldLeft(ByteString.newBuilder) { (x, m) ⇒ x.append(m.finish) }
  }
  override lazy val finish = build.result()

}
