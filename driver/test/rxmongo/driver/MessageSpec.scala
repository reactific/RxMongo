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

import akka.util.ByteString
import org.specs2.mutable.Specification
import rxmongo.bson._

class MessageSpec extends Specification {

  val selector: BSONObject = BSONObject("item" -> BSONString("MNO2"))
  val update: BSONObject = BSONObject("$set" -> BSONObject("category" -> BSONString("apparel")))

  "Message" should {
    "produce correct buffer for OP_UPDATE" in {
      val msg = UpdateMessage("db.coll", selector, update, upsert = true, multiUpdate = true)

      // struct OP_UPDATE {
      //   MsgHeader header;             // standard message header
      //   int32     ZERO;               // 0 - reserved for future use
      //   cstring   fullCollectionName; // "dbname.collectionname"
      //   int32     flags;              // bit vector. see below
      //   document  selector;           // the query to select the document
      //   document  update;             // specification of the update to perform
      // }

      val expected: ByteString = {
        val b = ByteString.newBuilder
        b.putInt(0)
        b.putCStr("db.coll")
        b.putInt(3)
        b.putDoc(selector)
        b.putDoc(update)
        val payload = b.result()
        val c = ByteString.newBuilder
        c.putInt(payload.length)
        c.putInt(msg.requestId)
        c.putInt(0)
        c.putInt(Message.OpCode.OP_UPDATE.id)
        c ++= payload
        c.result()
      }

      val buffer = msg.finish
      buffer must beEqualTo(expected)
    }

    "produce correct buffer for OP_INSERT" in {
      val documents: Seq[BSONObject] = Seq(selector, update)
      val msg = InsertMessage("db.coll", documents, continueOnError = true)

      // struct {
      //   MsgHeader header;             // standard message header
      //   int32     flags;              // bit vector - see below
      //   cstring   fullCollectionName; // "dbname.collectionname"
      //   document* documents;          // one or more documents to insert into the collection
      // }

      val expected: ByteString = {
        val b = ByteString.newBuilder
        b.putInt(1)
        b.putCStr("db.coll")
        b.putDoc(selector)
        b.putDoc(update)
        val payload = b.result()
        val c = ByteString.newBuilder
        c.putInt(payload.length)
        c.putInt(msg.requestId)
        c.putInt(0)
        c.putInt(Message.OpCode.OP_INSERT.id)
        c ++= payload
        c.result()
      }
      val buffer = msg.finish
      buffer must beEqualTo(expected)
    }

    "produce correct buffer for OP_QUERY" in {

      val msg = QueryMessage(
        "db.coll", 0, 1, selector, None, tailableCursor = true, slaveOk = true,
        noCursorTimeout = true, awaitData = true, exhaust = true, partial = true)

      // struct OP_QUERY {
      //   MsgHeader header;                   // standard message header
      //   int32     flags;                    // bit vector of query options.  See below for details.
      //   cstring   fullCollectionName ;      // "dbname.collectionname"
      //   int32     numberToSkip;             // number of documents to skip
      //   int32     numberToReturn;           // number of documents to return in the first OP_REPLY batch
      //   document  query;                    // query object.
      //   [ document  returnFieldsSelector; ] // Optional. Selector indicating the fields to return.
      // }

      val expected: ByteString = {
        val b = ByteString.newBuilder
        b.putInt(2 + 4 + 16 + 32 + 64 + 128)
        b.putCStr("db.coll")
        b.putInt(0)
        b.putInt(1)
        b.putDoc(selector)
        val payload = b.result()
        val c = ByteString.newBuilder
        c.putInt(payload.length)
        c.putInt(msg.requestId)
        c.putInt(0)
        c.putInt(Message.OpCode.OP_QUERY.id)
        c ++= payload
        c.result()
      }
      val buffer = msg.finish
      buffer must beEqualTo(expected)
    }

    "produce correct buffer for OP_GET_MORE" in {
      val replyBuff: ByteString = {
        // struct {
        //   MsgHeader header;         // standard message header
        //   int32     responseFlags;  // bit vector - see details below
        //   int64     cursorID;       // cursor id if client needs to do get more's
        //   int32     startingFrom;   // where in the cursor this reply is starting
        //   int32     numberReturned; // number of documents in the reply
        //   document* documents;      // documents
        // }
        val b = ByteString.newBuilder
        b.putInt(0)
        b.putLong(23L)
        b.putInt(0)
        b.putInt(0)
        b.result()
      }

      val replyMessage = ReplyMessage(replyBuff)
      val msg = GetMoreMessage("db.coll", 1, replyMessage)

      // struct {
      //   MsgHeader header;             // standard message header
      //   int32     ZERO;               // 0 - reserved for future use
      //   cstring   fullCollectionName; // "dbname.collectionname"
      //   int32     numberToReturn;     // number of documents to return
      //   int64     cursorID;           // cursorID from the OP_REPLY
      // }
      val expected: ByteString = {
        val b = ByteString.newBuilder
        b.putInt(0)
        b.putCStr("db.coll")
        b.putInt(1)
        b.putLong(23)
        val payload = b.result()
        val c = ByteString.newBuilder
        c.putInt(payload.length)
        c.putInt(msg.requestId)
        c.putInt(0)
        c.putInt(Message.OpCode.OP_GET_MORE.id)
        c ++= payload
        c.result()
      }
      val buffer = msg.finish
      buffer must beEqualTo(expected)
    }

    "produce correct buffer for OP_DELETE" in {

      val msg = DeleteMessage("db.coll", selector, singleRemove = true)

      // struct {
      //   MsgHeader header;             // standard message header
      //   int32     ZERO;               // 0 - reserved for future use
      //   cstring   fullCollectionName; // "dbname.collectionname"
      //   int32     flags;              // bit vector - see below for details.
      //   document  selector;           // query object.  See below for details.
      // }

      val expected: ByteString = {
        val b = ByteString.newBuilder
        b.putInt(0)
        b.putCStr("db.coll")
        b.putInt(1)
        b.putDoc(selector)
        val payload = b.result()
        val c = ByteString.newBuilder
        c.putInt(payload.length)
        c.putInt(msg.requestId)
        c.putInt(0)
        c.putInt(Message.OpCode.OP_DELETE.id)
        c ++= payload
        c.result()
      }
      val buffer = msg.finish
      buffer must beEqualTo(expected)
    }

    "produce correct buffer for OP_KILL_CURSORS" in {
      val msg = KillCursorsMessage(Seq(1, 2, 3))

      // struct {
      //   MsgHeader header;            // standard message header
      //   int32     ZERO;              // 0 - reserved for future use
      //   int32     numberOfCursorIDs; // number of cursorIDs in message
      //   int64*    cursorIDs;         // sequence of cursorIDs to close
      // }

      val expected: ByteString = {
        val b = ByteString.newBuilder
        b.putInt(0)
        b.putInt(3)
        b.putLong(1)
        b.putLong(2)
        b.putLong(3)
        val payload = b.result()
        val c = ByteString.newBuilder
        c.putInt(payload.length)
        c.putInt(msg.requestId)
        c.putInt(0)
        c.putInt(Message.OpCode.OP_KILL_CURSORS.id)
        c ++= payload
        c.result()
      }
      val buffer = msg.finish
      buffer must beEqualTo(expected)
    }
  }
}
