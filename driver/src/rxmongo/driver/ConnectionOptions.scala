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

import java.net.InetAddress

/** Options for connecting to a Mongo Replica Set.
  *
  * When a connection to a replica set is made, several options are available to control the aspects of establishing
  * and maintaining the connection. Since replica set nodes can come and go, these options allow users to decide
  * how hard they want RxMongo to try to maintain a connection to a replica set.
  *
  * @param replicaSet Specifies the name of the replica set, if the mongod is a member of a replica set. When
  *              connecting to a replica set it is important to give a seed list of at least two mongod instances.
  *              If you only provide the connection point of a single mongod instance, and omit the replicaSet,
  *              the client will create a standalone connection.
  * @param ssl When true: Initiate the connection with SSL. When false: Initiate the connection without SSL. The
  *       default value is false.
  * @param connectTimeoutMS The time in milliseconds to attempt a connection before timing out. The default (0) is never
  *                    to timeout, unless the underlying TCP implementation times out.
  * @param socketTimeoutMS The time in milliseconds to attempt a send or receive on a socket before the attempt times
  *                   out. The default (0) is never to timeout, unless the underlying TCP implementation times out.
  * @param maxPoolSize The maximum number of channels, per replica node, in the channel pool. The default value is 100.
  *               RxMongo will always attempt to keep at least this number of channels active and ready for use.
  *               If this is too low, it affects latency of requests because channels may need to be obtained
  *               frequently. If this is too high, it consumes unneeded resources in the form of TCP connections.
  * @param minPoolSize The minimum number of channels, per replica node, in the channel pool. The default value is 0.
  *               RxMongo will never allocate more channels than specified by this value. If this is set too low
  *               then blocking will occur as more requests are being processed than the pool of channels can
  *               handle. If this is set too high, then unneeded connections may be retained until the idle
  *               timeout expires.
  * @param maxIdleTimeMS The maximum number of milliseconds that a connection can remain idle in the pool before being
  *                 removed and closed. The default is 60,000.
  * @param waitQueueMultiple A number that the driver multiples the maxPoolSize value to, to provide the maximum number
  *                     of requests allowed to wait for a connection to become available from the pool. The
  *                     default, 0, indicates that the requests are only constrained by memory size
  * @param waitQueueTimeoutMS The maximum time in milliseconds that a request can wait for a connection to become
  *                      available. The default is 60,000
  * @param w The Write Concern option. Write concern describes the kind of assurances that the mongod and the driver
  *     provide to the application regarding the success and durability of the write operation. This option defines
  *     the level and kind of write concern. This option can take either a number or a string as a value, as
  *     follows:
  * {{{
  * Option	Type	  Description
  * -1      number  The driver will not acknowledge write operations and will suppress all network or socket errors.
  * 0      number  The driver will not acknowledge write operations but will pass or handle any network and socket
  *             errors that it receives to the client. You can specify the write concern both in the connection
  *             string and as a parameter to method calls like insert or update. If the write concern is specified
  *             in both places, the method parameter overrides the connection-string setting.
  * 1      number  Provides basic acknowledgment of write operations. By specifying 1, you require that a standalone
  *             mongod instance, or the primary for replica sets, acknowledge all write operations. This is the
  *             default write concern setting.
  * majority string  For replica sets, if you specify the special majority value to w option, write operations will
  *             only return successfully after a majority of the voting members of the replica set have
  *             acknowledged the write operation.
  * n      number  For replica sets, if you specify a number n, greater than 1, operations with this write concern
  *             return only after n members of the set have acknowledged the write. If you set n to a number that
  *             is greater than the number of available set members or members that hold data, MongoDB will wait,
  *             potentially indefinitely, for these members to become available.
  * tags	   string  For replica sets, you can specify a tag set to require that all members of the set that have these
  *             tags configured return confirmation of the write operation.
  * }}}
  *
  * @param wtimeoutMS The time in milliseconds to wait for replication to succeed, as specified in the w option,
  *              before timing out. When wtimeoutMS is 0 (the default), write operations will never time out.
  * @param journal Controls whether write operations will wait until the mongod acknowledges the write operations and
  *           commits the data to the on disk journal. When true: Enables journal commit acknowledgment write
  *           concern. Equivalent to specifying a write concern with the j option enabled. When false: Does not
  *           require that mongod commit write operations to the journal before acknowledging the write operation.
  *           This is the default option for the journal parameter. If you set journal to true, and specify a w
  *           value less than 1, journal prevails. If you set journal to true, and the mongod does not have
  *           journaling enabled, as with storage.journal.enabled, then MongoDB will error.
  * @param readPreference The Read Preference Option. The Read preference describe the behavior of read operations with
  *                  regards to replica sets. It specifies the replica set read preference for this connection.
  *                  The read preference values are the following: "primary": All read operations use only the
  *                  current replica set primary. This is the default. If the primary is unavailable, read
  *                  operations produce an error or throw an exception. "primaryPreferred": In most situations,
  *                  operations read from the primary member of the set. However, if the primary is unavailable,
  *                  as is the case during failover situations, operations read from secondary members.
  *                  "secondary": Operations read only from the secondary members of the set. If no secondaries
  *                  are available, then this read operation produces an error or exception. Read operations using
  *                  the secondary mode may return stale data. "secondaryPreferred": In most situations,
  *                  operations read from secondary members, but in situations where the set consists of a
  *                  single primary (and no other members), the read operation will use the set’s primary.
  *                  Read operations using the secondaryPreferred mode may return stale data. "nearest":
  *                  The driver reads from the nearest member of the set according to the member selection
  *                  process. Reads in the nearest mode do not consider the member’s type. Reads in nearest mode
  *                  may read from both primaries and secondaries. Set this mode to minimize the effect of
  *                  network latency on read operations without preference for current or stale data. Read
  *                  operations using the nearest mode may return stale data.
  * @param readPreferenceTags Specifies a tag set as a comma-separated list of colon-separated key-value pairs.
  *                      For example: `dc:ny,rack:1`. To specify a list of tag sets, use multiple
  *                      readPreferenceTags options. The following specifies two tag sets and an empty tag set:
  *                      `readPreferenceTags=dc:ny,rack:1&readPreferenceTags=dc:ny&readPreferenceTags=`. Order
  *                      matters when using multiple readPreferenceTags.
  * @param authSource Specify the database name associated with the user’s credentials, if the users collection do
  *              not exist in the database where the client is connecting. authSource defaults to the database
  *              specified in the connection string. For authentication mechanisms that delegate credential
  *              storage to other services, the authSource value should be $external as with the
  *              PLAIN (LDAP) and GSSAPI (Kerberos) authentication mechanisms. MongoDB will ignore authSource
  *              values if the connection string specifies no user name.
  * @param authMechanism Support for the PLAIN and MONGODB-X509 authentication mechanisms. Specify the authentication
  *                 mechanism that MongoDB will use to authenticate the connection. Possible values include:
  *                 "MONGODB-CR", "MONGODB-X509", "GSSAPI", "PLAIN". Only MongoDB Enterprise mongod and mongos
  *                 instances provide GSSAPI (Kerberos) and PLAIN (LDAP) mechanisms. To use MONGODB-X509, you must
  *                 have SSL Enabled.
  * @param gssapiServiceName Set the Kerberos service name when connecting to Kerberized MongoDB instances. This value
  *                     must match the service name set on MongoDB instances. gssapiServiceName defaults to
  *                     mongodb for all clients and for MongoDB instance. If you change saslServiceName setting
  *                     on a MongoDB instance, you will need to set gssapiServiceName to the same value.
  * @param uuidRepresentation The representation of UUIDs in this driver. This is provided for completeness only with
  *                      the MongoDB documentation. For RxMongo, this value is always "standard"
  * @param tcpNoDelay Enables or disables the TCP_NODELAY flag (disable/enable Nagle's algorithm). Enabled by default.
  * @param tcpKeepAlive Enables or disables the TCP_KEEPALIVE flag. Enabled by default
  * @param tcpOOBInline Enables or disables the TCP_OOBINLINE flag (receipt of urgent data). Disabled by default.
  * @param localIP Optionally specifies the local IP address from which the connection should be established. If not
  *           specified, the connection is made through whichever route can get there.
  * @param localPort Optionally specifies the local TCP port number to use. Next available is used if not specified.
  */

case class ConnectionOptions(
  replicaSet : Option[String] = None,
  ssl : Boolean = false,
  connectTimeoutMS : Long = 0,
  socketTimeoutMS : Long = 0,
  maxPoolSize : Int = 100,
  minPoolSize : Int = 0,
  maxIdleTimeMS : Long = 60000, // One minute
  waitQueueMultiple : Int = 0, // Unconstrained
  waitQueueTimeoutMS : Long = 60000, // One minute
  w : WriteConcern = BasicAcknowledgmentWC,
  wtimeoutMS : Long = 0,
  journal : Boolean = false,
  readPreference : ReadPreference = PrimaryRP,
  readPreferenceTags : Seq[Seq[(String, String)]] = Seq.empty[Seq[(String, String)]],
  authSource : Option[String] = None,
  authMechanism : AuthMechanism = MONGODB_X509,
  gssapiServiceName : Option[String] = None,
  final val uuidRepresentation : Option[String] = Some("standard"),
  tcpNoDelay : Boolean = true,
  tcpKeepAlive : Boolean = true,
  tcpOOBInline : Boolean = false,
  localIP : Option[InetAddress] = None,
  localPort : Int = 0)

sealed trait WriteConcern
case object NoAcknowledgmentWC extends WriteConcern { override def toString = "-1" }
case object ErrorsOnlyWC extends WriteConcern { override def toString = "0" }
case object BasicAcknowledgmentWC extends WriteConcern { override def toString = "1" }
case object MajorityWC extends WriteConcern { override def toString = "majority" }
case class WaitForMembersWC(numMembers : Int) extends WriteConcern { override def toString = numMembers.toString }
case class MembersWithTagWC(tag : String) extends WriteConcern { override def toString = tag }

object WriteConcern {
  def apply(str : String) : WriteConcern = {
    str match {
      case num : String if num forall { ch ⇒ ch.isDigit | ch == '-' } ⇒
        num.toInt match {
          case -1 ⇒ NoAcknowledgmentWC
          case 0 ⇒ ErrorsOnlyWC
          case 1 ⇒ BasicAcknowledgmentWC
          case x : Int ⇒ WaitForMembersWC(x)
        }
      case "majority" ⇒ MajorityWC
      case tag : String ⇒ MembersWithTagWC(tag)
      case _ ⇒ BasicAcknowledgmentWC
    }
  }
}

sealed trait ReadPreference
case object PrimaryRP extends ReadPreference { override def toString = "primary" }
case object PrimaryPreferredRP extends ReadPreference { override def toString = "primaryPreferred" }
case object SecondaryRP extends ReadPreference { override def toString = "secondary" }
case object SecondaryPreferredRP extends ReadPreference { override def toString = "secondaryPreferred" }
case object NearestRP extends ReadPreference { override def toString = "nearest" }

object ReadPreference {
  def apply(str : String) : ReadPreference = {
    str match {
      case "primary" ⇒ PrimaryRP
      case "primaryPreferred" ⇒ PrimaryPreferredRP
      case "secondary" ⇒ SecondaryRP
      case "secondaryPreferred" ⇒ SecondaryPreferredRP
      case "nearest" ⇒ NearestRP
      case _ ⇒ PrimaryRP
    }
  }

  def tags(str : String) : Iterable[(String, String)] = {
    val parts = str.split(",")
    for (part ← parts if part.contains(":")) yield {
      val parts = part.split(":")
      parts(0) -> parts(1)
    }
  }
}

sealed trait AuthMechanism { val asStr : String }
case object MONGODB_X509 extends AuthMechanism { override def toString = asStr; val asStr = "MONGODB-X509"; }
case object MONGODB_CR extends AuthMechanism { override def toString = asStr; val asStr = "MONGODB-CR" }
case object GSSAPI extends AuthMechanism { override def toString = asStr; val asStr = "GSSAPI" }
case object PLAIN extends AuthMechanism { override def toString = asStr; val asStr = "PLAIN" }

object AuthMechanism {
  def apply(str : String) : AuthMechanism = {
    str match {
      case MONGODB_X509.asStr ⇒ MONGODB_X509
      case MONGODB_CR.asStr ⇒ MONGODB_CR
      case GSSAPI.asStr ⇒ GSSAPI
      case PLAIN.asStr ⇒ PLAIN
      case _ ⇒ MONGODB_X509
    }
  }
}