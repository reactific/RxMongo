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

import rxmongo.bson.RxMongoError

import akka.http.model.Uri
import akka.http.model.Uri.Path.{ Segment, Slash, Empty }
import akka.http.model.Uri.Path

import java.net.InetSocketAddress

import scala.util.Try

/** MongoDB Connection URI format.
  *
  * This represents the information parsed from a MongoDB Connection URI. It is in a form that is ready to be
  * consumed by RxMongo.
  *
  * A MongoDB URI has the following structure:
  * {{{
  * mongodb://[username:password@]host1[:port1][,host2[:port2],...[,hostN[:portN]]][/[database][?options]]
  * }}}
  *
  * The components of this string are:
  * {{{
  * mongodb://
  * A required prefix to identify that this is a string in the standard connection format.
  * username:password@
  * Optional. If specified, the client will attempt to log in to the specific database using these
  * credentials after connecting to the mongod instance.
  * host1
  * This the only required part of the URI. It identifies a server address to connect to. It identifies
  * either a hostname, IP address, or UNIX domain socket.
  * :port1
  * Optional. The default value is :27017 if not specified.
  * hostX
  * Optional. You can specify as many hosts as necessary. You would specify multiple hosts, for example,
  * for connections to replica sets.
  * :portX
  * Optional. The default value is :27017 if not specified.
  * /database
  * Optional. The name of the database to authenticate if the connection string includes authentication
  * credentials in the form of username:password@. If /database is not specified and the connection string includes
  * credentials, the driver will authenticate to the admin database.
  * ?options
  * Connection specific options. See Connection String Options for a full description of these options.
  * }}}
  *
  * If the connection string does not specify a database/ you must specify a slash (i.e. /) between the last
  * hostN and the question mark that begins the string of options.
  *
  * @param hosts The resolved InetSocketAddresses for the hosts in the replica set
  * @param database The (optional) name of the database to authenticate to (defaults to admin database)
  * @param credentials The (optional) credentials for authentication
  * @param options The options parsed from the URI or defaulted
  *
  * @see [[http://docs.mongodb.org/master/reference/connection-string/]]
  *
  */
case class MongoURI(
  hosts : List[InetSocketAddress],
  database : Option[String],
  credentials : Option[Credentials],
  options : ConnectionOptions)

object MongoURI {

  val scheme = "mongodb://"
  val DefaultPort = 27017

  def apply(uri_str : String) : Try[MongoURI] = Try {
    if (!uri_str.startsWith(scheme))
      throw RxMongoError(s"Mongo URI must start with '$scheme'")
    val parts = uri_str.substring(scheme.length).split("/")
    val (authority : String, rest : String) = parts.length match {
      case 2 ⇒ parts(0) -> parts(1)
      case 1 ⇒ parts(0) -> ""
      case _ ⇒
        throw RxMongoError(s"Mongo URI must contain an authority.")
    }
    val parts2 = authority.split("@")
    val (credentials : String, hostpart : String) = parts2.length match {
      case 2 ⇒ parts2(0) -> parts2(1)
      case 1 ⇒ "" -> parts(0)
      case _ ⇒ throw RxMongoError(s"Mongo URI must contain an authority section")
    }
    val hosts = hostpart.split(",")
    if (hosts.length < 1)
      throw RxMongoError(s"Mongo URI must contain at least one host")

    val str = scheme + credentials + "@" + hosts(0) + "/" + rest
    val uri = Uri(str)
    val auth = uri.authority
    val database = uri.path match {
      case Slash(Segment(p : String, Empty)) ⇒ Some(p)
      case Path.SingleSlash ⇒ None
      case Empty ⇒ None
      case _ ⇒ throw RxMongoError(s"Mongo URI path must only be one segment")
    }
    val creds = if (auth.userinfo.isEmpty) None else Some(Credentials(auth.userinfo))
    val options = ConnectionOptions(uri.query)

    val hostList : List[InetSocketAddress] = {
      hosts.map { h ⇒
        val parts = h.split(":")
        val (host, port) = parts.size match {
          case 2 ⇒ parts(0) -> parts(1).toInt
          case 1 ⇒ parts(0) -> DefaultPort
          case _ ⇒ throw RxMongoError("Mongo URI host names must not be empty or contain more than one :")
        }
        InetSocketAddress.createUnresolved(host, port)
      }
    }.toList
    MongoURI(hostList, database, creds, options)
  }
}

case class Credentials(user : String, password : String) {
  override def toString = s"Credentials($user)"
}

object Credentials {
  def apply(str : String) : Credentials = {
    val parts = str.split(":")
    parts.length match {
      case 2 ⇒ Credentials(parts(0), parts(1))
      case 1 ⇒ Credentials(parts(0), "")
      case _ ⇒ throw RxMongoError("User credentials must contain a user name")
    }
  }
}

