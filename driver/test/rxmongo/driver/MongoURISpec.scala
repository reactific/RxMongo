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

import java.net.{ InetAddress, InetSocketAddress }
import java.util.concurrent.TimeUnit

import org.specs2.mutable.Specification

import scala.concurrent.duration.Duration

class MongoURISpec extends Specification {

  val simple = "mongodb://reid@somewhere.com/"
  val multi_host = "mongodb://host1.com,host2.com:8989/"
  val options_uri = "mongodb://host1.com/?localPort=29292"
  val complex =
    "mongodb://joe:blow@host1.com:999,host-with-dashes.com:1000,other_host.net:1001/dbName?replicaSet=james&ssl=true&connectTimeoutMS=10000&socketTimeoutMS=45000&maxPoolSize=10&minPoolSize=2&maxIdleTimeMS=30000&waitQueueMultiple=4&waitQueueTimeoutMS=30000&w=7&wtimeoutMS=10000&journal=true&readPreference=primaryPreferred&readPreferenceTags=k:v,l:w&authSource=other&authMechanism=PLAIN&gssapiServiceName=mygss&readPreferenceTags=a:b,c:d&tcpNoDelay=false&tcpKeepAlive=false&tcpOOBInline=true&localIP=0.0.0.0&localPort=27272"

  def getUri(s : String) : MongoURI = {
    val try_uri = MongoURI(s)
    if (try_uri.isFailure)
      throw try_uri.failed.get
    try_uri.get
  }

  def addr(h : String, p : Int) = InetSocketAddress.createUnresolved(h, p)

  "MongoURI" should {
    s"parse a simple url: $simple" in {
      val uri = getUri(simple)
      uri.hosts.head must beEqualTo(addr("somewhere.com", MongoURI.DefaultPort))
      uri.credentials must beEqualTo(Some(Credentials("reid", "")))
      uri.database must beEqualTo(None)
      uri.options must beEqualTo(ConnectionOptions())
    }

    s"parse a uri with multiple hosts: $multi_host" in {
      val uri = getUri(multi_host)
      uri.hosts.head must beEqualTo(addr("host1.com", MongoURI.DefaultPort))
      uri.hosts.tail.head must beEqualTo(addr("host2.com", 8989))
      uri.credentials must beEqualTo(None)
      uri.database must beEqualTo(None)
      uri.options must beEqualTo(ConnectionOptions())
    }

    s"parse a uri with options: $options_uri" in {
      val uri = getUri(options_uri)
      uri.hosts.head must beEqualTo(addr("host1.com", MongoURI.DefaultPort))
      uri.hosts.tail must beEqualTo(List.empty[InetSocketAddress])
      uri.database must beEqualTo(None)
      uri.options must beEqualTo(ConnectionOptions(localPort = 29292))
    }

    s"parse a complex url: $complex" in {
      val uri = getUri(complex)
      val hosts = uri.hosts
      hosts.length must beEqualTo(3)
      hosts(0) must beEqualTo(addr("host1.com", 999))
      hosts(1) must beEqualTo(addr("host-with-dashes.com", 1000))
      hosts(2) must beEqualTo(addr("other_host.net", 1001))
      uri.database must beEqualTo(Some("dbName"))
      uri.credentials must beEqualTo(Some(Credentials("joe", "blow")))
      uri.options must beEqualTo(ConnectionOptions(
        replicaSet = Some("james"),
        ssl = true,
        connectTimeoutMS = 10000,
        socketTimeoutMS = 45000,
        maxPoolSize = 10,
        minPoolSize = 2,
        maxIdleTimeMS = 30000, // One minute
        waitQueueMultiple = 4, // Unconstrained
        waitQueueTimeoutMS = 30000, // One minute
        WriteConcern(WaitForMembersWC(7), Duration(10000, TimeUnit.MILLISECONDS), journal = true),
        readPreference = PrimaryPreferredRP,
        readPreferenceTags = List(List("a" -> "b", "c" -> "d"), List("k" -> "v", "l" -> "w")),
        authSource = Some("other"),
        authMechanism = PLAIN,
        gssapiServiceName = Some("mygss"),
        tcpNoDelay = false,
        tcpKeepAlive = false,
        tcpOOBInline = true,
        localIP = Some(InetAddress.getByName("0.0.0.0")),
        localPort = 27272)
      )
    }
  }
}
