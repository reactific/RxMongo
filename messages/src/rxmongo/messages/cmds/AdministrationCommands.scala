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

package rxmongo.messages.cmds

import rxmongo.bson.{ BSONBuilder, BSONObject }
import rxmongo.messages.{ AdminCommand, Command }

case class ServerStatus() extends AdminCommand(BSONObject("serverStatus" → 1))

/** renameCollection
  * Changes the name of an existing collection.
  * @see [[http://docs.mongodb.org/master/reference/command/renameCollection/#dbcmd.renameCollection]]
  * @param db Database in which source collection exists
  * @param fromName Name of existing collection
  * @param toName New name for the collection
  * @param dropTarget True if you want a new target collection to be dropped if it already exists.
  */
case class RenameCollectionCmd(
  db : String,
  fromName : String,
  toName : String,
  dropTarget : Boolean) extends AdminCommand(
  BSONObject(
    "renameCollection" → (db + "." + fromName),
    "to" → (db + "." + toName),
    "dropTarget" → dropTarget
  ))

/** copyDb
  * Copies a database from a remote host to the current host.
  * @see [[http://docs.mongodb.org/master/reference/command/copydb/]]
  * @param fromdb The name of the database from which the copy is made
  * @param todb The name of the newly copied database
  * @param fromhost The optional host from which the database is copied
  * @param slaveOk Set to true to allow
  * @param username Username for fromhost
  * @param nonce One time shared secret for the copy
  * @param key Hash of user password for fromhost
  */
case class CopyDbCmd(
  fromdb : String,
  todb : String,
  slaveOk : Boolean = false,
  fromhost : Option[String] = None,
  username : Option[String] = None,
  nonce : Option[String] = None,
  key : Option[String] = None) extends AdminCommand({
  val b = BSONBuilder().
    integer("copydb", 1).
    string("fromdb", fromdb).
    string("todb", todb).
    boolean("slaveOk", slaveOk)
  fromhost.map { hostname ⇒ b.string("fromhost", hostname) }
  username.map { username ⇒ b.string("username", username) }
  nonce.map { nonce ⇒ b.string("nonce", nonce) }
  key.map { key ⇒ b.string("key", key) }
  b.result
})

/** dropDatabase
  * Removes the current database.
  * @see [[http://docs.mongodb.org/master/reference/command/dropDatabase/]]
  * @param db the database to drop
  */
case class DropDatabaseCmd(db : String) extends Command(db, BSONObject("dropDatabase" → 1))

/** clone
  * Copies a database from a remote host to the current host.
  * @see [[http://docs.mongodb.org/master/reference/command/clone/]]
  * @param dbSpec The database/hostname/port to clone from
  */
case class CloneCmd(dbSpec : String) extends AdminCommand(BSONObject("clone" → dbSpec))

/** fsync
  * Flushes pending writes to the storage layer and locks the database to allow backups.
  * @see [[http://docs.mongodb.org/master/reference/command/fsync/]]
  * @param async Do fsync asynchronously
  */
case class FSyncCmd(async : Boolean = false)
  extends AdminCommand(BSONObject("fsync" → 1, "async" → async))

/** connectionStatus
  * Reports the authentication state for the current connection.
  * @see [[http://docs.mongodb.org/master/reference/command/connectionStatus/#dbcmd.connectionStatus]]
  */
case class ConnectionStatus()
  extends AdminCommand(BSONObject("connectionStatus" → 1))

class GetParameterCmd(name : String) extends AdminCommand(BSONObject("getParameter" → 1, name → 1))

case class GetQuietParameterCmd() extends GetParameterCmd("quiet")
case class GetNoTableScanCmd() extends GetParameterCmd("notablescan")
case class GetLogLevelCmd() extends GetParameterCmd("logLevel")
case class GetSyncDelayCmd() extends GetParameterCmd("syncdelay")

/** Base Class For "setParameter" types of commands
  *
  * @see [[http://docs.mongodb.org/master/reference/parameters/]]
  * @param name The name of the parameter to set
  * @param value The value to which the parameter must be set
  */
class SetParameterCmd(name : String, value : Any)
  extends AdminCommand(BSONObject("setParameter" → 1, name → value))

/** journalCommitInterval
  * @see [[http://docs.mongodb.org/master/reference/parameters/#param.journalCommitInterval]]
  * @param ms The number of milliseconds to set journalCommitInterval to
  */
case class SetJournalCommitInterval(ms : Int)
  extends SetParameterCmd("journalCommitInterval", ms) {
  require(ms >= 1 && ms <= 500, "1 <= journalCommitInterval <= 500")
}

/** logUserIds
  * @see [[http://docs.mongodb.org/master/reference/parameters/#param.logUserIds]]
  * @param on Set to true to turn on logging of user ids, set to false to turn it off.
  */
case class SetLogUserIds(on : Boolean) extends SetParameterCmd("logUserIds", if (on) 1 else 0)

/** logLevel
  * @see [[http://docs.mongodb.org/master/reference/parameters/#param.logLevel]]
  * @param level The level of verbosity for mongod logging (0-5).
  */
case class SetLogLevel(level : Int) extends SetParameterCmd("logLevel", level) {
  require(level >= 0 && level <= 5, "0 <= logLevel <= 5")
}

/** noTableScan
  * @see [[http://docs.mongodb.org/master/reference/parameters/#param.notablescan]]
  * @param on Turn noTableScan on or off
  */
case class SetNoTableScan(on : Boolean) extends SetParameterCmd("notablescan", if (on) 1 else 0)

/** quiet
  * @see [[http://docs.mongodb.org/master/reference/parameters/#param.quiet]]
  * @param on Turn quiet mode on or off
  */
case class SetQuiet(on : Boolean) extends SetParameterCmd("quiet", if (on) 1 else 0)

/** replApplyBatchSize
  *
  * @see [[http://docs.mongodb.org/master/reference/parameters/#param.replApplyBatchSize]]
  * @param value The value to which the parameter must be set
  */
case class SetReplApplyBatchSize(value : Int) extends SetParameterCmd("replApplyBatchSize", value) {
  require(value >= 1 && value <= 1024, "1 <= replApplyBatchSize <= 1024")
}

sealed trait ReplIndexPrefetchOption
case object ReplIndexPrefetch_None extends ReplIndexPrefetchOption { override def toString = "none" }
case object ReplIndexPrefetch_All extends ReplIndexPrefetchOption { override def toString = "all" }
case object ReplIndexPrefetch_Id_Only extends ReplIndexPrefetchOption { override def toString = "_id_only" }

/** replIndexPrefetch
  *
  * @see [[http://docs.mongodb.org/master/reference/parameters/#param.replIndexPrefetch]]
  * @param value The value to which the parameter must be set
  */
case class SetReplIndexPrefetch(value : ReplIndexPrefetchOption)
  extends SetParameterCmd("replIndexPrefetch", value.toString)

/** syncdelay
  *
  * @see [[http://docs.mongodb.org/master/reference/parameters/#param.syncdelay]]
  * @param value The value to which the parameter must be set
  */
case class SetSyncDelay(value : Int) extends SetParameterCmd("syncdelay", value) {
  require(value > 0 && value < 3600, "0 < syncDelay < 3600")
}

/** traceExceptions
  *
  * @see [[http://docs.mongodb.org/master/reference/parameters/#param.traceExceptions]]
  * @param value The value to which the parameter must be set
  */
case class SetTraceExceptions(value : Boolean) extends SetParameterCmd("traceExceptions", value)

sealed trait SslModeOption
case object PreferSSL extends SslModeOption { override def toString = "preferSSL" }
case object RequireSSL extends SslModeOption { override def toString = "requireSSL" }
/** sslMode
  *
  * @see [[http://docs.mongodb.org/master/reference/parameters/#param.sslMode]]
  * @param value The value to which the parameter must be set
  */
case class SetSslMode(value : SslModeOption) extends SetParameterCmd("sslMode", value.toString)

sealed trait ClusterAuthModes
case object KeyFileClusterAuthMode extends ClusterAuthModes { override def toString = "keyFile" }
case object SendKeyFileClusterAuthMode extends ClusterAuthModes { override def toString = "sendKeyFile" }
case object SendX590ClusterAuthMode extends ClusterAuthModes { override def toString = "sendX509" }
case object X509ClusterAuthMode extends ClusterAuthModes { override def toString = "x509" }

/** clusterAuthMode
  *
  * @see [[http://docs.mongodb.org/master/reference/parameters/#param.clusterAuthMode]]
  * @param value The value to which the parameter must be set
  */
case class SetClusterAuthMode(value : ClusterAuthModes) extends SetParameterCmd("clusterAuthMode", value.toString)

/** userCacheInvalidationIntervalSecs
  *
  * @see [[http://docs.mongodb.org/master/reference/parameters/#param.userCacheInvalidationIntervalSecs]]
  * @param value The value to which the parameter must be set
  */
case class SetUserCacheInvalidationIntervalSecs(value : Int)
  extends SetParameterCmd("userCacheInvalidationIntervalSecs", value) {
  require(value > 0 && value <= 60 * 60 * 24, "0 < userCacheInvalidationIntervalSecs < 86400")
}

/** wiredTigerEngineRuntimeConfigSetting
  *
  * @see [[http://docs.mongodb.org/master/reference/parameters/#param.wiredTigerEngineRuntimeConfigSetting]]
  * @param config A string containing name=value pairs, separated by commas, for the WiredTiger configuration
  */
class SetWiredTigerEngineRuntimeConfigSetting(config : String)
  extends SetParameterCmd("wiredTigerEngineRuntimeConfigSetting", config)

sealed trait MongoLoggers
case object AccessControlLogger extends MongoLoggers { override def toString = "accessControl" }
case object CommandLogger extends MongoLoggers { override def toString = "command" }
case object ControlLogger extends MongoLoggers { override def toString = "control" }
case object GeoLogger extends MongoLoggers { override def toString = "geo" }
case object IndexLogger extends MongoLoggers { override def toString = "index" }
case object NetworkLogger extends MongoLoggers { override def toString = "network" }
case object QueryLogger extends MongoLoggers { override def toString = "query" }
case object ReplicationLogger extends MongoLoggers { override def toString = "replication" }
case object ShardingLogger extends MongoLoggers { override def toString = "sharding" }
case object StorageLogger extends MongoLoggers { override def toString = "storage" }
case object WriteLogger extends MongoLoggers { override def toString = "write" }

/** logComponentVerbosity
  *
  * @see [[http://docs.mongodb.org/master/reference/parameters/#param.logComponentVerbosity]]
  * @param default THe default verbosity level for unspecified loggers
  * @param components The logging levels for various components.
  */
case class SetLogComponentVerbosity(default : Int, components : (MongoLoggers, Int)*)
  extends AdminCommand(
    BSONObject.from(components.map { case (l, v) ⇒ l.toString -> v } :+ ("verbosity" → default)))

/** logRotate
  * Rotates the MongoDB logs to prevent a single file from taking too much space.
  * @see [[http://docs.mongodb.org/master/reference/command/logRotate/]]
  */
case object LogRotateCmd extends AdminCommand(BSONObject("logRotate" → 1))
