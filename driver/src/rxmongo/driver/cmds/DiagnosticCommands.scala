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

package rxmongo.driver.cmds

import rxmongo.bson.{ BSONBuilder, BSONObject }
import rxmongo.driver.{ AdminCommand, Command }

/** logApplicationMessage
  * The logApplicationMessage command allows users to post a custom message to the audit log. If running with
  * authorization, users must have clusterAdmin role, or roles that inherit from clusterAdmin, to run the command.
  * Available only in MongoDB Enterprise.
  * @see [[http://docs.mongodb.org/master/reference/command/logApplicationMessage/]]
  */
case class LogApplicationMessageCmd(msg : String) extends AdminCommand(BSONObject("logApplicationMessage" → msg))

sealed trait ExplainVerbosity
case object QueryPlanner extends ExplainVerbosity { override def toString = "queryPlanner" }
case object ExecutionStats extends ExplainVerbosity { override def toString = "executionStats" }
case object AllPlansExecution extends ExplainVerbosity { override def toString = "allPlansExecution" }

/** explain
  * Returns information on the execution of various operations.
  * @see [[http://docs.mongodb.org/master/reference/command/explain/#dbcmd.explain]]
  * @param db The database to run the explain against
  * @param cmd The command object to explain
  * @param verbosity The verbosity level
  */
case class ExplainCmd private[rxmongo] (db : String, cmd : BSONObject, verbosity : ExplainVerbosity)
  extends Command(db, BSONObject("explain" → cmd))

object ExplainCmd {
  def apply(db : String, cmd : UpdateCmd, verbosity : ExplainVerbosity) : ExplainCmd =
    new ExplainCmd(db, cmd.selector, verbosity)
  def apply(db : String, cmd : CountCmd, verbosity : ExplainVerbosity) : ExplainCmd =
    new ExplainCmd(db, cmd.selector, verbosity)
}

/** listDatabases
  * Returns a document that lists all databases and returns basic database statistics.
  * http://docs.mongodb.org/master/reference/command/listDatabases/
  */
case class ListDatabasesCmd() extends AdminCommand(BSONObject("listDatabases" → 1))

/** listCommands
  * Lists all database commands provided by the current mongod instance.
  * @see [[http://docs.mongodb.org/master/reference/command/listCommands/]]
  */
case class ListCommandsCmd() extends AdminCommand(BSONObject("listCommands" → 1))

/** buildInfo
  * Displays statistics about the MongoDB build.
  * @see [[http://docs.mongodb.org/master/reference/command/buildInfo/]]
  */
case class BuildInfoCmd() extends AdminCommand(BSONObject("buildInfo" → 1))

/** collStats
  * Reports storage utilization statics for a specified collection.
  * @see [[http://docs.mongodb.org/master/reference/command/collStats/]]
  * @param db The database containing the collection
  * @param collection The name of the collection for which stats are returned.
  * @param scale A scale factor for the output
  * @param verbose Increases reporting for mmapv1 storage engines when set to true
  */
case class CollStatsCmd(db : String, collection : String, scale : Int = 1024, verbose : Boolean = true)
  extends Command(db, BSONObject("collStats" → collection, "scale" → scale, "verbose" → verbose))

/** connPoolStats
  * Reports statistics on the outgoing connections from this MongoDB instance to other MongoDB instances in the
  * deployment.
  * @see [[http://docs.mongodb.org/master/reference/command/connPoolStats/]]
  */
case class ConnPoolStatsCmd() extends AdminCommand(BSONObject("connPoolStats" → 1))

/** shardConnPoolStats
  * Reports statistics on a mongos‘s connection pool for client operations against shards.
  * @see [[http://docs.mongodb.org/master/reference/command/shardConnPoolStats/]]
  */
case class ShardConnPoolStatsCmd() extends AdminCommand(BSONObject("shardConnPoolStats" -> 1))

/** dbStats
  * Reports storage utilization statistics for the specified database.
  * @see [[http://docs.mongodb.org/master/reference/command/dbStats/]]
  * @param db The database to report on.
  * @param scale The scale factor for the output
  */
case class DbStatsCmd(db : String, scale : Int = 1024) extends Command(db, BSONObject("dbStats" → 1, "scale" → scale))

/** getCmdLineOpts
  * Returns a document with the run-time arguments to the MongoDB instance and their parsed options.
  */
case class GetCmdLineOptsCmd() extends AdminCommand(BSONObject("getCmdLineOpts" -> 1))

/** profile
  * Interface for the database profiler.
  * @see [[http://docs.mongodb.org/master/reference/command/profile/]]
  * @param db The databaset o profile
  * @param level The profiling level. -1=No change(returns the current profile level), 0=Off(no profiling),
  *  1=Only include slow operations, 2=Include all operations.
  * @param slowms Optional.  Sets the threshold in milliseconds for slow operations
  */
case class ProfileCmd(db : String, level : Int, slowms : Option[Int] = None) extends Command(db, {
  val b = BSONBuilder()
  b.integer("profile", level)
  slowms.map { ms ⇒ b.integer("slowms", ms) }
  b.result
})

/** top
  * Returns raw usage statistics for each database in the mongod instance.
  * @see [[http://docs.mongodb.org/master/reference/command/top/]]
  */
case class TopCmd() extends AdminCommand(BSONObject("top" -> 1))

sealed trait GetLogOption
case object GlobalLogOption extends GetLogOption { override def toString = "global" }
case object ReplicaSetLogOption extends GetLogOption { override def toString = "rs" }
case object StartupWarningsLogOption extends GetLogOption { override def toString = "startupWarnings" }
case object AllLogOption extends GetLogOption { override def toString = "*" }

/** getLog
  * Returns recent log messages.
  * @see [[http://docs.mongodb.org/master/reference/command/getLog/]]
  * @param option A GetLogOption value to control what log events get returned
  */
case class GetLogCmd(option : GetLogOption) extends AdminCommand(BSONObject("getLog" → option.toString))

/** hostInfo
  * Returns data that reflects the underlying host system.
  * @see [[http://docs.mongodb.org/master/reference/command/hostInfo/]]
  */
case class HostInfoCmd() extends AdminCommand(BSONObject("hostInfo" -> 1))

/** serverStatus
  * Returns a collection metrics on instance-wide resource utilization and status.
  * @see [[http://docs.mongodb.org/master/reference/command/serverStatus/]]
  */
case class serverStatusCmd() extends AdminCommand(BSONObject("serverStatus" → 1))
