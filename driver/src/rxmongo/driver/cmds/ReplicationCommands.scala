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

import akka.util.ByteString
import rxmongo.bson.{ BSONBuilder, BSONProvider, BSONObject }
import rxmongo.driver.AdminCommand

case class ReplicaSetMemberConfiguration(
  _id : Int,
  host : String,
  arbiterOnly : Boolean = false,
  buildIndexes : Boolean = true,
  hidden : Boolean = false,
  priority : Double = 1.0,
  tags : Map[String, String] = Map.empty[String, String],
  slaveDelay : Int = 0,
  votes : Int = 1) extends BSONProvider {
  require(votes == 0 || votes == 1, "0 <= votes <= 1")
  require(priority >= 0.0 && priority <= 1000.0, "0 <= priority <= 1000")
  def toByteString : ByteString = {
    val b = BSONBuilder()
    b.integer("_id", _id)
    b.string("host", host)
    b.boolean("arbiterOnly", arbiterOnly)
    b.boolean("buildIndexes", buildIndexes)
    b.double("priority", priority)
    b.obj("tags", BSONObject(tags))
    b.integer("slaveDelay", slaveDelay)
    b.integer("votes", votes)
    b.toByteString
  }
}

case class ReplicaSetConfiguration(
  _id : String,
  version : Int,
  members : Seq[ReplicaSetMemberConfiguration],
  getLastErrorDefaults : BSONObject = BSONObject(),
  chainingAllowed : Boolean = true,
  getLastErrorModes : BSONObject = BSONObject(),
  heartbeatTimeoutSec : Int = 10) extends BSONProvider {
  def toByteString : ByteString = {
    val b = BSONBuilder()
    b.string("_id", _id)
    b.integer("version", version)
    b.array("members", members.map { m ⇒ m.result })

    val s = BSONBuilder()
    s.obj("getLastErrorDefaults", getLastErrorDefaults)
    s.obj("getLastErrorModes", getLastErrorModes)
    s.boolean("chainingAllowed", chainingAllowed)
    s.integer("heartbeatTimeoutSec", heartbeatTimeoutSec)
    b.obj("settings", s)
    b.toByteString
  }
}
/** replSetFreeze
  * Prevents the current member from seeking election as primary for a period of time.
  * @see [[http://docs.mongodb.org/master/reference/command/replSetFreeze/]]
  * @param seconds The number of seconds to freeze the replication set
  */
case class ReplSetFreezeCmd(
  seconds : Int) extends AdminCommand(BSONObject("replSetFreeze" → seconds))

/** replSetFreeze
  * Cancels a previous replSetFreeze command, presumably early
  * @see [[http://docs.mongodb.org/master/reference/command/replSetFreeze/]]
  */
case class ReplSetUnfreezeCmd() extends AdminCommand(BSONObject("replSetFreeze" → 0))

/** replSetGetStatus
  * Returns a document that reports on the status of the replica set.
  * @see http://docs.mongodb.org/master/reference/command/replSetGetStatus/
  */
case class ReplSetGetStatusCmd() extends AdminCommand(BSONObject("replSetGetStatus" → 1))

/** replSetInitiate
  * Initializes a new replica set.
  * @see [[http://docs.mongodb.org/master/reference/command/replSetInitiate/]]
  * @param config The replication set configuration
  */
case class ReplSetInitiateCmd(
  config : ReplicaSetConfiguration) extends AdminCommand(BSONObject("replSetInitiate" → config.toBSONObject))

/** replSetMaintenance
  * Enables or disables a maintenance mode, which puts a secondary node in a RECOVERING state.
  * @see [[http://docs.mongodb.org/master/reference/command/replSetMaintenance/]]
  * @param mode true to turn replication set maintance on, false otherwise
  */
case class ReplSetMaintenanceCmd(mode : Boolean) extends AdminCommand(BSONObject("replSetMaintenance" → mode))

/** replSetReconfig
  * Applies a new configuration to an existing replica set.
  */
case class ReplSetReconfigCmd(
  config : ReplicaSetConfiguration,
  force : Boolean = false) extends AdminCommand(BSONObject("replSetReconfig" → config, "force" → force))

/** replSetStepDown
  * Forces the current primary to step down and become a secondary, forcing an election.
  * @see [[http://docs.mongodb.org/master/reference/command/replSetStepDown/]]
  * @param avoidReelectionSecs Optional. A number of seconds for the member to avoid election to primary. If you do not
  *                     specify a value for <seconds>, replSetStepDown will attempt to avoid reelection to
  *                     primary for 60 seconds from the time that the mongod received the replSetStepDown
  *                     command.
  * @param force Optional. New in version 2.0: Forces the primary to step down even if there are no secondary members
  *       that could become primary.
  * @param secondaryCatchupPeriodSecs	Optional. The amount of time that the mongod will wait for another secondary in
  *                            the replica set to catch up to the primary. If no secondary catches up before
  *                            this period ends, then the command will fail and the member will not step down,
  *                            unless you specify { force: true }. The default value is 10 seconds, unless you
  *                            set { force: true }, which changes the default to 0.
  */
case class ReplSetStepDownCmd(
  avoidReelectionSecs : Option[Int] = None,
  force : Option[Boolean] = None,
  secondaryCatchupPeriodSecs : Option[Int] = None) extends AdminCommand({
  val b = BSONBuilder()
  avoidReelectionSecs.map { rssd ⇒ b.integer("replSetStepDown", rssd) }
  force.map { f ⇒ b.boolean("force", f) }
  secondaryCatchupPeriodSecs.map { scps ⇒ b.integer("secondaryCatchupPeriodSecs", scps) }
  b.result
})

/** replSetSyncFrom
  * Explicitly override the default logic for selecting a member to replicate from.
  * @see [[http://docs.mongodb.org/master/reference/command/replSetSyncFrom/]]
  * @param from The name and port number of the replica set member that this member should replicate from.
  *      Use the [hostname]:[port] form.
  */
case class ReplSetSyncFromCmd(
  from : String) extends AdminCommand(BSONObject("replSetSyncFrom" → from))

/** resync
  * Forces a mongod to re-synchronize from the master. For master-slave replication only.
  * @see [[http://docs.mongodb.org/master/reference/command/resync/]]
  */
case class ResyncCmd() extends AdminCommand(BSONObject("resync" → 1))

/** isMaster
  * Displays information about this member’s role in the replica set, including whether it is the master.
  * @see [[http://docs.mongodb.org/master/reference/command/isMaster]]
  */
case class IsMasterCmd() extends AdminCommand(BSONObject("isMaster" → 1))

/** replSetGetConfig
  * Returns the replica set’s configuration object.
  * @see [[http://docs.mongodb.org/master/reference/command/replSetGetConfig/]]
  */
case class replSetGetConfigCmd() extends AdminCommand(BSONObject("replSetGetConfig" → 1))
