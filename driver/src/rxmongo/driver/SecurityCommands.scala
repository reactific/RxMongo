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
import rxmongo.bson._

case class GetNonceCmd(
  db : String) extends Command(db, BSONObject("getnonce" -> 1))

case class AuthenticateCmd(
  db : String,
  user : String,
  pass : String,
  mechanism : AuthMechanism) extends Command(db, BSONObject(
  "authenticate" -> 1, "username" -> user, "password" -> pass, "mechanism" -> mechanism.asStr)
)

case class Role(role : String, db : String) extends BSONProvider {
  def toByteString : ByteString = {
    val b = BSONBuilder()
    b.string("role", role)
    b.string("db", db)
    b.toByteString
  }
}

object Role {
  implicit object Codec extends BSONCodec[Role, BSONObject] {
    def code : TypeCode = ObjectCode
    def write(value : Role) : BSONObject = BSONObject("role" -> value.role, "db" -> value.db)
    def read(value : BSONObject) : Role = { Role(value.getAsString("role"), value.getAsString("db")) }
  }
}

/** createUser
  * @see [[http://docs.mongodb.org/master/reference/command/createUser/#dbcmd.createUser]]
  * @param db
  * @param userName
  * @param password
  * @param customData
  * @param thisDbRole
  * @param roles
  * @param writeConcern
  */
case class CreateUserCmd(
  db : String,
  userName : String,
  password : String,
  customData : BSONObject,
  thisDbRole : String,
  roles : Seq[Role],
  writeConcern : Option[WriteConcern] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("createUser", userName)
  b.string("pwd", password)
  b.obj("customData", customData)
  b.array("roles", thisDbRole, roles.map { r ⇒ r.toBSONObject } : _*)
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.toBSONObject) }
  b.result
}
)

/** updateUser
  * @see [[http://docs.mongodb.org/master/reference/command/updateUser/]]
  * TODO: UpdateUserCmd
  */
case class UpdateUserCmd()

/** dropUser
  * @see [[http://docs.mongodb.org/master/reference/command/dropUser/]]
  * TODO: DropUserCmd
  */
case class DropUserCmd()

/** dropAllUsersFromDatabase
  * @see [[http://docs.mongodb.org/master/reference/command/dropAllUsersFromDatabase/]]
  * TODO: DropAllUsersFromDatabaseCmd
  */
case class DropAllUsersFromDatabaseCmd()

/** grantRolesToUsers
  * @see [[http://docs.mongodb.org/master/reference/command/grantRolesToUser/]]
  * TODO: GrantRolesToUsersCmd
  */
case class GrantRolesToUsersCmd()

/** revokeRolesFromUser
  * @see [[http://docs.mongodb.org/master/reference/command/revokeRolesFromUser/]]
  * TODO: RevokeRolesFromUserCmd
  */
case class RevokeRolesFromUserCmd()

/** usersInfo
  * @see [[http://docs.mongodb.org/master/reference/command/usersInfo/]]
  * TODO: UsersInfoCmd
  */
case class UsersInfoCmd()

sealed trait Action
object Action {
  implicit object Codec extends BSONCodec[Action, BSONString] {
    def code : TypeCode = StringCode
    def write(value : Action) : BSONString = { BSONString(value.toString) }
    def read(value : BSONString) : Action = {
      value.value match {
        case "find" ⇒ FindAction
        case "insert" ⇒ InsertAction
        case "remove" ⇒ RemoveAction
        case "update" ⇒ UpdateAction
        case "changeCustomData" ⇒ ChangeCustomDataAction
        case "changeOwnCustomData" ⇒ ChangeOwnCustomDataAction
        case "changeOwnPassword" ⇒ ChangeOwnPasswordAction
        case "changePassword" ⇒ ChangePasswordAction
        case "createCollection" ⇒ CreateCollectionAction
        case "createIndex" ⇒ CreateIndexAction
        case "createRole" ⇒ CreateRoleAction
        case "createUser" ⇒ CreateUserAction
        case "dropCollection" ⇒ DropCollectionAction
        case "dropRole" ⇒ DropRoleAction
        case "dropUser" ⇒ DropUserAction
        case "emptycapped" ⇒ EmptyCappedAction
        case "enableProfiler" ⇒ EnableProfilerAction
        case "grantRole" ⇒ GrantRoleAction
        case "killCursors" ⇒ KillCursorsAction
        case "revokeRole" ⇒ RevokeRoleAction
        case "unlock" ⇒ UnlockAction
        case "viewRole" ⇒ ViewRoleAction
        case "viewUser" ⇒ ViewUserAction
        case "authSchemaUpgrade" ⇒ AuthSchemaUpgradeAction
        case "cleanupOrphaned" ⇒ CleanupOrphanedAction
        case "cpuProfiler" ⇒ CpuProfilerAction
        case "inprog" ⇒ InprogAction
        case "invalidateUserCache" ⇒ InvalidateUserCacheAction
        case "killop" ⇒ KillopAction
        case "planCacheRead" ⇒ PlanCacheReadAction
        case "planCacheWrite" ⇒ PlanCacheWriteAction
        case "storageDetails" ⇒ StorageDetailsAction
        case "appendOplogNote" ⇒ AppendOplogNoteAction
        case "replSetConfigure" ⇒ ReplSetConfigureAction
        case "replSetGetStatus" ⇒ ReplSetGetStatusAction
        case "replSetHeartbeat" ⇒ ReplSetHeartbeatAction
        case "replSetStateChange" ⇒ ReplSetStateChangeAction
        case "resync" ⇒ ResyncAction
        case "addShard" ⇒ AddShardAction
        case "enableSharding" ⇒ EnableShardingAction
        case "flushRouterConfig" ⇒ FlushRouterConfigAction
        case "getShardMap" ⇒ GetShardMapAction
        case "getShardVersion" ⇒ GetShardVersionAction
        case "listShards" ⇒ ListShardsAction
        case "moveChunk" ⇒ MoveChunkAction
        case "removeShard" ⇒ RemoveShardAction
        case "shardingState" ⇒ ShardingStateAction
        case "splitChunk" ⇒ SplitChunkAction
        case "splitVector" ⇒ SplitVectorAction
        case "applicationMessage" ⇒ ApplicationMessageAction
        case "closeAllDatabases" ⇒ CloseAllDatabasesAction
        case "collMod" ⇒ CollModAction
        case "compact" ⇒ CompactAction
        case "connPoolSync" ⇒ ConnPoolSyncAction
        case "convertToCapped" ⇒ ConvertToCappedAction
        case "dropDatabase" ⇒ DropDatabaseAction
        case "dropIndex" ⇒ DropIndexAction
        case "fsync" ⇒ FsyncAction
        case "getParameter" ⇒ GetParameterAction
        case "hostInfo" ⇒ HostInfoAction
        case "logRotate" ⇒ LogRotateAction
        case "reIndex" ⇒ ReIndexAction
        case "renameCollectionSameDB" ⇒ RenameCollectionSameDBAction
        case "repairDatabase" ⇒ RepairDatabaseAction
        case "setParameter" ⇒ SetParameterAction
        case "shutdown" ⇒ ShutdownAction
        case "touch" ⇒ TouchAction
        case "collStats" ⇒ CollStatsAction
        case "connPoolStats" ⇒ ConnPoolStatsAction
        case "cursorInfo" ⇒ CursorInfoAction
        case "dbHash" ⇒ DbHashAction
        case "dbStats" ⇒ DbStatsAction
        case "diagLogging" ⇒ DiagLoggingAction
        case "getCmdLineOpts" ⇒ GetCmdLineOptsAction
        case "getLog" ⇒ GetLogAction
        case "indexStats" ⇒ IndexStatsAction
        case "listDatabases" ⇒ ListDatabasesAction
        case "listCollections" ⇒ ListCollectionsAction
        case "listIndexes" ⇒ ListIndexesAction
        case "netstat" ⇒ NetstatAction
        case "serverStatus" ⇒ ServerStatusAction
        case "validate" ⇒ ValidateAction
        case "top" ⇒ TopAction
        case x ⇒ throw new NoSuchElementException(s"Invalid action $x")
      }
    }
  }
}

trait QueryAndWriteActions extends Action

/** Find Action
  * User can perform the db.collection.find() method. Apply this action to database or collection resources.
  */
case object FindAction extends QueryAndWriteActions { override def toString = "find" }
/** insert
  * User can perform the insert command. Apply this action to database or collection resources.
  */
case object InsertAction extends QueryAndWriteActions { override def toString = "insert" }
/** remove
  * User can perform the db.collection.remove() method. Apply this action to database or collection resources.
  */
case object RemoveAction extends QueryAndWriteActions { override def toString = "remove" }
/** update
  * User can perform the update command. Apply this action to database or collection resources.
  */
case object UpdateAction extends QueryAndWriteActions { override def toString = "update" }

trait DatabaseManagementActions extends Action

/** changeCustomData
  * User can change the custom information of any user in the given database. Apply this action to database resources.
  */
case object ChangeCustomDataAction extends DatabaseManagementActions { override def toString = "changeCustomData" }
/** changeOwnCustomData
  * Users can change their own custom information. Apply this action to database resources.
  */
case object ChangeOwnCustomDataAction extends DatabaseManagementActions { override def toString = "changeOwnCustomData" }
/** changeOwnPassword
  * Users can change their own passwords. Apply this action to database resources.
  */
case object ChangeOwnPasswordAction extends DatabaseManagementActions { override def toString = "changeOwnPassword" }
/** changePassword
  * User can change the password of any user in the given database. Apply this action to database resources.
  */
case object ChangePasswordAction extends DatabaseManagementActions { override def toString = "changePassword" }
/** createCollection
  * User can perform the db.createCollection() method. Apply this action to database or collection resources.
  */
case object CreateCollectionAction extends DatabaseManagementActions { override def toString = "createCollection" }
/** createIndex
  * Provides access to the db.collection.createIndex() method and the createIndexes command. Apply this action to database or collection resources.
  */
case object CreateIndexAction extends DatabaseManagementActions { override def toString = "createIndex" }
/** createRole
  * User can create new roles in the given database. Apply this action to database resources.
  */
case object CreateRoleAction extends DatabaseManagementActions { override def toString = "createRole" }
/** createUser
  * User can create new users in the given database. Apply this action to database resources.
  */
case object CreateUserAction extends DatabaseManagementActions { override def toString = "createUser" }
/** dropCollection
  * User can perform the db.collection.drop() method. Apply this action to database or collection resources.
  */
case object DropCollectionAction extends DatabaseManagementActions { override def toString = "dropCollection" }
/** dropRole
  * User can delete any role from the given database. Apply this action to database resources.
  */
case object DropRoleAction extends DatabaseManagementActions { override def toString = "dropRole" }
/** dropUser
  * User can remove any user from the given database. Apply this action to database resources.
  */
case object DropUserAction extends DatabaseManagementActions { override def toString = "dropUser" }
/** emptycapped
  * User can perform the emptycapped command. Apply this action to database or collection resources.
  */
case object EmptyCappedAction extends DatabaseManagementActions { override def toString = "emptycapped" }
/** enableProfiler
  * User can perform the db.setProfilingLevel() method. Apply this action to database resources.
  */
case object EnableProfilerAction extends DatabaseManagementActions { override def toString = "enableProfiler" }
/** grantRole
  * User can grant any role in the database to any user from any database in the system. Apply this action to database resources.
  */
case object GrantRoleAction extends DatabaseManagementActions { override def toString = "grantRole" }
/** killCursors
  * User can kill cursors on the target collection.
  */
case object KillCursorsAction extends DatabaseManagementActions { override def toString = "killCursors" }
/** revokeRole
  * User can remove any role from any user from any database in the system. Apply this action to database resources.
  */
case object RevokeRoleAction extends DatabaseManagementActions { override def toString = "revokeRole" }
/** unlock
  * User can perform the db.fsyncUnlock() method. Apply this action to the cluster resource.
  */
case object UnlockAction extends DatabaseManagementActions { override def toString = "unlock" }
/** viewRole
  * User can view information about any role in the given database. Apply this action to database resources.
  */
case object ViewRoleAction extends DatabaseManagementActions { override def toString = "viewRole" }
/** viewUser
  * User can view the information of any user in the given database. Apply this action to database resources.
  */
case object ViewUserAction extends DatabaseManagementActions { override def toString = "viewUser" }

trait DeploymentManagementActions extends Action

/** authSchemaUpgrade
  * User can perform the authSchemaUpgrade command. Apply this action to the cluster resource.
  */
case object AuthSchemaUpgradeAction extends DeploymentManagementActions { override def toString = "authSchemaUpgrade" }
/** cleanupOrphaned
  * User can perform the cleanupOrphaned command. Apply this action to the cluster resource.
  */
case object CleanupOrphanedAction extends DeploymentManagementActions { override def toString = "cleanupOrphaned" }
/** cpuProfiler
  * User can enable and use the CPU profiler. Apply this action to the cluster resource.
  */
case object CpuProfilerAction extends DeploymentManagementActions { override def toString = "cpuProfiler" }
/** inprog
  * User can use the db.currentOp() method to return pending and active operations. Apply this action to the cluster resource.
  */
case object InprogAction extends DeploymentManagementActions { override def toString = "inprog" }
/** invalidateUserCache
  * Provides access to the invalidateUserCache command. Apply this action to the cluster resource.
  */
case object InvalidateUserCacheAction extends DeploymentManagementActions { override def toString = "invalidateUserCache" }
/** killop
  * User can perform the db.killOp() method. Apply this action to the cluster resource.
  */
case object KillopAction extends DeploymentManagementActions { override def toString = "killop" }
/** planCacheRead
  * User can perform the planCacheListPlans and planCacheListQueryShapes commands and the PlanCache.getPlansByQuery() and PlanCache.listQueryShapes() methods. Apply this action to database or collection resources.
  */
case object PlanCacheReadAction extends DeploymentManagementActions { override def toString = "planCacheRead" }
/** planCacheWrite
  * User can perform the planCacheClear command and the PlanCache.clear() and PlanCache.clearPlansByQuery() methods. Apply this action to database or collection resources.
  */
case object PlanCacheWriteAction extends DeploymentManagementActions { override def toString = "planCacheWrite" }
/** storageDetails
  * User can perform the storageDetails command. Apply this action to database or collection resources.
  */
case object StorageDetailsAction extends DeploymentManagementActions { override def toString = "storageDetails" }

trait ReplicationActions extends Action

/** appendOplogNote
  * User can append notes to the oplog. Apply this action to the cluster resource.
  */
case object AppendOplogNoteAction extends ReplicationActions { override def toString = "appendOplogNote" }
/** replSetConfigure
  * User can configure a replica set. Apply this action to the cluster resource.
  */
case object ReplSetConfigureAction extends ReplicationActions { override def toString = "replSetConfigure" }
/** replSetGetStatus
  * User can perform the replSetGetStatus command. Apply this action to the cluster resource.
  */
case object ReplSetGetStatusAction extends ReplicationActions { override def toString = "replSetGetStatus" }
/** replSetHeartbeat
  * User can perform the replSetHeartbeat command. Apply this action to the cluster resource.
  */
case object ReplSetHeartbeatAction extends ReplicationActions { override def toString = "replSetHeartbeat" }
/** replSetStateChange
  * User can change the state of a replica set through the replSetFreeze, replSetMaintenance, replSetStepDown, and replSetSyncFrom commands. Apply this action to the cluster resource.
  */
case object ReplSetStateChangeAction extends ReplicationActions { override def toString = "replSetStateChange" }
/** resync
  * User can perform the resync command. Apply this action to the cluster resource.
  */
case object ResyncAction extends ReplicationActions { override def toString = "resync" }

trait ShardingActions extends Action

/** addShard
  * User can perform the addShard command. Apply this action to the cluster resource.
  */
case object AddShardAction extends ShardingActions { override def toString = "addShard" }
/** enableSharding
  * User can enable sharding on a database using the enableSharding command and can shard a collection using the shardCollection command. Apply this action to database or collection resources.
  */
case object EnableShardingAction extends ShardingActions { override def toString = "enableSharding" }
/** flushRouterConfig
  * User can perform the flushRouterConfig command. Apply this action to the cluster resource.
  */
case object FlushRouterConfigAction extends ShardingActions { override def toString = "flushRouterConfig" }
/** getShardMap
  * User can perform the getShardMap command. Apply this action to the cluster resource.
  */
case object GetShardMapAction extends ShardingActions { override def toString = "getShardMap" }
/** getShardVersion
  * User can perform the getShardVersion command. Apply this action to database resources.
  */
case object GetShardVersionAction extends ShardingActions { override def toString = "getShardVersion" }
/** listShards
  * User can perform the listShards command. Apply this action to the cluster resource.
  */
case object ListShardsAction extends ShardingActions { override def toString = "listShards" }
/** moveChunk
  * User can perform the moveChunk command. In addition, user can perform the movePrimary command provided that the privilege is applied to an appropriate database resource. Apply this action to database or collection resources.
  */
case object MoveChunkAction extends ShardingActions { override def toString = "moveChunk" }
/** removeShard
  * User can perform the removeShard command. Apply this action to the cluster resource.
  */
case object RemoveShardAction extends ShardingActions { override def toString = "removeShard" }
/** shardingState
  * User can perform the shardingState command. Apply this action to the cluster resource.
  */
case object ShardingStateAction extends ShardingActions { override def toString = "shardingState" }
/** splitChunk
  * User can perform the splitChunk command. Apply this action to database or collection resources.
  */
case object SplitChunkAction extends ShardingActions { override def toString = "splitChunk" }
/** splitVector
  * User can perform the splitVector command. Apply this action to database or collection resources.
  */
case object SplitVectorAction extends ShardingActions { override def toString = "splitVector" }

trait ServerAdministrationActions extends Action

/** applicationMessage
  * User can perform the logApplicationMessage command. Apply this action to the cluster resource.
  */
case object ApplicationMessageAction extends ServerAdministrationActions { override def toString = "applicationMessage" }
/** closeAllDatabases
  * User can perform the closeAllDatabases command. Apply this action to the cluster resource.
  */
case object CloseAllDatabasesAction extends ServerAdministrationActions { override def toString = "closeAllDatabases" }
/** collMod
  * User can perform the collMod command. Apply this action to database or collection resources.
  */
case object CollModAction extends ServerAdministrationActions { override def toString = "collMod" }
/** compact
  * User can perform the compact command. Apply this action to database or collection resources.
  */
case object CompactAction extends ServerAdministrationActions { override def toString = "compact" }
/** connPoolSync
  * User can perform the connPoolSync command. Apply this action to the cluster resource.
  */
case object ConnPoolSyncAction extends ServerAdministrationActions { override def toString = "connPoolSync" }
/** convertToCapped
  * User can perform the convertToCapped command. Apply this action to database or collection resources.
  */
case object ConvertToCappedAction extends ServerAdministrationActions { override def toString = "convertToCapped" }
/** dropDatabase
  * User can perform the dropDatabase command. Apply this action to database resources.
  */
case object DropDatabaseAction extends ServerAdministrationActions { override def toString = "dropDatabase" }
/** dropIndex
  * User can perform the dropIndexes command. Apply this action to database or collection resources.
  */
case object DropIndexAction extends ServerAdministrationActions { override def toString = "dropIndex" }
/** fsync
  * User can perform the fsync command. Apply this action to the cluster resource.
  */
case object FsyncAction extends ServerAdministrationActions { override def toString = "fsync" }
/** getParameter
  * User can perform the getParameter command. Apply this action to the cluster resource.
  */
case object GetParameterAction extends ServerAdministrationActions { override def toString = "getParameter" }
/** hostInfo
  * Provides information about the server the MongoDB instance runs on. Apply this action to the cluster resource.
  */
case object HostInfoAction extends ServerAdministrationActions { override def toString = "hostInfo" }
/** logRotate
  * User can perform the logRotate command. Apply this action to the cluster resource.
  */
case object LogRotateAction extends ServerAdministrationActions { override def toString = "logRotate" }
/** reIndex
  * User can perform the reIndex command. Apply this action to database or collection resources.
  */
case object ReIndexAction extends ServerAdministrationActions { override def toString = "reIndex" }
/** renameCollectionSameDB
  * Allows the user to rename collections on the current database using the renameCollection command. Apply this action to database resources. Additionally, the user must either have find on the source collection or not have find on the destination collection. If a collection with the new name already exists, the user must also have the dropCollection action on the destination collection.
  */
case object RenameCollectionSameDBAction extends ServerAdministrationActions { override def toString = "renameCollectionSameDB" }
/** repairDatabase
  * User can perform the repairDatabase command. Apply this action to database resources.
  */
case object RepairDatabaseAction extends ServerAdministrationActions { override def toString = "repairDatabase" }
/** setParameter
  * User can perform the setParameter command. Apply this action to the cluster resource.
  */
case object SetParameterAction extends ServerAdministrationActions { override def toString = "setParameter" }
/** shutdown
  * User can perform the shutdown command. Apply this action to the cluster resource.
  */
case object ShutdownAction extends ServerAdministrationActions { override def toString = "shutdown" }
/** touch
  * User can perform the touch command. Apply this action to the cluster resource.
  */
case object TouchAction extends ServerAdministrationActions { override def toString = "touch" }

trait DiagnosticActions extends Action

/** collStats
  * User can perform the collStats command. Apply this action to database or collection resources.
  */
case object CollStatsAction extends DiagnosticActions { override def toString = "collStats" }
/** connPoolStats
  * User can perform the connPoolStats and shardConnPoolStats commands. Apply this action to the cluster resource.
  */
case object ConnPoolStatsAction extends DiagnosticActions { override def toString = "connPoolStats" }
/** cursorInfo
  * User can perform the cursorInfo command. Apply this action to the cluster resource.
  */
case object CursorInfoAction extends DiagnosticActions { override def toString = "cursorInfo" }
/** dbHash
  * User can perform the dbHash command. Apply this action to database or collection resources.
  */
case object DbHashAction extends DiagnosticActions { override def toString = "dbHash" }
/** dbStats
  * User can perform the dbStats command. Apply this action to database resources.
  */
case object DbStatsAction extends DiagnosticActions { override def toString = "dbStats" }
/** diagLogging
  * User can perform the diagLogging command. Apply this action to the cluster resource.
  */
case object DiagLoggingAction extends DiagnosticActions { override def toString = "diagLogging" }
/** getCmdLineOpts
  * User can perform the getCmdLineOpts command. Apply this action to the cluster resource.
  */
case object GetCmdLineOptsAction extends DiagnosticActions { override def toString = "getCmdLineOpts" }
/** getLog
  * User can perform the getLog command. Apply this action to the cluster resource.
  */
case object GetLogAction extends DiagnosticActions { override def toString = "getLog" }
/** indexStats
  * User can perform the indexStats command. Apply this action to database or collection resources.
  */
case object IndexStatsAction extends DiagnosticActions { override def toString = "indexStats" }
/** listDatabases
  * User can perform the listDatabases command. Apply this action to the cluster resource.
  */
case object ListDatabasesAction extends DiagnosticActions { override def toString = "listDatabases" }
/** listCollections
  * User can perform the listCollections command. Apply this action to database resources.
  */
case object ListCollectionsAction extends DiagnosticActions { override def toString = "listCollections" }
/** listIndexes
  * User can perform the ListIndexes command. Apply this action to database or collection resources.
  */
case object ListIndexesAction extends DiagnosticActions { override def toString = "listIndexes" }
/** netstat
  * User can perform the netstat command. Apply this action to the cluster resource.
  */
case object NetstatAction extends DiagnosticActions { override def toString = "netstat" }
/** serverStatus
  * User can perform the serverStatus command. Apply this action to the cluster resource.
  */
case object ServerStatusAction extends DiagnosticActions { override def toString = "serverStatus" }
/** validate
  * User can perform the validate command. Apply this action to database or collection resources.
  */
case object ValidateAction extends DiagnosticActions { override def toString = "validate" }
/** top
  * User can perform the top command. Apply this action to the cluster resource.
  */
case object TopAction extends DiagnosticActions { override def toString = "top" }

case class Privilege(
  db : String,
  collection : String,
  actions : Seq[Action])

object Privilege {
  implicit object Codec extends BSONCodec[Privilege, BSONObject] {
    def code : TypeCode = ObjectCode
    def write(value : Privilege) : BSONObject = {
      val b = BSONBuilder()
      b.obj("resource", BSONObject("db" -> value.db, "collection" -> value.collection))
      b.array("actions", value.actions)
      b.result
    }
    def read(value : BSONObject) : Privilege = {
      val resource = value.getObj("resource")
      Privilege(
        resource.getAsString("db"),
        resource.getAsString("collection"),
        value.getAsArray[Action, BSONString]("actions")(Action.Codec)
      )
    }
  }
}

/** createRole
  * Creates a role and specifies its privileges.
  * @see [[http://docs.mongodb.org/master/reference/command/createRole/#dbcmd.createRole]]
  * @param db The database for the role
  * @param roleName The name of the role to create
  * @param privileges The privileges to associate with the role
  * @param thisDbRole A role to inherent from this database
  * @param roles Other roles to inherit privileges from other databases
  * @param writeConcern The write concern.
  */
case class CreateRoleCmd(
  db : String,
  roleName : String,
  privileges : Seq[Privilege],
  thisDbRole : String,
  roles : Seq[Role],
  writeConcern : Option[WriteConcern] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("createRole", roleName)
  b.array("privileges", privileges)
  b.array("roles", thisDbRole, roles : _*)
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.toBSONObject) }
  b.result
})

/** updateRole
  * Updates a user-defined role.
  * @see [[http://docs.mongodb.org/master/reference/command/updateRole/]]
  * @param db The database for the role
  * @param roleName The name of the role to create
  * @param privileges The privileges to associate with the role
  * @param thisDbRole A role to inherent from this database
  * @param roles Other roles to inherit privileges from other databases
  * @param writeConcern The write concern.
  */
case class UpdateRoleCmd(
  db : String,
  roleName : String,
  privileges : Seq[Privilege],
  thisDbRole : String,
  roles : Seq[Role] = Seq.empty[Role],
  writeConcern : Option[WriteConcern] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("updateRole", roleName)
  b.array("privileges", privileges)
  b.array("roles", thisDbRole, roles : _*)
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.toBSONObject) }
  b.result
})

/** dropRole
  * Deletes the user-defined role.
  */
case class DropRoleCmd(
  db : String,
  role : String,
  writeConcern : Option[WriteConcern] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("dropRole", role)
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.toBSONObject) }
  b.result
})

/** dropAllRolesFromDatabase
  * Deletes all user-defined roles from a database.
  */
case class DropAllRolesFromDatabaseCmd(
  db : String,
  writeConcern : Option[WriteConcern] = None) extends Command(db, {
  val b = BSONBuilder()
  b.integer("dropAllRolesFromDatabase", 1)
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.toBSONObject) }
  b.result
})

/** grantPrivilegesToRole
  * Assigns privileges to a user-defined role.
  */
case class GrantPrivilegesToRoleCmd(
  db : String,
  roleName : String,
  privileges : Seq[Privilege],
  writeConcern : Option[WriteConcern] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("grantPrivilegesToRole", roleName)
  b.array("privileges", privileges)
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.toBSONObject) }
  b.result
})

/** revokePrivilegesFromRole
  * Removes the specified privileges from a user-defined role.
  */
case class RevokePrivilegesFromRoleCmd(
  db : String,
  roleName : String,
  privileges : Seq[Privilege],
  writeConcern : Option[WriteConcern] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("revokePrivilegesFromRole", roleName)
  b.array("privileges", privileges)
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.toBSONObject) }
  b.result
})

/** grantRolesToRole
  * Specifies roles from which a user-defined role inherits privileges.
  */
case class GrantRolesToRoleCmd(
  db : String,
  roleName : String,
  thisDbRole : String,
  roles : Seq[Role] = Seq.empty[Role],
  writeConcern : Option[WriteConcern] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("grantRolesToRole", roleName)
  b.array("roles", thisDbRole, roles : _*)
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.toBSONObject) }
  b.result
})

/** revokeRolesFromRole
  * Removes specified inherited roles from a user-defined role.
  */
case class RevokeRolesFromRoleCmd(
  db : String,
  roleName : String,
  thisDbRole : String,
  roles : Seq[Role] = Seq.empty[Role],
  writeConcern : Option[WriteConcern] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("removeRolesFromRole", roleName)
  b.array("roles", thisDbRole, roles : _*)
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.toBSONObject) }
  b.result
})

/** rolesInfo
  * Returns information for the specified role or roles.
  */
case class RolesInfoCmd(db : String, roles : Seq[Role], showPrivileges : Boolean)
  extends Command(db, {
    val b = BSONBuilder()
    b.array[Role, BSONObject]("rolesInfo", roles)
    b.boolean("showPrivileges", showPrivileges)
    b.result
  })

case class RoleInfoCmd(db : String, role : String, showPrivileges : Boolean)
  extends Command(db, BSONObject("rolesInfo" -> role, "showPrivileges" -> showPrivileges))

case class AllRolesInfoCmd(db : String, showPrivileges : Boolean, showBuiltinRoles : Boolean = false)
  extends Command(db, BSONObject(
    "rolesInfo" -> 1, "showPrivileges" -> showPrivileges, "showBuiltinRoles" -> showBuiltinRoles
  ))

/** invalidateUserCache
  * Flushes the in-memory cache of user information, including credentials and roles.
  */
case class InvalidateUserCacheCmd(db : String) extends Command(db, BSONObject("invalidateUserCache" -> 1))
