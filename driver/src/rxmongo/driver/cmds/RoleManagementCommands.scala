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

import rxmongo.bson.{ BSONObject, BSONBuilder }
import rxmongo.driver.{ Command, WriteConcern }

/** createRole
  * Creates a role and specifies its privileges.
  * @see [[http://docs.mongodb.org/master/reference/command/createRole/#dbcmd.createRole]]
  * @param db The database for the role
  * @param roleName The name of the role to create
  * @param privileges The privileges to associate with the role
  * @param thisDbRole A role to inherent from this database
  * @param roles Other roles to inherit privileges from other databases
  * @param writeConcern Optional. The level of write concern for the modification. The writeConcern document takes
  *          the same fields as the getLastError command.
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
  * @param writeConcern Optional. The level of write concern for the modification. The writeConcern document takes
  *          the same fields as the getLastError command.
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
  * @see [[http://docs.mongodb.org/master/reference/command/dropRole/]]
  * @param db The name of the database
  * @param role The name of the role to drop
  * @param writeConcern Optional. The level of write concern for the modification. The writeConcern document takes
  *          the same fields as the getLastError command.
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
  * @see [[http://docs.mongodb.org/master/reference/command/dropAllRolesFromDatabase/]]
  * @param db The name of the database.
  * @param writeConcern Optional. The level of write concern for the modification. The writeConcern document takes
  *          the same fields as the getLastError command.
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
  * @see [[http://docs.mongodb.org/master/reference/command/grantPrivilegesToRole/]]
  * @param db The name of the database.
  * @param roleName The name of the user-defined role to grant privileges to.
  * @param privileges The privileges to add to the role.
  * @param writeConcern Optional. The level of write concern for the modification. The writeConcern document takes
  *          the same fields as the getLastError command.
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
  * @see [[http://docs.mongodb.org/master/reference/command/revokePrivilegesFromRole/]]
  * @param db The name of the database.
  * @param roleName The user-defined role to revoke privileges from.
  * @param privileges An array of privileges to remove from the role.
  * @param writeConcern Optional. The level of write concern for the modification. The writeConcern document takes
  *          the same fields as the getLastError command.
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
  * @see [[http://docs.mongodb.org/master/reference/command/grantRolesToRole/]]
  * @param db The name of the database
  * @param roleName The name of a role to add subsidiary roles.
  * @param thisDbRole A role, in this database, from which to inherit.
  * @param roles An array of roles from which to inherit.
  * @param writeConcern Optional. The level of write concern for the modification. The writeConcern document takes
  *          the same fields as the getLastError command.
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
  * @see [[http://docs.mongodb.org/master/reference/command/revokeRolesFromRole/#dbcmd.revokeRolesFromRole]]
  * @param db THe name of the database.
  * @param roleName The role from which to remove inherited roles.
  * @param thisDbRole The inherited roles, in this database, to remove.
  * @param roles The inherited roles, in any database, to remove.
  * @param writeConcern Optional. The level of write concern to apply to this operation. The writeConcern document
  *          uses the same fields as the getLastError command.
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
  * @see [[http://docs.mongodb.org/master/reference/command/rolesInfo/#dbcmd.rolesInfo]]
  * @param db The name of the database for the roles.
  * @param roles The roles about which information shoud be obtained.
  * @param showPrivileges Whether or not to show the privileges in the returned information
  */
case class RolesInfoCmd(db : String, roles : Seq[Role], showPrivileges : Boolean)
  extends Command(db, {
    val b = BSONBuilder()
    b.array[Role]("rolesInfo", roles)
    b.boolean("showPrivileges", showPrivileges)
    b.result
  })

/** roleInfo
  * Returns information for the specified role or roles.
  * @see [[http://docs.mongodb.org/master/reference/command/rolesInfo/#dbcmd.rolesInfo]]
  * @param db The name of the database for the roles.
  * @param role The role about which information shoud be obtained.
  * @param showPrivileges Whether or not to show the privileges in the returned information
  */
case class RoleInfoCmd(db : String, role : String, showPrivileges : Boolean)
  extends Command(db, BSONObject("rolesInfo" -> role, "showPrivileges" -> showPrivileges))

/** roleInfo
  * Returns information about all roles in a database
  * @see [[http://docs.mongodb.org/master/reference/command/rolesInfo/#dbcmd.rolesInfo]]
  * @param db The name of the database for the roles.
  * @param showPrivileges Whether or not to show the privileges in the returned information
  * @param showBuiltinRoles Whether or not to show the built-in roles in the returned information
  */
case class AllRolesInfoCmd(db : String, showPrivileges : Boolean, showBuiltinRoles : Boolean = false)
  extends Command(db, BSONObject(
    "rolesInfo" -> 1, "showPrivileges" -> showPrivileges, "showBuiltinRoles" -> showBuiltinRoles
  ))

/** invalidateUserCache
  * Flushes the in-memory cache of user information, including credentials and roles.
  * @see [[http://docs.mongodb.org/master/reference/command/invalidateUserCache/#dbcmd.invalidateUserCache]]
  */
case class InvalidateUserCacheCmd(db : String) extends Command(db, BSONObject("invalidateUserCache" -> 1))
