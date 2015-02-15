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
import rxmongo.driver.{ Command, WriteConcern }

/** createUser
  * Creates a new user.
  * @see [[http://docs.mongodb.org/master/reference/command/createUser/#dbcmd.createUser]]
  * @param db THe name of the database on which the user will be created
  * @param userName The name of the new user.
  * @param password The user’s password. The pwd field is not required if you run createUser on the \$external
  *           database to create users who have credentials stored externally to MongoDB.
  * @param customData Optional. Any arbitrary information. This field can be used to store any data an admin wishes
  *             to associate with this particular user. For example, this could be the user’s full name or
  *             employee id.
  * @param thisDbRole The roles granted to the user. Can specify an empty array [] to create users without roles.
  * @param roles Optional. When true, the mongod instance will create the hash of the user password; otherwise, the
  *        client is responsible for creating the hash of the password. Defaults to true.
  * @param writeConcern Optional. The level of write concern for the creation operation. The writeConcern document
  *               takes the same fields as the getLastError command.
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
  val theRoles : Seq[Role] = Role(thisDbRole, db) +: roles
  b.array("roles", theRoles)
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.toBSONObject) }
  b.result
})

/** updateUser
  * Update an existing user.
  * @see [[http://docs.mongodb.org/master/reference/command/updateUser/]]
  * @param db The name of the database for the user.
  * @param userName The name of the user to update.
  * @param pwd Optional. The user’s password.
  * @param customData	Optional. Any arbitrary information.
  * @param roles Optional. The roles granted to the user. An update to the roles array overrides the previous array’s
  *        values.
  * @param digestPassword	Optional. When true, the mongod instance will create the hash of the user password;
  *                 otherwise, the client is responsible for creating the hash of the password. Defaults to true.
  * @param writeConcern	Optional. The level of write concern for the update operation. The writeConcern document takes
  *               the same fields as the getLastError command.
  */
case class UpdateUserCmd(
  db : String,
  userName : String,
  pwd : Option[String] = None,
  customData : Option[BSONObject] = None,
  thisDbRole : Option[String] = None,
  roles : Option[Seq[Role]] = None,
  digestPassword : Option[Boolean] = None,
  writeConcern : Option[WriteConcern] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("updateUser", userName)
  pwd.map { p ⇒ b.string("pwd", p) }
  customData.map { cd ⇒ b.obj("customData", cd) }
  val theRoles : Seq[Role] = thisDbRole.map { r ⇒ Role(r, db) }.toSeq ++ roles.getOrElse(Seq.empty[Role])
  theRoles.map { r ⇒ b.array("roles", r) }
  digestPassword.map { dp ⇒ b.boolean("digestPassword", dp) }
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.result) }
  b.result
})

/** dropUser
  * Removes the user from the database on which you run the command.
  * @see [[http://docs.mongodb.org/master/reference/command/dropUser/]]
  * TODO: DropUserCmd
  */
case class DropUserCmd(
  db : String,
  userName : String,
  writeConcern : Option[WriteConcern] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("dropUser", userName)
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.result) }
  b.result
})

/** dropAllUsersFromDatabase
  * Deletes all users associated with a database
  * @see [[http://docs.mongodb.org/master/reference/command/dropAllUsersFromDatabase/]]
  * TODO: DropAllUsersFromDatabaseCmd
  */
case class DropAllUsersFromDatabaseCmd(
  db : String,
  writeConcern : Option[WriteConcern] = None) extends Command(db, {
  val b = BSONBuilder()
  b.integer("dropAllUsersFromDatabase", 1)
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.result) }
  b.result
})

/** grantRolesToUsers
  * Grants a role and its privileges to a user.
  * @see [[http://docs.mongodb.org/master/reference/command/grantRolesToUser/]]
  * TODO: GrantRolesToUsersCmd
  */
case class GrantRolesToUsersCmd(
  db : String,
  userName : String,
  thisDbRole : Option[String] = None,
  roles : Option[Seq[Role]] = None,
  writeConcern : Option[WriteConcern] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("grantRolesToUsers", userName)
  val theRoles : Seq[Role] = thisDbRole.map { r ⇒ Role(r, db) }.toSeq ++ roles.getOrElse(Seq.empty[Role])
  theRoles.map { r ⇒ b.array("roles", r) }
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.result) }
  b.result
})

/** revokeRolesFromUser
  * Removes a role from a user.
  * @see [[http://docs.mongodb.org/master/reference/command/revokeRolesFromUser/]]
  * TODO: RevokeRolesFromUserCmd
  */
case class RevokeRolesFromUserCmd(
  db : String,
  userName : String,
  thisDbRole : Option[String] = None,
  roles : Option[Seq[Role]] = None,
  writeConcern : Option[WriteConcern] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("revokeRolesFromUser", userName)
  val theRoles : Seq[Role] = thisDbRole.map { r ⇒ Role(r, db) }.toSeq ++ roles.getOrElse(Seq.empty[Role])
  theRoles.map { r ⇒ b.array("roles", r) }
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.result) }
  b.result
})

/** usersInfo
  * Returns information about the specified users.
  * @see [[http://docs.mongodb.org/master/reference/command/usersInfo/]]
  */
case class UserInfoCmd(
  db : String,
  userName : String,
  showCredentials : Boolean = false,
  showPrivileges : Boolean = false) extends Command(db, BSONObject(
  "usersInfo" → userName, "showCredentials" → showCredentials, "showPrivileges" → showPrivileges)
)

case class UsersInfoCmd(
  db : String,
  userNames : Seq[(String, String)],
  showCredentials : Boolean = false,
  showPrivileges : Boolean = false) extends Command(db, {
  val b = BSONBuilder()
  val objs = userNames.map { case (name, dbName) ⇒ BSONObject("user" → name, "db" → dbName) }
  b.array("usersInfo", objs)
  b.boolean("showCredentials", showCredentials)
  b.boolean("showPrivileges", showPrivileges)
  b.result
})
