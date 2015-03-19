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

import rxmongo.bson.BSONObject
import rxmongo.messages.{ AuthMechanism, Command }

/** logout
  * @see [[http://docs.mongodb.org/master/reference/command/logout]]
  * @param db
  */
case class LogoutCmd(db : String) extends Command(db, BSONObject("logout" → 1))

/** authenticate
  * Starts an authenticated session using a username and password.
  * @see [[http://docs.mongodb.org/master/reference/command/authenticate/#dbcmd.authenticate]]
  * @param db
  * @param user
  * @param pass
  * @param mechanism
  */
case class AuthenticateCmd(
  db : String,
  user : String,
  pass : String,
  mechanism : AuthMechanism) extends Command(db, BSONObject(
  "authenticate" -> 1, "username" -> user, "password" -> pass, "mechanism" -> mechanism.asStr)
)

/** copydbgetnonce
  * This is an internal command to generate a one-time password for use with the copydb command.
  * @see [[http://docs.mongodb.org/master/reference/command/copydbgetnonce/#dbcmd.copydbgetnonce]]
  * @param db
  */
case class CopyDbGetNonceCmd(
  db : String) extends Command(db, BSONObject("copydbgetnonce" -> 1))

/** getnonce
  * This is an internal command to generate a one-time password for authentication.
  * @see [[http://docs.mongodb.org/master/reference/command/getnonce/#dbcmd.getnonce]]
  * @param db The name of the database
  */
case class GetNonceCmd(
  db : String) extends Command(db, BSONObject("getnonce" -> 1))

