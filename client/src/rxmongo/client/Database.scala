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

package rxmongo.client

import akka.util.Timeout
import rxmongo.driver.{WriteConcern}

/** Represents A MongoDB Database
  *
  * The methods of this class align with the mongo shell's naming.
  *
  * @see http://docs.mongodb.org/master/reference/method/js-database/
  * @param name
  * @param client
  */
case class Database(name : String, client : Client)
  (override implicit val timeout : Timeout = client.timeout,
   override implicit val writeConcern: WriteConcern = client.writeConcern)
  extends RxMongoComponent(client.driver) {

  def namespace = name
  def collection(name : String)
    (implicit to: Timeout = timeout, wc: WriteConcern = writeConcern) : Collection = {
    Collection(name, this)(to, wc)
  }

  /** Authenticates a user to a database. */
  def auth() = ???
  /** Changes an existing user’s password. */
  def changeUserPassword() = ???
  /** Copies data directly between MongoDB instances. Wraps cloneCollection. */
  def cloneCollection() = ???
  /** Copies a database from a remote host to the current host. Wraps clone. */
  def cloneDatabase() = ???
  /** Returns help information for a database command. */
  def commandHelp() = ???
  /** Copies a database to another database on the current host. Wraps copydb. */
  def copyDatabase() = ???
  /** Creates a new collection. Commonly used to create a capped collection. */
  def createCollection() = ???
  /** Reports the current in-progress operations. */
  def currentOp() = ???
  /** Removes the current database. */
  def dropDatabase() = ???
  /** Passes a JavaScript function to the mongod instance for server-side JavaScript evaluation. */
  def eval() = ???
  /** Flushes writes to disk and locks the database to prevent write operations and assist backup operations. Wraps fsync. */
  def fsyncLock() = ???
  /** Allows writes to continue on a database locked with db.fsyncLock(). */
  def fsyncUnlock() = ???
  /** Returns a collection object. Used to access collections with names that are not valid in the mongo shell. */
  def getCollection() = ???
  /** Returns collection information for all collections in the current database. */
  def getCollectionInfos() = ???
  /** Lists all collections in the current database. */
  def getCollectionNames() = ???
  /** Checks and returns the status of the last operation. Wraps getLastError. */
  def getLastError() = ???
  /** Returns the status document for the last operation. Wraps getLastError. */
  def getLastErrorObj() = ???
  /** Returns the log message verbosity levels. */
  def getLogComponents() = ???
  /** Returns the Mongo() connection object for the current connection. */
  def getMongo() = ???
  /** Returns the name of the current database. */
  def getName() = ???
  /** Returns a status document containing all errors since the last error reset. Wraps getPrevError. */
  def getPrevError() = ???
  /** Returns the current profiling level for database operations. */
  def getProfilingLevel() = ???
  /** Returns a document that reflects the current profiling level and the profiling threshold. */
  def getProfilingStatus() = ???
  /** Returns a document with replication statistics. */
  def getReplicationInfo() = ???
  /** Provides access to the specified database. */
  def getSiblingDB() = ???
  /** Displays descriptions of common db object methods. */
  def help() = ???
  /** Returns a document with information about the system MongoDB runs on. Wraps hostInfo */
  def hostInfo() = ???
  /** Returns a document that reports the state of the replica set. */
  def isMaster() = ???
  /** Terminates a specified operation. */
  def killOp() = ???
  /** Displays a list of common database commands. */
  def listCommands() = ???
  /** Loads all scripts in the system.js collection for the current database into the shell session. */
  def loadServerScripts() = ???
  /** Ends an authenticated session. */
  def logout() = ???
  /** Prints statistics from every collection. Wraps db.collection.stats(). */
  def printCollectionStats() = ???
  /** Prints a report of the status of the replica set from the perspective of the primary. */
  def printReplicationInfo() = ???
  /** Prints a report of the sharding configuration and the chunk ranges. */
  def printShardingStatus() = ???
  /** Prints a report of the status of the replica set from the perspective of the secondaries. */
  def printSlaveReplicationInfo() = ???
  /** Removes a user from a database. */
  def removeUser() = ???
  /** Runs a repair routine on the current database. */
  def repairDatabase() = ???
  /** Resets the error message returned by db.getPrevError() and getPrevError. */
  def resetError() = ???
  /** Runs a database command. */
  def runCommand() = ???
  /** Returns a document that displays the compilation parameters for the mongod instance. Wraps buildinfo. */
  def serverBuildInfo() = ???
  /** Returns a document with information about the runtime used to start the MongoDB instance. Wraps getCmdLineOpts. */
  def serverCmdLineOpts() = ???
  /** Returns a document that provides an overview of the state of the database process. */
  def serverStatus() = ???
  /** Sets a single log message verbosity level. */
  def setLogLevel() = ???
  /** Modifies the current level of database profiling. */
  def setProfilingLevel() = ???
  /** Shuts down the current mongod or mongos process cleanly and safely. */
  def shutdownServer() = ???
  /** Returns a document that reports on the state of the current database. */
  def stats() = ???
  /** Returns the version of the mongod instance. */
  def version() = ???
  /** Performs a preliminary check for upgrade preparedness for a specific database or collection. */
  def upgradeCheck() = ???
  /** Performs a preliminary check for upgrade preparedness for all databases and collections. */
  def upgradeCheckAllDBs() = ???

}

object Database {
  /** Convenience Constructior
    * In case the programmer gets the argument order wrong, they still get a database from it
    * @param client The client connection
    * @param name The name of the database to access
    * @return
    */
  def apply(client : Client, name : String) = new Database(name, client)
}
