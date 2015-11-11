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

import akka.actor.{ ActorSystem, Actor }
import akka.event.LoggingAdapter
import com.typesafe.scalalogging.Logger
import org.slf4j.Marker

abstract class ActorBase extends Actor {

  final private val adapterLogger = AdapterLogger(context.system, this)
  final private val log = Logger(adapterLogger)

}

case class AdapterLogger(system : ActorSystem, logger : Actor) extends org.slf4j.Logger {
  private val akkaLogger : LoggingAdapter = akka.event.Logging(system, logger)

  override def getName : String = ???

  override def warn(msg : String) : Unit = ???

  override def warn(format : String, arg : scala.Any) : Unit = ???

  override def warn(format : String, arguments : AnyRef*) : Unit = ???

  override def warn(format : String, arg1 : scala.Any, arg2 : scala.Any) : Unit = ???

  override def warn(msg : String, t : Throwable) : Unit = ???

  override def warn(marker : Marker, msg : String) : Unit = ???

  override def warn(marker : Marker, format : String, arg : scala.Any) : Unit = ???

  override def warn(marker : Marker, format : String, arg1 : scala.Any, arg2 : scala.Any) : Unit = ???

  override def warn(marker : Marker, format : String, arguments : AnyRef*) : Unit = ???

  override def warn(marker : Marker, msg : String, t : Throwable) : Unit = ???

  override def isErrorEnabled : Boolean = ???

  override def isErrorEnabled(marker : Marker) : Boolean = ???

  override def isInfoEnabled : Boolean = ???

  override def isInfoEnabled(marker : Marker) : Boolean = ???

  override def isDebugEnabled : Boolean = ???

  override def isDebugEnabled(marker : Marker) : Boolean = ???

  override def isTraceEnabled : Boolean = ???

  override def isTraceEnabled(marker : Marker) : Boolean = ???

  override def error(msg : String) : Unit = ???

  override def error(format : String, arg : scala.Any) : Unit = ???

  override def error(format : String, arg1 : scala.Any, arg2 : scala.Any) : Unit = ???

  override def error(format : String, arguments : AnyRef*) : Unit = ???

  override def error(msg : String, t : Throwable) : Unit = ???

  override def error(marker : Marker, msg : String) : Unit = ???

  override def error(marker : Marker, format : String, arg : scala.Any) : Unit = ???

  override def error(marker : Marker, format : String, arg1 : scala.Any, arg2 : scala.Any) : Unit = ???

  override def error(marker : Marker, format : String, arguments : AnyRef*) : Unit = ???

  override def error(marker : Marker, msg : String, t : Throwable) : Unit = ???

  override def debug(msg : String) : Unit = ???

  override def debug(format : String, arg : scala.Any) : Unit = ???

  override def debug(format : String, arg1 : scala.Any, arg2 : scala.Any) : Unit = ???

  override def debug(format : String, arguments : AnyRef*) : Unit = ???

  override def debug(msg : String, t : Throwable) : Unit = ???

  override def debug(marker : Marker, msg : String) : Unit = ???

  override def debug(marker : Marker, format : String, arg : scala.Any) : Unit = ???

  override def debug(marker : Marker, format : String, arg1 : scala.Any, arg2 : scala.Any) : Unit = ???

  override def debug(marker : Marker, format : String, arguments : AnyRef*) : Unit = ???

  override def debug(marker : Marker, msg : String, t : Throwable) : Unit = ???

  override def isWarnEnabled : Boolean = ???

  override def isWarnEnabled(marker : Marker) : Boolean = ???

  override def trace(msg : String) : Unit = ???

  override def trace(format : String, arg : scala.Any) : Unit = ???

  override def trace(format : String, arg1 : scala.Any, arg2 : scala.Any) : Unit = ???

  override def trace(format : String, arguments : AnyRef*) : Unit = ???

  override def trace(msg : String, t : Throwable) : Unit = ???

  override def trace(marker : Marker, msg : String) : Unit = ???

  override def trace(marker : Marker, format : String, arg : scala.Any) : Unit = ???

  override def trace(marker : Marker, format : String, arg1 : scala.Any, arg2 : scala.Any) : Unit = ???

  override def trace(marker : Marker, format : String, argArray : AnyRef*) : Unit = ???

  override def trace(marker : Marker, msg : String, t : Throwable) : Unit = ???

  override def info(msg : String) : Unit = ???

  override def info(format : String, arg : scala.Any) : Unit = ???

  override def info(format : String, arg1 : scala.Any, arg2 : scala.Any) : Unit = ???

  override def info(format : String, arguments : AnyRef*) : Unit = ???

  override def info(msg : String, t : Throwable) : Unit = ???

  override def info(marker : Marker, msg : String) : Unit = ???

  override def info(marker : Marker, format : String, arg : scala.Any) : Unit = ???

  override def info(marker : Marker, format : String, arg1 : scala.Any, arg2 : scala.Any) : Unit = ???

  override def info(marker : Marker, format : String, arguments : AnyRef*) : Unit = ???

  override def info(marker : Marker, msg : String, t : Throwable) : Unit = ???
}
