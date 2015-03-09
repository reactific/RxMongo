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

import scala.concurrent.duration._

/** RxMongo Retry Strategy.
  *
  * This represents the strategy for retrying database operations under certain conditions. When a database operation
  * fails because a replica set became unavailable, the RetryStrategy is used to determine how many times to repeat the
  * operation before giving up with a failure. This approach provides high availability and fault tolerance by
  * eliminating "normal" failures that can be recovered by doing a simple retry.
  */
class RetryStrategy extends Iterator[FiniteDuration] {
  val maxRetries : Int = 5
  protected var retry : Int = 0
  def hasNext : Boolean = { retry < maxRetries }
  def next() : FiniteDuration = {
    if (hasNext)
      throw new NoSuchElementException("Retries exhausted")
    retry += 1
    nextDelay
  }
  def nextDelay : FiniteDuration = 500.millis
  def reset : Unit = { retry = 0 }
}

object RetryStrategy {
  def apply(retries : Int = 5) : RetryStrategy = new RetryStrategy { override val maxRetries = retries }
}

/** Simple Retry Strategy
  * This retry strategy simply differentiates between the initial delay and subsequent delays
  * @param initialDelay The initial delay before the first retry (defaults to 250ms)
  * @param repeatDelay The subsequent delay after the first retry (defaults to 500ms)
  * @param maxRetries The maximum number of retries to attempt (defaults to 5)
  */
case class SimpleRetryStrategy(
  initialDelay : FiniteDuration = 250.millis,
  repeatDelay : FiniteDuration = 500.millis,
  override val maxRetries : Int = 5) extends RetryStrategy {

  override def nextDelay : FiniteDuration = {
    if (retry == 1)
      initialDelay
    else
      repeatDelay
  }
}

/** Linear Retry Strategy
  * A retry strategy that increases the delay for each retry linearly in an arithmetic progression
  * @param delay The amount of delay to increase on each successive attempt (defaults to 250ms)
  * @param maxRetries The maximum number of retries to attempt (defaults to 5)
  */
case class LinearRetryStrategy(
  delay : FiniteDuration = 250.millis,
  override val maxRetries : Int = 5) extends RetryStrategy {
  override def nextDelay : FiniteDuration = {
    delay * retry
  }
}

/** Fibonacci Retry Strategy
  * A retry strategy that uses the Fibonacci series to increase the delay. The series is multiplied by the
  * delay so you get 1*delay, 1*delay, 2*delay, 3*delay, 5*delay, 8*delay, etc.
  * @param delay The delay factor to multiply the fibonacci value with to arrive at a delay (defaults to 250ms)
  * @param maxRetries Maximum number of retries to attempt (defaults to 5)
  */
case class FibonacciRetryStrategy(
  delay : FiniteDuration = 250.millis,
  override val maxRetries : Int = 5) extends RetryStrategy {
  var last = 1
  var lastLast = 0
  override def nextDelay : FiniteDuration = {
    val nextLast = last + lastLast
    lastLast = last
    last = nextLast
    delay * nextLast
  }
  override def reset : Unit = { super.reset; last = 1; lastLast = 0 }
}

/** Geometric Retry Strategy
  * A retry strategy that increases the delay geometrically by multiplying the base delay by the square of the
  * retry number (1, 4, 9, 16, 25...)
  * @param delay The base delay factor that is multiplied by a geometrically increasing number.
  * @param maxRetries The maximum number of retry to attempt (defaults to 5)
  */
case class GeometricRetryStrategy(
  delay : FiniteDuration = 250.millis,
  override val maxRetries : Int = 5) extends RetryStrategy {
  override def nextDelay : FiniteDuration = {
    delay * retry * retry
  }
}
