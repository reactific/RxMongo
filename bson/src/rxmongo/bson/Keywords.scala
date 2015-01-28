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

package rxmongo.bson

object Keywords {

  sealed trait Keyword { def value : String }

  // Aggregation Pipeline Keywords
  object $geoNear extends Keyword { def value = "$geoNear" }
  object $group extends Keyword { def value = "$group" }
  object $limit extends Keyword { def value = "$limit" }
  object $match extends Keyword { def value = "$match" }
  object $out extends Keyword { def value = "$out" }
  object $project extends Keyword { def value = "$project" }
  object $redact extends Keyword { def value = "$redact" }
  object $skip extends Keyword { def value = "$skip" }
  object $sort extends Keyword { def value = "$sort " }
  object $unwind extends Keyword { def value = "$unwind" }

}
