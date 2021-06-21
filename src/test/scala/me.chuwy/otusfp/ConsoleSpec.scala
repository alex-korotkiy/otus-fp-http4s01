/*
 * Copyright (c) 2012-2020 Snowplow Analytics Ltd. All rights reserved.
 *
 * This program is licensed to you under the Apache License Version 2.0,
 * and you may not use this file except in compliance with the Apache License Version 2.0.
 * You may obtain a copy of the Apache License Version 2.0 at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the Apache License Version 2.0 is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the Apache License Version 2.0 for the specific language governing permissions and limitations there under.
 */
package me.chuwy.otusfp

import cats.data.State

import org.specs2.mutable.Specification
import cats.implicits._

import algebra.Console._

class ConsoleSpec extends Specification {
  "ConsoleSpec" should {
    "return bob" in {
      val doubleGreet = greetSomeone[WorldState] *> greetSomeone[WorldState]
      val (state, _) = doubleGreet.run(initial).value

      // Тест валится
      state.events.reverse must beEmpty
    }
  }
}
