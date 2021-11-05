/*
 * Copyright 2021 Arman Bilge
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cheshire.likelihood
package laws

import cheshire.likelihood.laws.testkit.Freqs
import cheshire.likelihood.laws.testkit.Params
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto.*
import eu.timepit.refined.numeric.Positive
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

trait PartitionTests[F[_], R](
    val laws: PartitionLaws[F, R]
) extends Laws:

  def partition(
      using Arbitrary[R Refined Positive],
      Arbitrary[Freqs[R]],
      Arbitrary[Params[R]],
      Arbitrary[F[laws.partition.Ppv]],
      Arbitrary[F[laws.partition.Clv]]): RuleSet =
    new:
      val name = "partition"
      val bases = Nil
      val parents = Nil
      val props = List(
      )
