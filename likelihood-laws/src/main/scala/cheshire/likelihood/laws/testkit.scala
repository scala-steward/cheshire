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
package laws.testkit

import cats.Monad
import cats.syntax.all.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.numeric.Positive
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

final case class Freqs[R](freqs: IndexedSeq[R])
object Freqs:
  given Arbitrary[Freqs[Double]] = Arbitrary(
    Gen.listOfN(4, Gen.exponential(1)).map { x =>
      val sum = x.sum
      Freqs(x.map(_ / sum).toVector)
    }
  )

final case class Params[R](params: IndexedSeq[R])
object Params:
  given Arbitrary[Params[Double]] = Arbitrary(
    Gen.listOfN(6, Gen.exponential(1)).map { x =>
      val last = x.last
      Params(x.map(_ / last).toVector)
    }
  )

final case class NodeHeights[R](
    leftHeight: R,
    rightHeight: R,
    parentHeight: R,
    t: R
)
object NodeHeights:
  given Arbitrary[NodeHeights[Double]] = Arbitrary(
    for
      leftHeight <- Gen.exponential(1)
      rightHeight <- Gen.exponential(1)
      maxChildHeight = math.max(leftHeight, rightHeight)
      parentHeight <- Gen.exponential(1).map(_ + maxChildHeight)
      t <- Gen.chooseNum(0, 1).map(maxChildHeight + (parentHeight - maxChildHeight) * _)
    yield NodeHeights(leftHeight, rightHeight, parentHeight, t)
  )

def arbitraryModel[F[_], R](partition: Partition[F, R])(
    using Arbitrary[Freqs[R]],
    Arbitrary[Params[R]],
    Arbitrary[R Refined Positive]): Arbitrary[F[partition.Model]] =
  Arbitrary(
    for
      case Freqs(freqs) <- Arbitrary.arbitrary[Freqs[R]]
      case Params(params) <- Arbitrary.arbitrary[Params[R]]
      rate <- Arbitrary.arbitrary[R Refined Positive]
      alpha <- Arbitrary.arbitrary[R Refined Positive]
    yield partition.model(freqs, params, rate, alpha)
  )

def arbitraryMatrix[F[_]: Monad, R](partition: Partition[F, R])(
    using Arbitrary[F[partition.Model]],
    Arbitrary[R Refined NonNegative]): Arbitrary[F[partition.Matrix]] =
  Arbitrary(
    for
      model <- Arbitrary.arbitrary[F[partition.Model]]
      t <- Arbitrary.arbitrary[R Refined NonNegative]
    yield model.flatMap(partition.matrix(_, t))
  )
