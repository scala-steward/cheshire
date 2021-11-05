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

import algebra.ring.Field
import cats.Monad
import cats.kernel.Eq
import cats.kernel.Order
import cats.kernel.laws.discipline.CommutativeSemigroupTests
import cats.syntax.all.*
import cheshire.likelihood.laws.testkit.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.numeric.Positive
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

object PartitionTests:
  def apply[F[_]: Monad, R: Field: Order](partition: Partition[F, R]): PartitionTests[F, R] =
    new PartitionTests(PartitionLaws(partition)) {}

trait PartitionTests[F[_], R: Order](
    val laws: PartitionLaws[F, R]
) extends Laws:
  self =>

  type PositiveR = R Refined Positive
  type NonNegativeR = R Refined NonNegative

  import laws.F
  import laws.*
  import laws.partition.*

  def partition(
      using Eq[F[R]],
      Eq[F[LikelihoodEvaluation[R]]],
      Eq[F[Ppv]],
      Eq[F[Clv]],
      Arbitrary[R],
      Arbitrary[PositiveR],
      Arbitrary[NonNegativeR],
      Arbitrary[Freqs[R]],
      Arbitrary[Params[R]],
      Arbitrary[NodeHeights[R]],
      Arbitrary[F[Ppv]],
      Arbitrary[F[Clv]]): RuleSet =

    given arbModel: Arbitrary[F[Model]] = arbitraryModel(laws.partition)
    given arbMatrix: Arbitrary[F[Matrix]] = arbitraryMatrix(laws.partition)

    new:
      val name = "partition"
      val bases = Nil
      val parents = Nil
      val props = List(
        "mean rate is rate" -> forAll {
          (freqs: Freqs[R], params: Params[R], rate: PositiveR, alpha: PositiveR) =>
            laws.meanRate(freqs.freqs, params.params, rate, alpha)
        },
        "forecast identity" -> forAll(laws.forecastIdentity),
        "equilibrium identity" -> forAll { (model: F[Model], t: PositiveR) =>
          laws.equilibriumIdentity(model, t)
        },
        "forecast scale invariance" -> forAll {
          (
              ppv: F[Ppv],
              freqs: Freqs[R],
              params: Params[R],
              alpha: PositiveR,
              x: PositiveR,
              y: PositiveR) =>
            laws.forecastScaleInvariance(ppv, freqs.freqs, params.params, alpha, x, y)
        },
        "forecast compatibility" -> forAll {
          (model: F[Model], ppv: F[Ppv], s: PositiveR, t: PositiveR) =>
            laws.forecastCompatibility(model, ppv, s, t)
        },
        "forecast commutativity" -> forAll {
          (model: F[Model], ppv: F[Ppv], s: PositiveR, t: PositiveR) =>
            laws.forecastCommutativity(model, ppv, s, t)
        },
        "backcast identity" -> forAll(laws.backcastIdentity),
        "backcast scale invariance" -> forAll {
          (
              clv: F[Clv],
              freqs: Freqs[R],
              params: Params[R],
              alpha: PositiveR,
              x: PositiveR,
              y: PositiveR) =>
            laws.backcastScaleInvariance(clv, freqs.freqs, params.params, alpha, x, y)
        },
        "backcast compatibility" -> forAll {
          (model: F[Model], clv: F[Clv], s: PositiveR, t: PositiveR) =>
            laws.backcastCompatibility(model, clv, s, t)
        },
        "backcast commutativity" -> forAll {
          (model: F[Model], clv: F[Clv], s: PositiveR, t: PositiveR) =>
            laws.backcastCommutativity(model, clv, s, t)
        },
        "ppv product compatibility" -> forAll(laws.ppvProductCompatibility)
      ) ++ (new CommutativeSemigroupTests[F[Clv]] { val laws = self.laws.clvProductLaws })
        .commutativeSemigroup
        .all
        .properties ++
        List(
          "backcast product consistency" -> forAll(laws.backcastProductConsistency),
          "backcast product commutativity" -> forAll(laws.backcastProductCommutativity),
          "seed and integrate consistency" -> forAll(laws.seedAndIntegrateConsistency),
          "edge likelihood consistency" -> forAll {
            (model: F[Model], ppv: F[Ppv], clv: F[Clv], t: NonNegativeR) =>
              laws.edgeLikelihoodConsistency(model, ppv, clv, t)
          },
          "edge likelihood derivatives" -> forAll {
            (model: F[Model], ppv: F[Ppv], clv: F[Clv], t: NonNegativeR) =>
              laws.edgeLikelihoodDerivatives(model, ppv, clv, t)
          },
          "node likelihood consistency" -> forAll {
            (
                model: F[Model],
                ppv: F[Ppv],
                leftClv: F[Clv],
                rightClv: F[Clv],
                heights: NodeHeights[R]
            ) =>
              import heights.*
              laws.nodeLikelihoodConsistency(
                model,
                ppv,
                parentHeight,
                leftClv,
                leftHeight,
                rightClv,
                rightHeight,
                t)
          },
          "node likelihood derivatives" -> forAll {
            (
                model: F[Model],
                ppv: F[Ppv],
                leftClv: F[Clv],
                rightClv: F[Clv],
                heights: NodeHeights[R]
            ) =>
              import heights.*
              laws.nodeLikelihoodDerivatives(
                model,
                ppv,
                parentHeight,
                leftClv,
                leftHeight,
                rightClv,
                rightHeight,
                t)
          }
        )
