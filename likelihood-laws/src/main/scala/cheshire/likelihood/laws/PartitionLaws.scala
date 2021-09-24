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

import cats.Monad
import cats.effect.kernel.MonadCancelThrow
import cats.effect.kernel.syntax.all.*
import cats.kernel.Group
import cats.kernel.laws.*
import cats.syntax.all.*

trait PartitionLaws[F[_]: MonadCancelThrow, R: Group]:

  def forecastAssociative(
      partition: Partition[F, R],
      model: partition.Model,
      ppv: partition.Ppv,
      s: R,
      t: R
  ): F[IsEq[partition.Ppv]] = partition.allocate(0, 3, 3, 0).use { (_, matrices, ppvs, _) =>
    for
      _ <- partition.computeMatrix(model, s |+| t, matrices(0))
      _ <- partition.computeMatrix(model, s, matrices(1))
      _ <- partition.computeMatrix(model, t, matrices(2))
      _ <- partition.forecast(ppv, matrices(0), ppvs(0))
      _ <- partition.forecast(ppv, matrices(1), ppvs(1))
      _ <- partition.forecast(ppvs(1), matrices(2), ppvs(2))
    yield ppvs(0) <-> ppvs(2)
  }

  def backcastAssociative(
      partition: Partition[F, R],
      model: partition.Model,
      clv: partition.Clv,
      s: R,
      t: R
  ): F[IsEq[partition.Clv]] = partition.allocate(0, 3, 0, 3).use { (_, matrices, _, clvs) =>
    for
      _ <- partition.computeMatrix(model, s |+| t, matrices(0))
      _ <- partition.computeMatrix(model, s, matrices(1))
      _ <- partition.computeMatrix(model, t, matrices(2))
      _ <- partition.backcast(clv, matrices(0), clvs(0))
      _ <- partition.backcast(clv, matrices(1), clvs(1))
      _ <- partition.backcast(clvs(1), matrices(2), clvs(2))
    yield clvs(0) <-> clvs(2)
  }

  def edgeLikelihoodConsistent(
      partition: Partition[F, R],
      model: partition.Model,
      ppv: partition.Ppv,
      clv: partition.Clv,
      t: R
  ): F[IsEq[R]] = partition.allocate(0, 1, 0, 1).use { (_, matrices, _, clvs) =>
    for
      _ <- partition.computeMatrix(model, t, matrices(0))
      _ <- partition.backcast(clv, matrices(0), clvs(0))
      expected <- partition.integrateProduct(ppv, clv)
      got <- partition.edgeLikelihood(model, ppv, clv)(t).use(_.logLikelihood.pure)
    yield got <-> expected
  }

  def nodeLikelihoodConsistent(
      partition: Partition[F, R],
      model: partition.Model,
      ppv: partition.Ppv,
      parentHeight: R,
      leftClv: partition.Clv,
      leftHeight: R,
      rightClv: partition.Clv,
      rightHeight: R,
      t: R
  ): F[IsEq[R]] = partition.allocate(0, 3, 0, 2).use { (_, matrices, _, clvs) =>
    for
      _ <- partition.computeMatrix(model, parentHeight |-| t, matrices(0))
      _ <- partition.computeMatrix(model, t |-| leftHeight, matrices(1))
      _ <- partition.computeMatrix(model, t |-| rightHeight, matrices(2))
      _ <- partition.backcastProduct(leftClv, matrices(1), rightClv, matrices(2), clvs(0))
      _ <- partition.backcast(clvs(0), matrices(0), clvs(1))
      expected <- partition.integrateProduct(ppv, clvs(2))
      got <- partition
        .nodeLikelihood(model, ppv, parentHeight, leftClv, leftHeight, rightClv, rightHeight)(t)
        .use(_.logLikelihood.pure)
    yield got <-> expected
  }
