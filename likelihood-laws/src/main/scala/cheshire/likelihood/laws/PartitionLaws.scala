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
import cats.kernel.laws.*
import cats.syntax.all.*

trait PartitionLaws[F[_]: Monad, R: Field]:

  def edgeLikelihoodConsistent(
      partition: Partition[F, F, R],
      model: partition.Model,
      ppv: partition.Ppv,
      clv: partition.Clv,
      t: R
  ): F[IsEq[R]] = for
    (_, matrices, _, clvs) <- partition.allocate(0, 1, 0, 1)
    matrix = matrices(0)
    clv1 = clvs(0)
    _ <- partition.computeMatrix(model, t, matrix)
    _ <- partition.backcast(clv, matrix, clv1)
    expected <- partition.integrateProduct(ppv, clv)
    got <- partition.edgeLikelihood(model, ppv, clv)(t).map(_.logLikelihood)
  yield got <-> expected
