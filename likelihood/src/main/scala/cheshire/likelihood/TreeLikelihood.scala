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

package cheshire
package likelihood

import cats.Comonad
import cats.MonadThrow
import cats.data.Kleisli
import cats.syntax.all.*
import cats.data.StateT
import cats.kernel.Group

trait TreeLikelihood[F[_], R]:

  def logLikelihoodC[N[_]: Comonad, L[_]: Comonad](
      partition: Partition[F, R],
      model: partition.Model,
      matrices: List[partition.Matrix],
      clvs: List[partition.NodeClv]
  )(tree: GenTree[N[R], L[(R, partition.Clv)]]): F[R]

object TreeLikelihood:

  given [F[_], R: Group](using F: MonadThrow[F]): TreeLikelihood[F, R] with

    def logLikelihoodC[N[_]: Comonad, L[_]: Comonad](
        partition: Partition[F, R],
        model: partition.Model,
        matrices: List[partition.Matrix],
        clvs: List[partition.NodeClv]
    )(tree: GenTree[N[R], L[(R, partition.Clv)]]): F[R] =

      def nextMatrix = for
        (matrices, clvs) <- StateT.get[F, (List[partition.Matrix], List[partition.NodeClv])]
        head <- StateT.liftF(F.fromOption(matrices.headOption, new NoSuchElementException))
        tail = matrices.tail
        _ <- StateT.set(((tail, clvs)))
      yield head

      def nextClv = for
        (matrices, clvs) <- StateT.get[F, (List[partition.Matrix], List[partition.NodeClv])]
        head <- StateT.liftF(F.fromOption(clvs.headOption, new NoSuchElementException))
        tail = clvs.tail
        _ <- StateT.set(((matrices, tail)))
      yield head

      tree
        .reducePostOrderM[
          StateT[F, (List[partition.Matrix], List[partition.NodeClv]), _],
          (R, partition.Clv)] { l => l.extract.pure } {
          case ((leftHeight, leftClv), (rightHeight, rightClv), n) =>
            val height = n.extract
            for
              leftMatrix <- nextMatrix
              _ <- StateT.liftF(
                partition.computeMatrix(model, height |-| leftHeight, leftMatrix))
              rightMatrix <- nextMatrix
              _ <- StateT.liftF(
                partition.computeMatrix(model, height |-| rightHeight, rightMatrix))
              clv <- nextClv
              _ <- StateT.liftF(
                partition.backcastProduct(leftClv, leftMatrix, rightClv, rightMatrix, clv))
            yield (height, clv)
        }
        .runA((matrices, clvs))
        .flatMap { (_, clv) => partition.seedAndIntegrate(model, clv) }
