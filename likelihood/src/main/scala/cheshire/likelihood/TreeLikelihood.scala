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

import cats.Monad
import cats.data.Kleisli
import cats.data.OptionT
import cats.data.StateT
import cats.kernel.Group
import cats.syntax.all.*

import TreeLikelihood.*

trait TreeLikelihood[F[_], R]:

  def logLikelihood[R: Group, N, L](
      partition: PartitionKernel[F, R],
      getN: N => PostOrderNode[R, partition.Matrix, partition.NodeClv],
      getL: L => PostOrderLeaf[R, partition.Matrix, partition.Clv]
  )(model: partition.Model, tree: GenTree[N, L]): F[R]

  def populate[R: Group, N, L](
      partition: PartitionKernel[F, R],
      getN: N => PreOrderNode[R, partition.Matrix, partition.Ppv, partition.NodeClv],
      getL: L => PostOrderLeaf[R, partition.Matrix, partition.Clv]
  )(
      model: partition.Model,
      rootParent: Option[PreOrderRootParent[R, partition.Matrix, partition.Ppv]],
      tree: GenTree[N, L]): F[Unit]

object TreeLikelihood:

  sealed abstract private[TreeLikelihood] class HeightClv[R, Clv]:
    def height: R
    def clv: Clv

  final case class PostOrderNode[R, Matrix, Clv](
      height: R,
      clv: Clv,
      leftMatrix: Matrix,
      rightMatrix: Matrix
  ) extends HeightClv[R, Clv]

  final case class PostOrderLeaf[R, Matrix, Clv](
      height: R,
      clv: Clv
  ) extends HeightClv[R, Clv]

  final case class PreOrderNode[R, Matrix, Ppv, Clv](
      height: R,
      clv: Clv,
      leftClv: Clv,
      rightClv: Clv,
      ppv: Ppv,
      leftPpv: Ppv,
      rightPpv: Ppv,
      leftMatrix: Matrix,
      rightMatrix: Matrix
  ) extends HeightClv[R, Clv]

  final case class PreOrderRootParent[R, Matrix, Ppv](
      height: R,
      parentPpv: Ppv,
      matrix: Matrix
  )

  def sequential[F[_], R: Group](using F: Monad[F]): TreeLikelihood[F, R] =
    new TreeLikelihood[F, R]:
      def logLikelihood[R: Group, N, L](
          partition: PartitionKernel[F, R],
          getN: N => PostOrderNode[R, partition.Matrix, partition.NodeClv],
          getL: L => PostOrderLeaf[R, partition.Matrix, partition.Clv]
      )(model: partition.Model, tree: GenTree[N, L]): F[R] =
        tree
          .reducePostOrderM(getL(_).pure) {
            case (
                  PostOrderLeaf(leftHeight, leftClv),
                  PostOrderLeaf(rightHeight, rightClv),
                  n) =>
              val PostOrderNode(height, clv, leftMatrix, rightMatrix) = getN(n)
              val leftLength = height |-| leftHeight
              val rightLength = height |-| rightHeight
              partition.computeMatrix(model, leftLength, leftMatrix) *>
                partition.computeMatrix(model, rightLength, rightMatrix) *>
                partition
                  .backcastProduct(leftClv, leftMatrix, rightClv, rightMatrix, clv)
                  .as(PostOrderLeaf(height, clv))
          }
          .flatMap {
            case PostOrderLeaf(_, clv) =>
              partition.seedAndIntegrate(model, clv)
          }

      def populate[R: Group, N, L](
          partition: PartitionKernel[F, R],
          getN: N => PreOrderNode[R, partition.Matrix, partition.Ppv, partition.NodeClv],
          getL: L => PostOrderLeaf[R, partition.Matrix, partition.Clv]
      )(
          model: partition.Model,
          rootParent: Option[PreOrderRootParent[R, partition.Matrix, partition.Ppv]],
          tree: GenTree[N, L]): F[Unit] =
        import GenTree.*
        tree.postOrder { button =>
          button.at match
            case Node(n, left, right) =>
              val PreOrderNode(height, clv, leftClv, rightClv, _, _, _, leftMat, rightMat) =
                getN(n)
              val leftHeightClv = left.valueEither.fold(getN(_), getL(_))
              val rightHeightClv = right.valueEither.fold(getN(_), getL(_))
              val leftLength = height |-| leftHeightClv.height
              val rightLength = height |-| rightHeightClv.height
              partition.computeMatrix(model, leftLength, leftMat) *>
                partition.computeMatrix(model, rightLength, rightMat) *>
                partition.backcast(leftHeightClv.clv, leftMat, leftClv) *>
                partition.backcast(rightHeightClv.clv, rightMat, rightClv) *>
                partition.product(leftClv, rightClv, clv)
            case Leaf(_) => F.unit
        } *> OptionT
          .fromOption(rootParent)
          .foldF(tree.valueEither.fold(n => partition.seed(model, getN(n).ppv), _ => F.unit)) {
            case PreOrderRootParent(height, ppv, matrix) =>
              tree match
                case Node(n, _, _) =>
                  val PreOrderNode(childHeight, _, _, _, childPpv, _, _, _, _) = getN(n)
                  val length = height |-| childHeight
                  partition.computeMatrix(model, length, matrix) *>
                    partition.forecast(ppv, matrix, childPpv)
                case _ => F.unit
          } *> tree.preOrder { button =>
          button.at match
            case Node(n, left, right) =>
              val PreOrderNode(
                _,
                _,
                leftClv,
                rightClv,
                ppv,
                leftPpv,
                rightPpv,
                leftMat,
                rightMat) =
                getN(n)
              val leftChildPpv = left.valueEither.left.toOption.map(getN(_).ppv)
              val rightChildPpv = right.valueEither.left.toOption.map(getN(_).ppv)
              partition.product(ppv, rightClv, leftPpv) *>
                partition.product(ppv, leftClv, rightPpv) *>
                leftChildPpv.fold(F.unit)(partition.forecast(leftPpv, leftMat, _)) *>
                rightChildPpv.fold(F.unit)(partition.forecast(rightPpv, rightMat, _))
            case Leaf(_) => F.unit
        }
