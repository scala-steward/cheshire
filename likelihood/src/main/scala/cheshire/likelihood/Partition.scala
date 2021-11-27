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

import cats.effect.kernel.Resource
import cats.effect.kernel.syntax.all.*
import cats.syntax.all.*

import scala.annotation.targetName

trait Partition[F[_], R]:

  type Model
  type Matrix
  type Partial = Ppv | Clv
  type Ppv
  type Clv = NodeClv | TipClv
  type NodeClv
  type TipClv

  def categoryCount: Int

  def tips: IndexedSeq[TipClv]

  def model(freqs: IndexedSeq[R], params: IndexedSeq[R], rate: R, alpha: R): F[Model]

  def rates(model: Model): F[IndexedSeq[R]]

  def matrix(model: Model, t: R): F[Matrix]

  def forecast(x: Ppv, P: Matrix): F[Ppv]

  def backcast(y: Clv, P: Matrix): F[NodeClv]

  def backcastProduct(y: Clv, Py: Matrix, z: Clv, Pz: Matrix): F[NodeClv]

  @targetName("ppvProduct")
  def product(x: Ppv, y: Clv): F[Ppv]

  @targetName("clvProduct")
  def product(x: Clv, y: Clv): F[NodeClv]

  def seed(model: Model): F[Ppv]

  def integrateProduct(x: Ppv, y: Clv): F[R]

  def seedAndIntegrate(model: Model, x: Clv): F[R]

  def edgeLikelihood(model: Model, ppv: Ppv, clv: Clv)(t: R): F[LikelihoodEvaluation[R]]

  def nodeLikelihood(
      model: Model,
      ppv: Ppv,
      parentHeight: R,
      leftClv: Clv,
      leftHeight: R,
      rightClv: Clv,
      rightHeight: R)(t: R): F[LikelihoodEvaluation[R]]

object Partition:

  type Aux[F[_], R, Model0, Matrix0, Ppv0, NodeClv0, TipClv0] = Partition[F, R] {
    type Model = Model0
    type Matrix = Matrix0
    type Ppv = Ppv0
    type NodeClv = NodeClv0
    type TipClv = TipClv0
  }

  def fromKernel[F[_], R](partition: PartitionKernel[F, R]): Partition.Aux[
    Resource[F, _],
    R,
    partition.Model,
    partition.Matrix,
    partition.Ppv,
    partition.NodeClv,
    partition.TipClv] =
    new Partition[Resource[F, _], R]:
      type Model = partition.Model
      type Matrix = partition.Matrix
      type Ppv = partition.Ppv
      type NodeClv = partition.NodeClv
      type TipClv = partition.TipClv

      def categoryCount: Int = partition.categoryCount

      def tips: IndexedSeq[TipClv] = partition.tips

      def model(
          freqs: IndexedSeq[R],
          params: IndexedSeq[R],
          rate: R,
          alpha: R): Resource[F, Model] =
        partition.allocateModel.evalTap(partition.initModel(freqs, params, rate, alpha, _))

      def rates(model: Model): Resource[F, IndexedSeq[R]] = partition.rates(model).toResource

      def matrix(model: Model, t: R): Resource[F, Matrix] =
        partition.allocateMatrix.evalTap(partition.computeMatrix(model, t, _))

      def forecast(x: Ppv, P: Matrix): Resource[F, Ppv] =
        partition.allocatePpv.evalTap(partition.forecast(x, P, _))

      def backcast(y: Clv, P: Matrix): Resource[F, NodeClv] =
        partition.allocateClv.evalTap(partition.backcast(y, P, _))

      def backcastProduct(y: Clv, Py: Matrix, z: Clv, Pz: Matrix): Resource[F, NodeClv] =
        partition.allocateClv.evalTap(partition.backcastProduct(y, Py, z, Pz, _))

      @targetName("ppvProduct")
      def product(x: Ppv, y: Clv): Resource[F, Ppv] =
        partition.allocatePpv.evalTap(partition.product(x, y, _))

      @targetName("clvProduct")
      def product(x: Clv, y: Clv): Resource[F, NodeClv] =
        partition.allocateClv.evalTap(partition.product(x, y, _))

      def seed(model: Model): Resource[F, Ppv] =
        partition.allocatePpv.evalTap(partition.seed(model, _))

      def integrateProduct(x: Ppv, y: Clv): Resource[F, R] =
        partition.integrateProduct(x, y).toResource

      def seedAndIntegrate(model: Model, x: Clv): Resource[F, R] =
        partition.seedAndIntegrate(model, x).toResource

      def edgeLikelihood(model: Model, ppv: Ppv, clv: Clv)(
          t: R): Resource[F, LikelihoodEvaluation[R]] =
        partition.edgeLikelihood.flatMap(_(model, ppv, clv)).flatMap(_(t))

      def nodeLikelihood(
          model: Model,
          ppv: Ppv,
          parentHeight: R,
          leftClv: Clv,
          leftHeight: R,
          rightClv: Clv,
          rightHeight: R)(t: R): Resource[F, LikelihoodEvaluation[R]] =
        partition
          .nodeLikelihood
          .flatMap(_(model, ppv, parentHeight, leftClv, leftHeight, rightClv, rightHeight))
          .flatMap(_(t))
