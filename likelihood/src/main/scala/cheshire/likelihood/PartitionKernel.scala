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

import cats.Functor
import cats.Invariant
import cats.effect.kernel.Resource
import cats.syntax.all.*

import scala.annotation.targetName

trait PartitionKernel[F[_], R]:
  outer =>

  type Model
  type Matrix
  type Partial = Ppv | Clv
  type Ppv
  type Clv = NodeClv | TipClv
  type NodeClv
  type TipClv

  def categoryCount: Int

  def tips: IndexedSeq[TipClv]

  def allocateModel: Resource[F, Model]

  def allocateMatrix: Resource[F, Matrix]

  def allocatePpv: Resource[F, Ppv]

  def allocateClv: Resource[F, NodeClv]

  def initModel(
      freqs: IndexedSeq[R],
      params: IndexedSeq[R],
      rate: R,
      alpha: R,
      model: Model): F[Unit]

  def rates(model: Model): F[IndexedSeq[R]]

  def computeMatrix(model: Model, t: R, P: Matrix): F[Unit]

  def forecast(x: Ppv, P: Matrix, y: Ppv): F[Unit]

  def backcast(y: Clv, P: Matrix, x: NodeClv): F[Unit]

  def backcastProduct(y: Clv, Py: Matrix, z: Clv, Pz: Matrix, x: NodeClv): F[Unit]

  @targetName("ppvProduct")
  def product(x: Ppv, y: Clv, z: Ppv): F[Unit]

  @targetName("clvProduct")
  def product(x: Clv, y: Clv, z: NodeClv): F[Unit]

  def seed(model: Model, x: Ppv): F[Unit]

  def integrateProduct(x: Ppv, y: Clv): F[R]

  def seedAndIntegrate(model: Model, x: Clv): F[R]

  def edgeLikelihood: Resource[F, EdgeLikelihood]

  trait EdgeLikelihood:
    def apply(
        model: Model,
        ppv: Ppv,
        clv: Clv): Resource[F, R => Resource[F, LikelihoodEvaluation[F, R]]]

  def nodeLikelihood: Resource[F, NodeLikelihood]

  trait NodeLikelihood:
    def apply(
        model: Model,
        ppv: Ppv,
        parentHeight: R,
        leftClv: Clv,
        leftHeight: R,
        rightClv: Clv,
        rightHeight: R): Resource[F, R => Resource[F, LikelihoodEvaluation[F, R]]]

  final def imap[S](f: R => S)(g: S => R)(
      using Functor[F]): PartitionKernel.Aux[F, S, Model, Matrix, Ppv, NodeClv, TipClv] =
    new PartitionKernel[F, S]:
      type Model = outer.Model
      type Matrix = outer.Matrix
      type Ppv = outer.Ppv
      type NodeClv = outer.NodeClv
      type TipClv = outer.TipClv
      def categoryCount: Int = outer.categoryCount
      def tips: IndexedSeq[TipClv] = outer.tips
      def allocateModel: Resource[F, Model] = outer.allocateModel
      def allocateMatrix: Resource[F, Matrix] = outer.allocateMatrix
      def allocatePpv: Resource[F, Ppv] = outer.allocatePpv
      def allocateClv: Resource[F, NodeClv] = outer.allocateClv
      def initModel(
          freqs: IndexedSeq[S],
          params: IndexedSeq[S],
          rate: S,
          alpha: S,
          model: Model): F[Unit] =
        outer.initModel(freqs.map(g), params.map(g), g(rate), g(alpha), model)
      def rates(model: Model): F[IndexedSeq[S]] = outer.rates(model).map(_.map(f))
      def computeMatrix(model: Model, t: S, P: Matrix): F[Unit] =
        outer.computeMatrix(model, g(t), P)
      def forecast(x: Ppv, P: Matrix, y: Ppv): F[Unit] = outer.forecast(x, P, y)
      def backcast(y: Clv, P: Matrix, x: NodeClv): F[Unit] = outer.backcast(y, P, x)
      def backcastProduct(y: Clv, Py: Matrix, z: Clv, Pz: Matrix, x: NodeClv): F[Unit] =
        outer.backcastProduct(y, Py, z, Pz, x)
      @targetName("ppvProduct")
      def product(x: Ppv, y: Clv, z: Ppv): F[Unit] = outer.product(x, y, z)
      @targetName("clvProduct")
      def product(x: Clv, y: Clv, z: NodeClv): F[Unit] = outer.product(x, y, z)
      def seed(model: Model, x: Ppv): F[Unit] = outer.seed(model, x)
      def integrateProduct(x: Ppv, y: Clv): F[S] = outer.integrateProduct(x, y).map(f)
      def seedAndIntegrate(model: Model, x: Clv): F[S] = outer.seedAndIntegrate(model, x).map(f)
      def edgeLikelihood: Resource[F, EdgeLikelihood] = outer.edgeLikelihood.map { outer =>
        outer(_, _, _).map { outer => t => outer(g(t)).map(_.map(f)) }
      }
      def nodeLikelihood: Resource[F, NodeLikelihood] = outer.nodeLikelihood.map {
        outer => (model, ppv, parentHeight, leftClv, leftHeight, rightClv, rightHeight) =>
          outer(model, ppv, g(parentHeight), leftClv, g(leftHeight), rightClv, g(rightHeight))
            .map { outer => t => outer(g(t)).map(_.map(f)) }
      }

object PartitionKernel:

  type Aux[F[_], R, Model0, Matrix0, Ppv0, NodeClv0, TipClv0] = PartitionKernel[F, R] {
    type Model = Model0
    type Matrix = Matrix0
    type Ppv = Ppv0
    type NodeClv = NodeClv0
    type TipClv = TipClv0
  }

  given [F[_]](using Functor[F]): Invariant[PartitionKernel[F, _]] with
    def imap[A, B](fa: PartitionKernel[F, A])(f: A => B)(g: B => A) = fa.imap(f)(g)
