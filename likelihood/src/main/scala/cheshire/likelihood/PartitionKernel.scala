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

import scala.annotation.targetName

trait PartitionKernel[F[_], R]:

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

object PartitionKernel:

  type Aux[F[_], R, Model0, Matrix0, Ppv0, NodeClv0, TipClv0] = PartitionKernel[F, R] {
    type Model = Model0
    type Matrix = Matrix0
    type Ppv = Ppv0
    type NodeClv = NodeClv0
    type TipClv = TipClv0
  }
