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

import scala.annotation.targetName

trait Partition[F[_], G[_], R]:

  type Model
  type Matrix
  type Partial = Ppv | Clv
  type Ppv
  type Clv = NodeClv | TipClv
  type NodeClv
  type TipClv

  def tips: IndexedSeq[TipClv]

  def allocate(modelCount: Int, matrixCount: Int, ppvCount: Int, clvCount: Int)
      : F[(IndexedSeq[Model], IndexedSeq[Matrix], IndexedSeq[Ppv], IndexedSeq[NodeClv])]

  def initModel(
      freqs: IndexedSeq[R],
      params: IndexedSeq[R],
      rate: R,
      alpha: R,
      model: Model): G[Unit]

  def rates(model: Model): G[IndexedSeq[R]]

  def computeMatrix(model: Model, t: R, P: Matrix): G[Unit]

  def forecast(x: Ppv, P: Matrix, y: Ppv): G[Unit]

  def backcast(y: Clv, P: Matrix, x: NodeClv): G[Unit]

  def backcastProduct(y: Clv, Py: Matrix, z: Clv, Pz: Matrix, x: NodeClv): G[Unit]

  @targetName("ppvProduct")
  def product(x: Ppv, y: Clv, z: Ppv): G[Unit]

  @targetName("clvProduct")
  def product(x: Clv, y: Clv, z: NodeClv): G[Unit]

  def seed(model: Model, x: Ppv): G[Unit]

  def integrateProduct(x: Ppv, y: Clv): G[R]

  def seedAndIntegrate(model: Model, x: Clv): G[R]

  def edgeLikelihood(model: Model, ppv: Ppv, clv: Clv)(t: R): F[LikelihoodEvaluation[R]]

  def nodeLikelihood(
      model: Model,
      ppv: Ppv,
      parentHeight: R,
      leftClv: Clv,
      leftHeight: R,
      rightClv: Clv,
      rightHeight: R)(t: R): F[LikelihoodEvaluation[R]]