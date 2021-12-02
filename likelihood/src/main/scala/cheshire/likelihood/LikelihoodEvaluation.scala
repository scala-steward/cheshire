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

import cats.Applicative
import cats.Functor
import cats.kernel.Eq
import cats.syntax.all.*
import cats.~>

trait LikelihoodEvaluation[F[_], R]:
  outer =>
  def logLikelihood: F[R]
  def derivative: F[R]
  def secondDerivative: F[R]

  final def map[S](f: R => S)(using Functor[F]): LikelihoodEvaluation[F, S] = new:
    def logLikelihood = outer.logLikelihood.map(f)
    def derivative = outer.derivative.map(f)
    def secondDerivative = outer.secondDerivative.map(f)

  final def mapK[G[_]](f: F ~> G): LikelihoodEvaluation[G, R] = new:
    def logLikelihood = f(outer.logLikelihood)
    def derivative = f(outer.derivative)
    def secondDerivative = f(outer.secondDerivative)

object LikelihoodEvaluation:
  def apply[F[_]: Applicative, R](ll: R, d: R, dd: R): LikelihoodEvaluation[F, R] =
    new:
      val logLikelihood = ll.pure
      val derivative = d.pure
      val secondDerivative = dd.pure

  given [F[_], R](using Eq[F[R]]): Eq[LikelihoodEvaluation[F, R]] =
    Eq.by(l => (l.logLikelihood, l.derivative, l.secondDerivative))

  given [F[_]](using Functor[F]): Functor[LikelihoodEvaluation[F, _]] with
    def map[A, B](fa: LikelihoodEvaluation[F, A])(f: A => B): LikelihoodEvaluation[F, B] =
      fa.map(f)
