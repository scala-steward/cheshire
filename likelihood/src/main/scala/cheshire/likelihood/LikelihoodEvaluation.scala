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
import cats.syntax.all.*

trait LikelihoodEvaluation[F[_], L, R]:
  def likelihood: F[L]
  def derivative: F[R]
  def secondDerivative: F[R]

object LikelihoodEvaluation:
  def apply[F[_]: Applicative, L, R](l: L, d: R, dd: R): LikelihoodEvaluation[F, L, R] =
    new {
      val likelihood = l.pure[F]
      val derivative = d.pure[F]
      val secondDerivative = d.pure[F]
    }
