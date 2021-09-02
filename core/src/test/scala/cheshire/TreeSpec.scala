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

import cats.data.Ior
import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.AlignTests
import cats.laws.discipline.BimonadTests
import cats.laws.discipline.BitraverseTests
import cats.laws.discipline.CommutativeApplyTests
import cats.laws.discipline.NonEmptyParallelTests
import cats.laws.discipline.NonEmptyTraverseTests
import cats.laws.discipline.ShortCircuitingTests
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.specs2.scalacheck.Parameters
import org.typelevel.discipline.specs2.mutable.Discipline

import Tree.*

@org.portablescala.reflect.annotation.EnableReflectiveInstantiation
class TreeSpec extends Specification, Discipline, ScalaCheck:

  // Even increasing maxSize = 3 seems to cause issues :(
  given Parameters = Parameters(maxSize = 2)

  given genTree[N: Gen, L: Gen]: Gen[Tree[N, L]] = Gen.sized { size =>
    if size <= 1 then summon[Gen[L]].flatMap(Leaf(_))
    else
      for
        value <- summon[Gen[N]]
        leftSize <- Gen.choose(0, size - 1)
        left <- Gen.resize(leftSize, genTree[N, L])
        rightSize <- Gen.choose(0, size - 1)
        right <- Gen.resize(rightSize, genTree[N, L])
      yield Node(value, left, right)
  }

  given cogenTree[N: Cogen, L: Cogen]: Cogen[Tree[N, L]] =
    Cogen { (seed, tree) =>
      tree match
        case Leaf(l) => Cogen[L].perturb(seed, l)
        case Node(n, left, right) =>
          Cogen[N].perturb(
            cogenTree.perturb(
              cogenTree.perturb(
                seed,
                right
              ),
              left
            ),
            n
          )

    }

  given [N: Arbitrary, L: Arbitrary]: Arbitrary[Tree[N, L]] =
    Arbitrary(genTree(using Arbitrary.arbitrary[N], Arbitrary.arbitrary[L]))

  given [A](using arb: Arbitrary[UTree[A]]): Arbitrary[ZipTree[A]] =
    Arbitrary(arb.arbitrary.map(ZipTree(_)))

  given [A, B](using arb: Arbitrary[Either[(A, B), Either[A, B]]]): Arbitrary[Ior[A, B]] =
    Arbitrary(
      arb.arbitrary.map {
        case Left((a, b)) => Ior.both(a, b)
        case Right(ab) => Ior.fromEither(ab)
      }
    )

  given [A, B, C](
      using arb: Arbitrary[Either[(A, B), Either[A, B]] => C]): Arbitrary[Ior[A, B] => C] =
    Arbitrary(
      arb.arbitrary.map { f =>
        (_: Ior[A, B]) match {
          case Ior.Both(a, b) => f(Left((a, b)))
          case Ior.Left(a) => f(Right(Left(a)))
          case Ior.Right(b) => f(Right(Right(b)))
        }
      }
    )

  // It is stack safe, but testing this is prohibitively explosive!
  checkAll("Bimonad[Tree]", BimonadTests[UTree].stackUnsafeMonad[Int, Int, Int])
  checkAll("Bimonad[Tree]", BimonadTests[UTree].comonad[Int, Int, Int])
  checkAll(
    "NonEmptyTraverse[Tree]",
    NonEmptyTraverseTests[UTree].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option])
  checkAll(
    "Bitraverse[Tree]",
    BitraverseTests[Tree].bitraverse[Option, Int, Int, Int, Int, Int, Int])
  checkAll("NonEmptyTraverse[Tree]", ShortCircuitingTests[UTree].foldable[Int])
  checkAll("Align[Tree]", AlignTests[UTree].align[Int, Int, Int, Int])
  checkAll("Parallel[Tree]", NonEmptyParallelTests[UTree].nonEmptyParallel[Int, Int])
  checkAll("Apply[ZipTree]", CommutativeApplyTests[ZipTree].commutativeApply[Int, Int, Int])
  checkAll("Eq[Tree]", EqTests[Tree[Int, Int]].eqv)
