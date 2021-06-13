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

import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.BimonadTests
import cats.laws.discipline.CommutativeApplyTests
import cats.laws.discipline.NonEmptyParallelTests
import cats.laws.discipline.NonEmptyTraverseTests
import cats.laws.discipline.ShortCircuitingTests
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.ScalacheckShapeless
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.specs2.scalacheck.Parameters
import org.typelevel.discipline.specs2.mutable.Discipline

import Tree._

class TreeSpec extends Specification with Discipline with ScalaCheck with ScalacheckShapeless {

  // Even increasing maxSize = 3 seems to cause issues :(
  implicit val parameters: Parameters = Parameters(maxSize = 2)

  implicit private def genTree[A: Gen]: Gen[Tree[A]] = Gen.sized { size =>
    if (size <= 1)
      implicitly[Gen[A]].flatMap(Leaf(_))
    else
      for {
        value <- implicitly[Gen[A]]
        leftSize <- Gen.choose(0, size - 1)
        left <- Gen.resize(leftSize, genTree)
        rightSize <- Gen.choose(0, size - 1)
        right <- Gen.resize(rightSize, genTree)
      } yield Node(value, left, right)
  }

  implicit private def arbitraryTree[A: Arbitrary]: Arbitrary[Tree[A]] =
    Arbitrary(genTree(Arbitrary.arbitrary[A]))

  // It is stack safe, but testing this is prohibitively explosive!
  checkAll("Bimonad[Tree]", BimonadTests[Tree].stackUnsafeMonad[Int, Int, Int])
  checkAll("Bimonad[Tree]", BimonadTests[Tree].comonad[Int, Int, Int])
  checkAll(
    "NonEmptyTraverse[Tree]",
    NonEmptyTraverseTests[Tree].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option])
  checkAll("NonEmptyTraverse[Tree]", ShortCircuitingTests[Tree].foldable[Int])
  checkAll("Parallel[Tree]", NonEmptyParallelTests[Tree].nonEmptyParallel[Int, Int])
  checkAll("Apply[ZipTree]", CommutativeApplyTests[ZipTree].commutativeApply[Int, Int, Int])
  checkAll("Eq[Tree]", EqTests[Tree[Int]].eqv)

}
