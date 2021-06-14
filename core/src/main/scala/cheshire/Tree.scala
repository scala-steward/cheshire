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

import cats.Align
import cats.Applicative
import cats.Apply
import cats.Bimonad
import cats.Bitraverse
import cats.CommutativeApply
import cats.Eq
import cats.Eval
import cats.FlatMap
import cats.Functor
import cats.Monad
import cats.Monoid
import cats.NonEmptyParallel
import cats.NonEmptyTraverse
import cats.Show
import cats.data.Ior
import cats.data.NonEmptyList
import cats.syntax.all._
import cats.~>

import Tree._

sealed abstract class Tree[+N, +L] {
  def valueEither: Either[N, L]
  def isLeaf: Boolean
  def leftOption: Option[Tree[N, L]]
  def rightOption: Option[Tree[N, L]]

  final def button: Button[N, L] = Button(this, Nil)

  final def preorder: Button[N, L] = button

  final def postorder: Button[N, L] =
    Monad[Option]
      .iterateUntilM(button)(_.left)(_.at.isLeaf)
      // Should never be called
      .getOrElse(button)

  final def size: Int = reducePostOrder(_ => 1)((l, r, _) => 1 + l + r)
  final def leafCount: Int = (size + 1) / 2
  final def nodeCount: Int = leafCount - 1

  final def isLeft[N1 >: N: Eq, L1 >: L: Eq](that: Tree[N1, L1]): Boolean =
    leftOption.exists(_ === that)

  final def isLeftOf[N1 >: N: Eq, L1 >: L: Eq](that: Tree[N1, L1]): Boolean =
    that.isLeft(this)

  final def isRight[N1 >: N: Eq, L1 >: L: Eq](that: Tree[N1, L1]): Boolean =
    rightOption.exists(_ === that)

  final def isRightOf[N1 >: N: Eq, L1 >: L: Eq](that: Tree[N1, L1]): Boolean =
    that.isRight(this)

  final def sibling[N1 >: N: Eq, L1 >: L: Eq](that: Tree[N1, L1]): Option[Tree[N1, L1]] =
    if (isRight(that)) leftOption else if (isLeft(that)) rightOption else None

  final def bimap[N1, L1](f: N => N1, g: L => L1): Tree[N1, L1] =
    bitraverse(n => Eval.now(f(n)), l => Eval.now(g(l))).value

  final def bitraverse[F[_]: Apply, N1, L1](f: N => F[N1], g: L => F[L1]): F[Tree[N1, L1]] =
    this match {
      case Node(value, left, right) =>
        (f(value), left.bitraverse(f, g), right.bitraverse(f, g)).mapN(Node(_, _, _))
      case Leaf(value) => g(value).map(Leaf(_))
    }

  final def reducePostOrder[Z](f: L => Z)(g: (Z, Z, N) => Z): Z =
    reducePostOrderM(a => Eval.now(f(a)))((l, r, a) => Eval.now(g(l, r, a))).value

  final def reducePostOrderM[F[_]: Monad, Z](f: L => F[Z])(g: (Z, Z, N) => F[Z]): F[Z] =
    this match {
      case Node(value, left, right) =>
        for {
          l <- left.reducePostOrderM(f)(g)
          r <- right.reducePostOrderM(f)(g)
          b <- g(l, r, value)
        } yield b
      case Leaf(value) => f(value)
    }

  final def scanPostOrder[Z](f: L => Z)(g: (Z, Z, N) => Z): Tree[Z, Z] =
    scanPostOrderM(a => Eval.now(f(a)))((l, r, a) => Eval.now(g(l, r, a))).value

  final def scanPostOrderM[F[_]: Monad, Z](f: L => F[Z])(g: (Z, Z, N) => F[Z]): F[Tree[Z, Z]] =
    this match {
      case Node(value, left, right) =>
        for {
          l <- left.scanPostOrderM(f)(g)
          r <- right.scanPostOrderM(f)(g)
          b <- g(l.value, r.value, value)
        } yield Node(b, l, r)
      case Leaf(value) => f(value).map(Leaf(_))
    }

  final def ===[N1 >: N: Eq, L1 >: L: Eq](that: Tree[N1, L1]): Boolean = {
    def recurse(x: Tree[N1, L1], y: Tree[N1, L1]): Eval[Boolean] = (x, y) match {
      case (Node(xn, xl, xr), Node(yn, yl, yr)) if xn === yn =>
        recurse(xl, yl).flatMap {
          if (_)
            recurse(xr, yr)
          else
            Eval.False
        }
      case (Leaf(xl), Leaf(yl)) if xl === yl => Eval.True
      case _ => Eval.False
    }
    recurse(this, that).value
  }

  final def show[N1 >: N: Show, L1 >: L: Show]: String =
    reducePostOrder(l => s"Leaf(${(l: L1).show})") { (l, r, n) =>
      s"Node(${(n: N1).show}, $l, $r)"
    }
}

object Tree extends TreeInstances {
  type UTree[+A] = Tree[A, A]
  type Oriented[+A] = Either[A, A]

  def unapply[N, L](
      tree: Tree[N, L]): Some[(Either[N, L], Option[Tree[N, L]], Option[Tree[N, L]])] =
    Some((tree.valueEither, tree.leftOption, tree.rightOption))

  def apply[L](value: L): Tree[Nothing, L] = Leaf(value)
  def apply[N, L](value: N, left: Tree[N, L], right: Tree[N, L]): Tree[N, L] =
    Node(value, left, right)

  final case class Node[+N, +L](value: N, left: Tree[N, L], right: Tree[N, L])
      extends Tree[N, L] {
    override def valueEither = Left(value)
    override def isLeaf = false
    override def leftOption = Some(left)
    override def rightOption = Some(right)
  }

  final case class Leaf[+L](value: L) extends Tree[Nothing, L] {
    override def valueEither = Right(value)
    override def isLeaf = true
    override def leftOption = None
    override def rightOption = None
  }

  implicit def cheshireTreeToUTreeOps[A](tree: Tree[A, A]): UTreeOps[A] =
    new UTreeOps(tree)

  final case class Button[+N, +L](at: Tree[N, L], ancestry: List[Node[N, L]]) {

    def tree: Tree[N, L] = ancestry.lastOption.getOrElse(at)

    def up: Option[Button[N, L]] = ancestry match {
      case at :: ancestry => Some(Button(at, ancestry))
      case _ => None
    }

    def left: Option[Button[N, L]] = at match {
      case at @ Node(_, left, _) => Some(Button(left, at :: ancestry))
      case _ => None
    }

    def right: Option[Button[N, L]] = at match {
      case at @ Node(_, _, right) => Some(Button(right, at :: ancestry))
      case _ => None
    }

    def isLeft: Boolean = up.flatMap(_.left).exists(_ eq this)

    def isRight: Boolean = up.flatMap(_.right).exists(_ eq this)

    def sibling: Option[Button[N, L]] =
      if (isLeft)
        up.flatMap(_.right)
      else if (isRight)
        up.flatMap(_.left)
      else
        None

    def nextPreorder: Option[Button[N, L]] =
      left.orElse {
        Monad[Option].iterateUntilM(this)(_.up)(_.isLeft).flatMap(_.sibling)
      }

    def nextPostorder: Option[Button[N, L]] =
      if (isLeft)
        sibling.flatMap(Monad[Option].iterateUntilM(_)(_.left)(_.at.isLeaf))
      else
        up

    def replace[N1 >: N, L1 >: L](tree: Tree[N1, L1]): Button[N1, L1] = Button(
      tree,
      NonEmptyList.fromList(ancestry).fold(List.empty[Node[N1, L1]]) {
        case NonEmptyList(oldParent, ancestry) =>
          val oldChild = at
          val newChild = tree
          val newParent =
            if (oldParent.left eq oldChild)
              oldParent.copy(left = newChild)
            else
              oldParent.copy(right = newChild)

          ancestry
            .view
            .scanLeft((oldParent, newParent)) {
              case ((oldChild, newChild), oldParent) =>
                val newParent =
                  if (oldParent eq oldChild)
                    oldParent.copy(left = newChild)
                  else
                    oldParent.copy(right = newChild)
                (oldParent, newParent)
            }
            .map(_._2)
            .toList
      }
    )

  }
}

final class UTreeOps[A](val tree: Tree[A, A]) extends AnyVal {
  def value: A = tree match {
    case Node(value, _, _) => value
    case Leaf(value) => value
  }

  def map[B](f: A => B): UTree[B] =
    tree.bimap(f, f)

  def traverse[F[_]: Apply, B](f: A => F[B]): F[UTree[B]] =
    tree.bitraverse(f, f)

  def mapWithParent[B](f: A => B)(g: (A, A) => B): UTree[B] =
    traverseWithParent(a => Eval.now(f(a)))((p, c) => Eval.now(g(p, c))).value

  def traverseWithParent[F[_]: Applicative, B](f: A => F[B])(g: (A, A) => F[B]): F[UTree[B]] =
    tree match {
      case Node(value, left, right) =>
        (
          f(value),
          left.traverseWithParent(g(value, _))(g),
          right.traverseWithParent(g(value, _))(g)).mapN(Node(_, _, _))
      case Leaf(value) => f(value).map(Leaf(_))
    }

  def mapWithOrientedParent[B](f: A => B)(g: (Oriented[A], A) => B): UTree[B] =
    traverseWithOrientedParent(a => Eval.now(f(a)))((p, c) => Eval.now(g(p, c))).value

  def traverseWithOrientedParent[F[_]: Apply, B](f: A => F[B])(
      g: (Oriented[A], A) => F[B]): F[UTree[B]] = tree match {
    case Node(value, left, right) =>
      (
        f(value),
        left.traverseWithOrientedParent(g(Left(value), _))(g),
        right.traverseWithOrientedParent(g(Right(value), _))(g)).mapN(Node(_, _, _))
    case Leaf(value) => f(value).map(Leaf(_))
  }

  def mapWithChildren[B](f: A => B)(g: (A, A, A) => B): UTree[B] =
    traverseWithChildren(a => Eval.now(f(a)))((a, l, r) => Eval.now(g(a, l, r))).value

  def traverseWithChildren[F[_]: Apply, B](f: A => F[B])(g: (A, A, A) => F[B]): F[UTree[B]] =
    tree match {
      case Node(value, left, right) =>
        (
          g(value, left.value, right.value),
          left.traverseWithChildren(f)(g),
          right.traverseWithChildren(f)(g)).mapN(Node(_, _, _))
      case Leaf(value) => f(value).map(Leaf(_))
    }

  def flatMap[B](f: A => UTree[B]): UTree[B] =
    flatTraverse(a => Eval.now(f(a))).value

  def flatTraverse[F[_]: Apply, B](f: A => F[UTree[B]]): F[UTree[B]] = tree match {
    case Node(value, left, right) =>
      (f(value), left.flatTraverse(f), right.flatTraverse(f)).mapN { (fb, flb, frb) =>
        def recurse(fb: Tree[B, B]): Eval[UTree[B]] = fb match {
          case Node(value, left, right) =>
            for {
              l <- recurse(left)
              r <- recurse(right)
            } yield Node(value, l, r)
          case Leaf(value) => Eval.now(Node(value, flb, frb))
        }
        recurse(fb).value
      }
    case Leaf(value) => f(value)
  }

  def coflatMap[B](f: UTree[A] => B): UTree[B] =
    coflatTraverse(fa => Eval.now(f(fa))).value

  def coflatTraverse[F[_]: Apply, B](f: UTree[A] => F[B]): F[UTree[B]] = tree match {
    case node @ Node(_, left, right) =>
      (f(node), left.coflatTraverse(f), right.coflatTraverse(f)).mapN(Node(_, _, _))
    case leaf => f(leaf).map(Leaf(_))
  }

  def mapWithButton[B](f: Button[A, A] => B): UTree[B] =
    traverseWithButton(c => Eval.now(f(c))).value

  def traverseWithButton[F[_]: Monad, B](f: Button[A, A] => F[B]): F[UTree[B]] = {
    def recurse(button: Button[A, A]): F[UTree[B]] =
      (button.left, button.right) match {
        case (Some(left), Some(right)) =>
          for {
            l <- recurse(left)
            r <- recurse(right)
            b <- f(button)
          } yield Node(b, l, r)
        case _ => f(button).map(Leaf(_))
      }
    recurse(tree.button)
  }

  def scanPreOrder[B](f: A => B)(g: (B, A) => B): UTree[B] =
    scanPreOrderM(a => Eval.now(f(a)))((b, a) => Eval.now(g(b, a))).value

  def scanPreOrderM[F[_]: Monad, B](f: A => F[B])(g: (B, A) => F[B]): F[UTree[B]] =
    tree match {
      case Node(value, left, right) =>
        for {
          b <- f(value)
          l <- left.scanPreOrderM(g(b, _))(g)
          r <- right.scanPreOrderM(g(b, _))(g)
        } yield Node(b, l, r)
      case Leaf(value) => f(value).map(Leaf(_))
    }

  def scanPreOrderOriented[B](f: A => B)(g: (Oriented[B], A) => B): UTree[B] =
    scanPreOrderOrientedM(a => Eval.now(f(a)))((b, a) => Eval.now(g(b, a))).value

  def scanPreOrderOrientedM[F[_]: Monad, B](f: A => F[B])(
      g: (Oriented[B], A) => F[B]): F[UTree[B]] =
    tree match {
      case Node(value, left, right) =>
        for {
          b <- f(value)
          l <- left.scanPreOrderOrientedM(g(Left(b), _))(g)
          r <- right.scanPreOrderOrientedM(g(Right(b), _))(g)
        } yield Node(b, l, r)
      case Leaf(value) => f(value).map(Leaf(_))
    }

  def zip[B](that: UTree[B]): UTree[(A, B)] =
    zipWithM(that)((a, b) => Eval.now((a, b))).value

  def zipWith[B, Z](that: UTree[B])(f: (A, B) => Z): UTree[Z] =
    zipWithM(that)((a, b) => Eval.now(f(a, b))).value

  def zipWithM[F[_]: Monad, B, Z](that: UTree[B])(f: (A, B) => F[Z]): F[UTree[Z]] =
    (tree, that: Tree[B, B]) match {
      case (Node(a, thisLeft, thisRight), Node(b, thatLeft, thatRight)) =>
        for {
          l <- thisLeft.zipWithM(thatLeft)(f)
          r <- thisRight.zipWithM(thatRight)(f)
          z <- f(a, b)
        } yield Node(z, l, r)
      case _ => f(this.value, that.value).map(Leaf(_))
    }

}

// sealed abstract class Tree[+A] {

//   final def zip[B](that: Tree[B]): Tree[(A, B)] =
//     zipWithM(that)((a, b) => Eval.now((a, b))).value

//   final def zipWith[B, Z](that: Tree[B])(f: (A, B) => Z): Tree[Z] =
//     zipWithM(that)((a, b) => Eval.now(f(a, b))).value

//   final def zipWithM[F[_]: Monad, B, Z](that: Tree[B])(f: (A, B) => F[Z]): F[Tree[Z]] =
//     (this, that) match {
//       case (Node(a, thisLeft, thisRight), Node(b, thatLeft, thatRight)) =>
//         for {
//           l <- thisLeft.zipWithM(thatLeft)(f)
//           r <- thisRight.zipWithM(thatRight)(f)
//           z <- f(a, b)
//         } yield Node(z, l, r)
//       case _ => f(this.value, that.value).map(Leaf(_))
//     }

//   final def ===[B >: A: Eq](that: Tree[B]): Boolean =
//     (this eq that) || ((this, that) match {
//       case (Node(thisValue, thisLeft, thisRight), Node(thatValue, thatLeft, thatRight)) =>
//         (thisValue: B) === thatValue && thisLeft === thatLeft && thisRight === thatRight
//       case (Leaf(thisValue), Leaf(thatValue)) =>
//         (thisValue: B) === thatValue
//       case _ => false
//     })

//   final def show[B >: A](implicit show: Show[B]): String = this match {
//     case Node(value, left, right) =>
//       s"Node(${(value: B).show}, ${left.show[B]}, ${right.show[B]})"
//     case Leaf(value) => s"Leaf(${(value: B).show})"
//   }

//   final def preorder: Button[A] = button

//   final def postorder: Button[A] =
//     Monad[Option]
//       .iterateUntilM(button)(_.left)(_.at.isLeaf)
//       // Should never be called
//       .getOrElse(button)
// }

private[cheshire] sealed abstract class TreeInstances {

  implicit val cheshireInstancesForTree: Bitraverse[Tree] =
    new Bitraverse[Tree] {

      override def bimap[A, B, C, D](fab: Tree[A, B])(f: A => C, g: B => D): Tree[C, D] =
        fab.bimap(f, g)

      override def bitraverse[G[_]: Applicative, A, B, C, D](
          fab: Tree[A, B])(f: A => G[C], g: B => G[D]): G[Tree[C, D]] =
        fab.bitraverse(f, g)

      override def bifoldLeft[A, B, C](fab: Tree[A, B], c: C)(
          f: (C, A) => C,
          g: (C, B) => C): C = {
        def recurse(fab: Tree[A, B], c: C): Eval[C] = fab match {
          case Node(value, left, right) =>
            for {
              l <- recurse(left, c)
              r <- recurse(right, l)
            } yield f(r, value)
          case Leaf(value) => Eval.now(g(c, value))
        }
        recurse(fab, c).value
      }

      override def bifoldRight[A, B, C](fab: Tree[A, B], c: Eval[C])(
          f: (A, Eval[C]) => Eval[C],
          g: (B, Eval[C]) => Eval[C]): Eval[C] = fab match {
        case Node(value, left, right) =>
          val c1 = Eval.defer(f(value, c))
          val r = Eval.defer(bifoldRight(right, c1)(f, g))
          Eval.defer(bifoldRight(left, r)(f, g))
        case Leaf(value) => Eval.defer(g(value, c))
      }

    }

  implicit val cheshireInstancesForUTree
      : Bimonad[UTree] with NonEmptyTraverse[UTree] with Align[UTree] =
    new Bimonad[UTree] with NonEmptyTraverse[UTree] with Align[UTree] {

      override def functor: Functor[UTree] = this

      override def pure[A](x: A): UTree[A] = Leaf(x)

      override def extract[A](x: UTree[A]): A = x.value

      override def map[A, B](fa: UTree[A])(f: A => B): UTree[B] = fa.map(f)

      override def flatMap[A, B](fa: UTree[A])(f: A => UTree[B]): UTree[B] =
        fa.flatMap(f)

      override def coflatMap[A, B](fa: UTree[A])(f: UTree[A] => B): UTree[B] =
        fa.coflatMap(f)

      override def tailRecM[A, B](a: A)(f: A => UTree[Either[A, B]]): UTree[B] = {
        val g = (a: A) => Eval.now(f(a))
        def tailRecEval(a: A): Eval[UTree[B]] =
          g(a).flatMap { ab =>
            ab.flatTraverse {
              case Left(a) => tailRecEval(a)
              case Right(b) => Eval.now(pure(b))
            }
          }
        tailRecEval(a).value
      }

      override def foldMap[A, B: Monoid](fa: UTree[A])(f: A => B): B =
        fa.reducePostOrder(f)((l, r, a) => l |+| r |+| f(a))

      override def foldLeft[A, B](fa: UTree[A], b: B)(f: (B, A) => B): B =
        reduceLeftTo(fa)(f(b, _))(f)

      override def foldRight[A, B](fa: UTree[A], lb: Eval[B])(
          f: (A, Eval[B]) => Eval[B]): Eval[B] =
        reduceRightToImpl(fa)(f(_, lb))(f)

      override def reduceLeftTo[A, B](fa: UTree[A])(f: A => B)(g: (B, A) => B): B = {
        def recurse(fa: UTree[A])(f: A => B): Eval[B] = fa match {
          case Node(value, left, right) =>
            for {
              l <- recurse(left)(f)
              r <- recurse(right)(g(l, _))
            } yield g(r, value)
          case Leaf(value) => Eval.now(f(value))
        }
        recurse(fa)(f).value
      }

      override def reduceRightTo[A, B](fa: UTree[A])(f: A => B)(
          g: (A, Eval[B]) => Eval[B]): Eval[B] =
        reduceRightToImpl(fa)(a => Eval.later(f(a)))(g)

      private def reduceRightToImpl[A, B](fa: UTree[A])(f: A => Eval[B])(
          g: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
        case Node(value, left, right) =>
          val b = Eval.defer(f(value))
          val r = Eval.defer(reduceRightToImpl(right)(g(_, b))(g))
          Eval.defer(reduceRightToImpl(left)(g(_, r))(g))
        case Leaf(value) => Eval.defer(f(value))
      }

      override def traverse[G[_]: Applicative, A, B](fa: UTree[A])(f: A => G[B]): G[UTree[B]] =
        fa.traverse(f)

      override def flatTraverse[G[_]: Applicative, A, B](fa: UTree[A])(f: A => G[UTree[B]])(
          implicit F: FlatMap[UTree]): G[UTree[B]] =
        fa.flatTraverse(f)

      override def nonEmptyTraverse[G[_]: Apply, A, B](fa: UTree[A])(
          f: A => G[B]): G[UTree[B]] =
        fa.traverse(f)

      override def nonEmptyFlatTraverse[G[_]: Apply, A, B](fa: UTree[A])(f: A => G[UTree[B]])(
          implicit F: FlatMap[UTree]): G[UTree[B]] =
        fa.flatTraverse(f)

      override def align[A, B](fa: UTree[A], fb: UTree[B]): UTree[Ior[A, B]] =
        alignEval(fa, fb).value

      private def alignEval[A, B](fa: UTree[A], fb: UTree[B]): Eval[UTree[Ior[A, B]]] =
        (fa, fb) match {
          case (Node(a, la, ra), Node(b, lb, rb)) =>
            for {
              left <- alignEval(la, lb)
              right <- alignEval(ra, rb)
            } yield Node(Ior.both(a, b), left, right)
          case (Leaf(a), Leaf(b)) => Eval.now(Leaf(Ior.both(a, b)))
          case (Node(a, la, ra), Leaf(b)) =>
            for {
              left <- la.traverse(a => Eval.now(Ior.left(a)))
              right <- ra.traverse(a => Eval.now(Ior.left(a)))
            } yield Node(Ior.both(a, b), left, right)
          case (Leaf(a), Node(b, lb, rb)) =>
            for {
              left <- lb.traverse(b => Eval.now(Ior.right(b)))
              right <- rb.traverse(b => Eval.now(Ior.right(b)))
            } yield Node(Ior.both(a, b), left, right)
        }

    }

  implicit def cheshireParallelForTreeZipTree: NonEmptyParallel.Aux[UTree, ZipTree] =
    new NonEmptyParallel[UTree] {
      type F[X] = ZipTree[X]

      override def flatMap: FlatMap[UTree] = cheshireInstancesForUTree
      override def apply: Apply[ZipTree] = ZipTree.cheshireApplyForZipTree

      override def parallel: UTree ~> ZipTree =
        new (UTree ~> ZipTree) {
          override def apply[A](fa: UTree[A]): ZipTree[A] = ZipTree(fa)
        }

      override def sequential: ZipTree ~> UTree =
        new (ZipTree ~> UTree) {
          override def apply[A](fa: ZipTree[A]): UTree[A] = fa.value
        }
    }

  implicit def cheshireShowForTree[N: Show, L: Show]: Show[Tree[N, L]] =
    new Show[Tree[N, L]] {
      override def show(t: Tree[N, L]): String = t.show
    }

  implicit def cheshireEqForTree[N: Eq, L: Eq]: Eq[Tree[N, L]] =
    new Eq[Tree[N, L]] {
      override def eqv(x: Tree[N, L], y: Tree[N, L]): Boolean =
        x === y
    }
}

final case class ZipTree[+A](value: UTree[A]) extends AnyVal

object ZipTree {
  implicit val cheshireApplyForZipTree: CommutativeApply[ZipTree] =
    new CommutativeApply[ZipTree] {
      override def map[A, B](fa: ZipTree[A])(f: A => B): ZipTree[B] =
        new ZipTree(fa.value.map(f))

      override def ap[A, B](ff: ZipTree[A => B])(fa: ZipTree[A]): ZipTree[B] =
        new ZipTree(ff.value.zipWith(fa.value)(_(_)))
    }

  implicit def cheshireEqForZipTree[A: Eq]: Eq[ZipTree[A]] = Eq.by(_.value)
}
