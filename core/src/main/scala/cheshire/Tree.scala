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

sealed abstract class Tree[+A] {
  def value: A
  def isLeaf: Boolean
  def leftOption: Option[Tree[A]]
  def rightOption: Option[Tree[A]]

  final def size: Int = reducePostOrder(_ => 1)((l, r, _) => 1 + l + r)
  final def leafCount: Int = (size + 1) / 2
  final def nodeCount: Int = leafCount - 1

  final def button: Button[A] = Button(this, Nil)

  final def isLeft[B >: A: Eq](that: Tree[B]): Boolean =
    leftOption.exists(_ === that)

  final def isLeftOf[B >: A: Eq](that: Tree[B]): Boolean =
    that.isLeft(this)

  final def isRight[B >: A: Eq](that: Tree[B]): Boolean =
    rightOption.exists(_ === that)

  final def isRightOf[B >: A: Eq](that: Tree[B]): Boolean =
    that.isRight(this)

  final def sibling[B >: A: Eq](that: Tree[B]): Option[Tree[A]] =
    if (isRight(that)) leftOption else if (isLeft(that)) rightOption else None

  final def map[B](f: A => B): Tree[B] =
    traverse(a => Eval.now(f(a))).value

  final def traverse[F[_]: Apply, B](f: A => F[B]): F[Tree[B]] = this match {
    case Node(value, left, right) =>
      (f(value), left.traverse(f), right.traverse(f)).mapN(Node(_, _, _))
    case Leaf(value) => f(value).map(Leaf(_))
  }

  final def mapWithParent[B](f: A => B)(g: (A, A) => B): Tree[B] =
    traverseWithParent(a => Eval.now(f(a)))((p, c) => Eval.now(g(p, c))).value

  final def traverseWithParent[F[_]: Applicative, B](f: A => F[B])(
      g: (A, A) => F[B]): F[Tree[B]] = this match {
    case Node(value, left, right) =>
      (
        f(value),
        left.traverseWithParent(g(value, _))(g),
        right.traverseWithParent(g(value, _))(g)).mapN(Node(_, _, _))
    case Leaf(value) => f(value).map(Leaf(_))
  }

  final def mapWithOrientedParent[B](f: A => B)(g: (Oriented[A], A) => B): Tree[B] =
    traverseWithOrientedParent(a => Eval.now(f(a)))((p, c) => Eval.now(g(p, c))).value

  final def traverseWithOrientedParent[F[_]: Apply, B](f: A => F[B])(
      g: (Oriented[A], A) => F[B]): F[Tree[B]] = this match {
    case Node(value, left, right) =>
      (
        f(value),
        left.traverseWithOrientedParent(g(Left(value), _))(g),
        right.traverseWithOrientedParent(g(Right(value), _))(g)).mapN(Node(_, _, _))
    case Leaf(value) => f(value).map(Leaf(_))
  }

  final def mapWithChildren[B](f: A => B)(g: (A, A, A) => B): Tree[B] =
    traverseWithChildren(a => Eval.now(f(a)))((a, l, r) => Eval.now(g(a, l, r))).value

  final def traverseWithChildren[F[_]: Apply, B](f: A => F[B])(
      g: (A, A, A) => F[B]): F[Tree[B]] = this match {
    case Node(value, left, right) =>
      (
        g(value, left.value, right.value),
        left.traverseWithChildren(f)(g),
        right.traverseWithChildren(f)(g)).mapN(Node(_, _, _))
    case Leaf(value) => f(value).map(Leaf(_))
  }

  final def flatMap[B](f: A => Tree[B]): Tree[B] =
    flatTraverse(a => Eval.now(f(a))).value

  final def flatTraverse[F[_]: Apply, B](f: A => F[Tree[B]]): F[Tree[B]] = this match {
    case Node(value, left, right) =>
      (f(value), left.flatTraverse(f), right.flatTraverse(f)).mapN { (fb, flb, frb) =>
        def recurse(fb: Tree[B]): Eval[Tree[B]] = fb match {
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

  final def coflatMap[B](f: Tree[A] => B): Tree[B] =
    coflatTraverse(fa => Eval.now(f(fa))).value

  final def coflatTraverse[F[_]: Apply, B](f: Tree[A] => F[B]): F[Tree[B]] = this match {
    case node @ Node(_, left, right) =>
      (f(node), left.coflatTraverse(f), right.coflatTraverse(f)).mapN(Node(_, _, _))
    case leaf => f(leaf).map(Leaf(_))
  }

  final def mapWithButton[B](f: Button[A] => B): Tree[B] =
    traverseWithButton(c => Eval.now(f(c))).value

  final def traverseWithButton[F[_]: Monad, B](f: Button[A] => F[B]): F[Tree[B]] = {
    def recurse(button: Button[A]): F[Tree[B]] =
      (button.left, button.right) match {
        case (Some(left), Some(right)) =>
          for {
            l <- recurse(left)
            r <- recurse(right)
            b <- f(button)
          } yield Node(b, l, r)
        case _ => f(button).map(Leaf(_))
      }
    recurse(button)
  }

  final def reducePostOrder[B](f: A => B)(g: (B, B, A) => B): B =
    reducePostOrderM(a => Eval.now(f(a)))((l, r, a) => Eval.now(g(l, r, a))).value

  final def reducePostOrderM[F[_]: Monad, B](f: A => F[B])(g: (B, B, A) => F[B]): F[B] =
    this match {
      case Node(value, left, right) =>
        for {
          l <- left.reducePostOrderM(f)(g)
          r <- right.reducePostOrderM(f)(g)
          b <- g(l, r, value)
        } yield b
      case Leaf(value) => f(value)
    }

  final def scanPreOrder[B](f: A => B)(g: (B, A) => B): Tree[B] =
    scanPreOrderM(a => Eval.now(f(a)))((b, a) => Eval.now(g(b, a))).value

  final def scanPreOrderM[F[_]: Monad, B](f: A => F[B])(g: (B, A) => F[B]): F[Tree[B]] =
    this match {
      case Node(value, left, right) =>
        for {
          b <- f(value)
          l <- left.scanPreOrderM(g(b, _))(g)
          r <- right.scanPreOrderM(g(b, _))(g)
        } yield Node(b, l, r)
      case Leaf(value) => f(value).map(Leaf(_))
    }

  final def scanPreOrderOriented[B](f: A => B)(g: (Oriented[B], A) => B): Tree[B] =
    scanPreOrderOrientedM(a => Eval.now(f(a)))((b, a) => Eval.now(g(b, a))).value

  final def scanPreOrderOrientedM[F[_]: Monad, B](f: A => F[B])(
      g: (Oriented[B], A) => F[B]): F[Tree[B]] =
    this match {
      case Node(value, left, right) =>
        for {
          b <- f(value)
          l <- left.scanPreOrderOrientedM(g(Left(b), _))(g)
          r <- right.scanPreOrderOrientedM(g(Right(b), _))(g)
        } yield Node(b, l, r)
      case Leaf(value) => f(value).map(Leaf(_))
    }

  final def scanPostOrder[B](f: A => B)(g: (B, B, A) => B): Tree[B] =
    scanPostOrderM(a => Eval.now(f(a)))((l, r, a) => Eval.now(g(l, r, a))).value

  final def scanPostOrderM[F[_]: Monad, B](f: A => F[B])(g: (B, B, A) => F[B]): F[Tree[B]] =
    this match {
      case Node(value, left, right) =>
        for {
          l <- left.scanPostOrderM(f)(g)
          r <- right.scanPostOrderM(f)(g)
          b <- g(l.value, r.value, value)
        } yield Node(b, l, r)
      case Leaf(value) => f(value).map(Leaf(_))
    }

  final def zip[B](that: Tree[B]): Tree[(A, B)] =
    zipWithM(that)((a, b) => Eval.now((a, b))).value

  final def zipWith[B, Z](that: Tree[B])(f: (A, B) => Z): Tree[Z] =
    zipWithM(that)((a, b) => Eval.now(f(a, b))).value

  final def zipWithM[F[_]: Monad, B, Z](that: Tree[B])(f: (A, B) => F[Z]): F[Tree[Z]] =
    (this, that) match {
      case (Node(a, thisLeft, thisRight), Node(b, thatLeft, thatRight)) =>
        for {
          l <- thisLeft.zipWithM(thatLeft)(f)
          r <- thisRight.zipWithM(thatRight)(f)
          z <- f(a, b)
        } yield Node(z, l, r)
      case _ => f(this.value, that.value).map(Leaf(_))
    }

  final def ===[B >: A: Eq](that: Tree[B]): Boolean =
    (this eq that) || ((this, that) match {
      case (Node(thisValue, thisLeft, thisRight), Node(thatValue, thatLeft, thatRight)) =>
        (thisValue: B) === thatValue && thisLeft === thatLeft && thisRight === thatRight
      case (Leaf(thisValue), Leaf(thatValue)) =>
        (thisValue: B) === thatValue
      case _ => false
    })

  final def show[B >: A](implicit show: Show[B]): String = this match {
    case Node(value, left, right) =>
      s"Node(${(value: B).show}, ${left.show[B]}, ${right.show[B]})"
    case Leaf(value) => s"Leaf(${(value: B).show})"
  }

  final def preorder: Button[A] = button

  final def postorder: Button[A] =
    Monad[Option]
      .iterateUntilM(button)(_.left)(_.at.isLeaf)
      // Should never be called
      .getOrElse(button)
}

object Tree {

  type Oriented[+A] = Either[A, A]

  def unapply[A](tree: Tree[A]): Some[(A, Option[Tree[A]], Option[Tree[A]])] =
    Some((tree.value, tree.leftOption, tree.rightOption))

  def apply[A](value: A): Tree[A] = Leaf(value)
  def apply[A](value: A, left: Tree[A], right: Tree[A]): Tree[A] =
    Node(value, left, right)

  final case class Node[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A] {
    override def isLeaf = false
    override def leftOption = Some(left)
    override def rightOption = Some(right)
  }

  final case class Leaf[+A](value: A) extends Tree[A] {
    override def isLeaf = true
    override def leftOption = None
    override def rightOption = None
  }

  final case class Button[+A](at: Tree[A], ancestry: List[Node[A]]) {

    def value: A = at.value

    def tree: Tree[A] = ancestry.lastOption.getOrElse(at)

    def up: Option[Button[A]] = ancestry match {
      case at :: ancestry => Some(Button(at, ancestry))
      case _ => None
    }

    def left: Option[Button[A]] = at match {
      case at @ Node(_, left, _) => Some(Button(left, at :: ancestry))
      case _ => None
    }

    def right: Option[Button[A]] = at match {
      case at @ Node(_, _, right) => Some(Button(right, at :: ancestry))
      case _ => None
    }

    def isLeft: Boolean = up.flatMap(_.left).exists(_ eq this)

    def isRight: Boolean = up.flatMap(_.right).exists(_ eq this)

    def sibling: Option[Button[A]] =
      if (isLeft)
        up.flatMap(_.right)
      else if (isRight)
        up.flatMap(_.left)
      else
        None

    def nextPreorder: Option[Button[A]] =
      left.orElse {
        Monad[Option].iterateUntilM(this)(_.up)(_.isLeft).flatMap(_.sibling)
      }

    def nextPostorder: Option[Button[A]] =
      if (isLeft)
        sibling.flatMap(Monad[Option].iterateUntilM(_)(_.left)(_.at.isLeaf))
      else
        up

    def replace[B >: A](tree: Tree[B]): Button[B] = Button(
      tree,
      NonEmptyList.fromList(ancestry).fold(List.empty[Node[B]]) {
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

  implicit val cheshireInstancesForTree
      : Bimonad[Tree] with NonEmptyTraverse[Tree] with Align[Tree] =
    new Bimonad[Tree] with NonEmptyTraverse[Tree] with Align[Tree] {

      override def functor: Functor[Tree] = this

      override def pure[A](x: A): Tree[A] = Leaf(x)

      override def extract[A](x: Tree[A]): A = x.value

      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa.map(f)

      override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
        fa.flatMap(f)

      override def coflatMap[A, B](fa: Tree[A])(f: Tree[A] => B): Tree[B] =
        fa.coflatMap(f)

      override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
        val g = (a: A) => Eval.now(f(a))
        def tailRecEval(a: A): Eval[Tree[B]] =
          g(a).flatMap { ab =>
            ab.flatTraverse {
              case Left(a) => tailRecEval(a)
              case Right(b) => Eval.now(pure(b))
            }
          }
        tailRecEval(a).value
      }

      override def foldMap[A, B: Monoid](fa: Tree[A])(f: A => B): B =
        fa.reducePostOrder(f)((l, r, a) => l |+| r |+| f(a))

      override def foldLeft[A, B](fa: Tree[A], b: B)(f: (B, A) => B): B =
        reduceLeftTo(fa)(f(b, _))(f)

      override def foldRight[A, B](fa: Tree[A], lb: Eval[B])(
          f: (A, Eval[B]) => Eval[B]): Eval[B] =
        reduceRightToImpl(fa)(f(_, lb))(f)

      override def reduceLeftTo[A, B](fa: Tree[A])(f: A => B)(g: (B, A) => B): B = {
        def recurse(fa: Tree[A])(f: A => B): Eval[B] = fa match {
          case Node(value, left, right) =>
            for {
              l <- recurse(left)(f)
              r <- recurse(right)(g(l, _))
            } yield g(r, value)
          case Leaf(value) => Eval.now(f(value))
        }
        recurse(fa)(f).value
      }

      override def reduceRightTo[A, B](fa: Tree[A])(f: A => B)(
          g: (A, Eval[B]) => Eval[B]): Eval[B] =
        reduceRightToImpl(fa)(a => Eval.later(f(a)))(g)

      private def reduceRightToImpl[A, B](fa: Tree[A])(f: A => Eval[B])(
          g: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
        case Node(value, left, right) =>
          val b = Eval.defer(f(value))
          val r = Eval.defer(reduceRightToImpl(right)(g(_, b))(g))
          Eval.defer(reduceRightToImpl(left)(g(_, r))(g))
        case Leaf(value) => Eval.defer(f(value))
      }

      override def traverse[G[_]: Applicative, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] =
        fa.traverse(f)

      override def flatTraverse[G[_]: Applicative, A, B](fa: Tree[A])(f: A => G[Tree[B]])(
          implicit F: FlatMap[Tree]): G[Tree[B]] =
        fa.flatTraverse(f)

      override def nonEmptyTraverse[G[_]: Apply, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] =
        fa.traverse(f)

      override def nonEmptyFlatTraverse[G[_]: Apply, A, B](fa: Tree[A])(f: A => G[Tree[B]])(
          implicit F: FlatMap[Tree]): G[Tree[B]] =
        fa.flatTraverse(f)

      override def align[A, B](fa: Tree[A], fb: Tree[B]): Tree[Ior[A, B]] =
        alignEval(fa, fb).value

      private def alignEval[A, B](fa: Tree[A], fb: Tree[B]): Eval[Tree[Ior[A, B]]] =
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

  implicit def cheshireParallelForTreeZipTree: NonEmptyParallel.Aux[Tree, ZipTree] =
    new NonEmptyParallel[Tree] {
      type F[X] = ZipTree[X]

      override def flatMap: FlatMap[Tree] = cheshireInstancesForTree
      override def apply: Apply[ZipTree] = ZipTree.cheshireApplyForZipTree

      override def parallel: Tree ~> ZipTree =
        Lambda[Tree ~> ZipTree](new ZipTree(_))

      override def sequential: ZipTree ~> Tree =
        Lambda[ZipTree ~> Tree](_.value)
    }

  implicit def cheshireShowForTree[A: Show]: Show[Tree[A]] =
    new Show[Tree[A]] {
      override def show(t: Tree[A]): String = t.show
    }

  implicit def cheshireEqForTree[A: Eq]: Eq[Tree[A]] =
    new Eq[Tree[A]] {
      override def eqv(x: Tree[A], y: Tree[A]): Boolean =
        x === y
    }
}

final case class ZipTree[+A](value: Tree[A]) extends AnyVal

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
