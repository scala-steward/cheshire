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

import cats.Applicative
import cats.Eq
import cats.Eval
import cats.Functor
import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all._
import monocle.Lens
import monocle.Optional

import Tree._

sealed abstract class Tree[+A] {
  def value: A
  def isLeaf: Boolean
  def leftOption: Option[Tree[A]]
  def rightOption: Option[Tree[A]]

  final def size: Int = foldPostOrder(_ => 1)((_, l, r) => 1 + l + r)
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

  final def traverse[F[_]: Applicative, B](f: A => F[B]): F[Tree[B]] = this match {
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

  final def traverseWithOrientedParent[F[_]: Applicative, B](f: A => F[B])(
      g: (Oriented[A], A) => F[B]): F[Tree[B]] = this match {
    case Node(value, left, right) =>
      (
        f(value),
        left.traverseWithOrientedParent(g(Left(value), _))(g),
        right.traverseWithOrientedParent(g(Right(value), _))(g)).mapN(Node(_, _, _))
    case Leaf(value) => f(value).map(Leaf(_))
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

  final def foldPostOrder[B](f: A => B)(g: (A, B, B) => B): B =
    foldPostOrderM(a => Eval.now(f(a)))((a, l, r) => Eval.now(g(a, l, r))).value

  final def foldPostOrderM[F[_]: Monad, B](f: A => F[B])(g: (A, B, B) => F[B]): F[B] =
    this match {
      case Node(value, left, right) =>
        for {
          l <- left.foldPostOrderM(f)(g)
          r <- right.foldPostOrderM(f)(g)
          b <- g(value, l, r)
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

  final def scanPostOrder[B](f: A => B)(g: (A, B, B) => B): Tree[B] =
    scanPostOrderM(a => Eval.now(f(a)))((a, l, r) => Eval.now(g(a, l, r))).value

  final def scanPostOrderM[F[_]: Monad, B](f: A => F[B])(g: (A, B, B) => F[B]): F[Tree[B]] =
    this match {
      case Node(value, left, right) =>
        for {
          l <- left.scanPostOrderM(f)(g)
          r <- right.scanPostOrderM(f)(g)
          b <- g(value, l.value, r.value)
        } yield Node(b, l, r)
      case Leaf(value) => f(value).map(Leaf(_))
    }

  final def ===[B >: A: Eq](that: Tree[B]): Boolean =
    (this eq that) || ((this, that) match {
      case (Node(thisValue, thisLeft, thisRight), Node(thatValue, thatLeft, thatRight)) =>
        (thisValue: B) === thatValue && thisLeft === thatLeft && thisRight === thatRight
      case (Leaf(thisValue), Leaf(thatValue)) =>
        (thisValue: B) === thatValue
      case _ => false
    })

  final def preorder: Button[A] = button

  final def postorder: Button[A] =
    Monad[Option]
      .iterateUntilM(button)(_.left)(_.at.isLeaf)
      // Should never be called
      .getOrElse(button)
}

object Tree {

  def value[A]: Lens[Tree[A], A] = new Lens[Tree[A], A] {
    override def get(s: Tree[A]): A = s.value
    override def modifyF[F[_]: Functor](f: A => F[A])(s: Tree[A]): F[Tree[A]] = s match {
      case Node(value, left, right) => f(value).map(Node(_, left, right))
      case Leaf(value) => f(value).map(Leaf(_))
    }
  }

  def left[A]: Optional[Tree[A], Tree[A]] = new Optional[Tree[A], Tree[A]] {
    override def modifyA[F[_]: Applicative](f: Tree[A] => F[Tree[A]])(s: Tree[A]): F[Tree[A]] =
      s match {
        case Node(value, left, right) => f(left).map(Node(value, _, right))
        case leaf => leaf.pure
      }

    override def getOrModify(s: Tree[A]): Either[Tree[A], Tree[A]] = s match {
      case Node(_, left, _) => Right(left)
      case leaf => Left(leaf)
    }

    override def getOption(s: Tree[A]): Option[Tree[A]] = s.leftOption
  }

  def right[A]: Optional[Tree[A], Tree[A]] = new Optional[Tree[A], Tree[A]] {
    override def modifyA[F[_]: Applicative](f: Tree[A] => F[Tree[A]])(s: Tree[A]): F[Tree[A]] =
      s match {
        case Node(value, left, right) => f(right).map(Node(value, left, _))
        case leaf => leaf.pure
      }

    override def getOrModify(s: Tree[A]): Either[Tree[A], Tree[A]] = s match {
      case Node(_, left, _) => Right(left)
      case leaf => Left(leaf)
    }

    override def getOption(s: Tree[A]): Option[Tree[A]] = s.rightOption
  }

  type Oriented[+A] = Either[A, A]

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

    def isLeft: Boolean = up.flatMap(_.left).exists(_ == this)

    def isRight: Boolean = up.flatMap(_.right).exists(_ == this)

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
          implicit val eq = Eq.fromUniversalEquals[A]
          val oldChild = at
          val newChild = tree
          val newParent =
            if (oldParent.isLeft(oldChild))
              oldParent.copy(left = newChild)
            else
              oldParent.copy(right = newChild)

          ancestry
            .view
            .scanLeft((oldParent, newParent)) {
              case ((oldChild, newChild), oldParent) =>
                val newParent =
                  if (oldParent.isLeft(oldChild))
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
