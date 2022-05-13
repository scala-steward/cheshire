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
import cats.syntax.all.*
import cats.~>

import GenTree.*

sealed abstract class GenTree[+N, +L]:
  def valueEither: Either[N, L]
  def isLeaf: Boolean
  def leftOption: Option[GenTree[N, L]]
  def rightOption: Option[GenTree[N, L]]

  final def button: Button[N, L] = Button(this, Nil)

  final def startPreOrder: Button[N, L] = button

  final def startPostOrder: Button[N, L] =
    Monad[Option]
      .iterateUntilM(button)(_.left)(_.at.isLeaf)
      // Should never be called
      .getOrElse(button)

  final def size: Int = reducePostOrder(_ => 1)((l, r, _) => 1 + l + r)
  final def leafCount: Int = (size + 1) / 2
  final def nodeCount: Int = leafCount - 1

  final def isLeft[N1 >: N: Eq, L1 >: L: Eq](that: GenTree[N1, L1]): Boolean =
    leftOption.exists(_ === that)

  final def isLeftOf[N1 >: N: Eq, L1 >: L: Eq](that: GenTree[N1, L1]): Boolean =
    that.isLeft(this)

  final def isRight[N1 >: N: Eq, L1 >: L: Eq](that: GenTree[N1, L1]): Boolean =
    rightOption.exists(_ === that)

  final def isRightOf[N1 >: N: Eq, L1 >: L: Eq](that: GenTree[N1, L1]): Boolean =
    that.isRight(this)

  final def sibling[N1 >: N: Eq, L1 >: L: Eq](that: GenTree[N1, L1]): Option[GenTree[N1, L1]] =
    if isRight(that) then leftOption else if isLeft(that) then rightOption else None

  final def bimap[N1, L1](f: N => N1, g: L => L1): GenTree[N1, L1] =
    bitraverse(n => Eval.now(f(n)), l => Eval.now(g(l))).value

  final def bitraverse[F[_]: Apply, N1, L1](f: N => F[N1], g: L => F[L1]): F[GenTree[N1, L1]] =
    this match
      case Node(value, left, right) =>
        (f(value), left.bitraverse(f, g), right.bitraverse(f, g)).mapN(Node(_, _, _))
      case Leaf(value) => g(value).map(Leaf(_))

  final def reducePostOrder[Z](f: L => Z)(g: (Z, Z, N) => Z): Z =
    reducePostOrderM(a => Eval.now(f(a)))((l, r, a) => Eval.now(g(l, r, a))).value

  final def reducePostOrderM[F[_]: Monad, Z](f: L => F[Z])(g: (Z, Z, N) => F[Z]): F[Z] =
    this match
      case Node(value, left, right) =>
        (
          left.reducePostOrderM(f)(g),
          right.reducePostOrderM(f)(g)
        ).mapN(g(_, _, value)).flatten
      case Leaf(value) => f(value)

  final def scanPostOrder[Z](f: L => Z)(g: (Z, Z, N) => Z): GenTree[Z, Z] =
    scanPostOrderM(a => Eval.now(f(a)))((l, r, a) => Eval.now(g(l, r, a))).value

  final def scanPostOrderM[F[_]: Monad, Z](f: L => F[Z])(
      g: (Z, Z, N) => F[Z]): F[GenTree[Z, Z]] =
    this match
      case Node(value, left, right) =>
        (
          left.scanPostOrderM(f)(g),
          right.scanPostOrderM(f)(g)
        ).mapN { (l, r) => g(l.value, r.value, value).map(Node(_, l, r)) }.flatten
      case Leaf(value) => f(value).map(Leaf(_))

  final def preOrder[F[_]: Monad](f: Button[N, L] => F[Unit]): F[Unit] =
    button.preOrder(f)

  final def postOrder[F[_]: Monad](f: Button[N, L] => F[Unit]): F[Unit] =
    button.postOrder(f)

  final def ===[N1 >: N: Eq, L1 >: L: Eq](that: GenTree[N1, L1]): Boolean =
    def recurse(x: GenTree[N1, L1], y: GenTree[N1, L1]): Eval[Boolean] = (x, y) match
      case (Node(xn, xl, xr), Node(yn, yl, yr)) if xn === yn =>
        recurse(xl, yl).flatMap {
          if _ then recurse(xr, yr)
          else Eval.False
        }
      case (Leaf(xl), Leaf(yl)) if xl === yl => Eval.True
      case _ => Eval.False
    recurse(this, that).value

  final def show[N1 >: N: Show, L1 >: L: Show]: String =
    reducePostOrder(l => s"Leaf(${(l: L1).show})") { (l, r, n) =>
      s"Node(${(n: N1).show}, $l, $r)"
    }

object GenTree extends GenTreeInstances:
  type Oriented[+A] = Either[A, A]

  def unapply[N, L](
      tree: GenTree[N, L]): Some[(Either[N, L], Option[GenTree[N, L]], Option[GenTree[N, L]])] =
    Some((tree.valueEither, tree.leftOption, tree.rightOption))

  def apply[L](value: L): GenTree[Nothing, L] = Leaf(value)
  def apply[N, L](value: N, left: GenTree[N, L], right: GenTree[N, L]): GenTree[N, L] =
    Node(value, left, right)

  final case class Node[+N, +L](value: N, left: GenTree[N, L], right: GenTree[N, L])
      extends GenTree[N, L]:
    override def valueEither = Left(value)
    override def isLeaf = false
    override def leftOption = Some(left)
    override def rightOption = Some(right)

  final case class Leaf[+L](value: L) extends GenTree[Nothing, L]:
    override def valueEither = Right(value)
    override def isLeaf = true
    override def leftOption = None
    override def rightOption = None

  final case class Button[+N, +L](at: GenTree[N, L], ancestry: List[Node[N, L]]):

    def tree: GenTree[N, L] = ancestry.lastOption.getOrElse(at)

    def up: Option[Button[N, L]] = ancestry match
      case at :: ancestry => Some(Button(at, ancestry))
      case _ => None

    def left: Option[Button[N, L]] = at match
      case at @ Node(_, left, _) => Some(Button(left, at :: ancestry))
      case _ => None

    def right: Option[Button[N, L]] = at match
      case at @ Node(_, _, right) => Some(Button(right, at :: ancestry))
      case _ => None

    def isLeft: Boolean = up.flatMap(_.left).exists(_ eq this)

    def isRight: Boolean = up.flatMap(_.right).exists(_ eq this)

    def sibling: Option[Button[N, L]] =
      if isLeft then up.flatMap(_.right)
      else if isRight then up.flatMap(_.left)
      else None

    def preOrder[F[_]](f: Button[N, L] => F[Unit])(using F: Monad[F]): F[Unit] =
      f(this) >> left.fold(F.unit)(_.preOrder(f)) *> right.fold(F.unit)(_.preOrder(f))

    def postOrder[F[_]](f: Button[N, L] => F[Unit])(using F: Monad[F]): F[Unit] =
      left.fold(F.unit)(_.postOrder(f)) *> right.fold(F.unit)(_.postOrder(f)) >> f(this)

    def preOrderSuccessor: Option[Button[N, L]] =
      left.orElse {
        Monad[Option].iterateUntilM(this)(_.up)(_.isLeft).flatMap(_.sibling)
      }

    def postOrderSuccessor: Option[Button[N, L]] =
      if isLeft then sibling.flatMap(Monad[Option].iterateUntilM(_)(_.left)(_.at.isLeaf))
      else up

    def replace[N1 >: N, L1 >: L](tree: GenTree[N1, L1]): Button[N1, L1] = Button(
      tree,
      NonEmptyList.fromList(ancestry).fold(List.empty[Node[N1, L1]]) {
        case NonEmptyList(oldParent, ancestry) =>
          val oldChild = at
          val newChild = tree
          val newParent =
            if oldParent.left eq oldChild then oldParent.copy(left = newChild)
            else oldParent.copy(right = newChild)

          ancestry
            .view
            .scanLeft((oldParent, newParent)) {
              case ((oldChild, newChild), oldParent) =>
                val newParent =
                  if oldParent eq oldChild then oldParent.copy(left = newChild)
                  else oldParent.copy(right = newChild)
                (oldParent, newParent)
            }
            .map(_._2)
            .toList
      }
    )

type Tree[A] = GenTree[A, A]
extension [A](tree: Tree[A])
  def value: A = tree match
    case Node(value, _, _) => value
    case Leaf(value) => value

  def map[B](f: A => B): Tree[B] =
    tree.bimap(f, f)

  def traverse[F[_]: Apply, B](f: A => F[B]): F[Tree[B]] =
    tree.bitraverse(f, f)

  def mapWithParent[B](f: A => B)(g: (A, A) => B): Tree[B] =
    traverseWithParent(a => Eval.now(f(a)))((p, c) => Eval.now(g(p, c))).value

  def traverseWithParent[F[_]: Applicative, B](f: A => F[B])(g: (A, A) => F[B]): F[Tree[B]] =
    tree match
      case Node(value, left, right) =>
        (
          f(value),
          left.traverseWithParent(g(value, _))(g),
          right.traverseWithParent(g(value, _))(g)).mapN(Node(_, _, _))
      case Leaf(value) => f(value).map(Leaf(_))

  def mapWithOrientedParent[B](f: A => B)(g: (Oriented[A], A) => B): Tree[B] =
    traverseWithOrientedParent(a => Eval.now(f(a)))((p, c) => Eval.now(g(p, c))).value

  def traverseWithOrientedParent[F[_]: Apply, B](f: A => F[B])(
      g: (Oriented[A], A) => F[B]): F[Tree[B]] = tree match
    case Node(value, left, right) =>
      (
        f(value),
        left.traverseWithOrientedParent(g(Left(value), _))(g),
        right.traverseWithOrientedParent(g(Right(value), _))(g)).mapN(Node(_, _, _))
    case Leaf(value) => f(value).map(Leaf(_))

  def mapWithChildren[B](f: A => B)(g: (A, A, A) => B): Tree[B] =
    traverseWithChildren(a => Eval.now(f(a)))((a, l, r) => Eval.now(g(a, l, r))).value

  def traverseWithChildren[F[_]: Apply, B](f: A => F[B])(g: (A, A, A) => F[B]): F[Tree[B]] =
    tree match
      case Node(value, left, right) =>
        (
          g(value, left.value, right.value),
          left.traverseWithChildren(f)(g),
          right.traverseWithChildren(f)(g)).mapN(Node(_, _, _))
      case Leaf(value) => f(value).map(Leaf(_))

  def flatMap[B](f: A => Tree[B]): Tree[B] =
    flatTraverse(a => Eval.now(f(a))).value

  def flatTraverse[F[_]: Apply, B](f: A => F[Tree[B]]): F[Tree[B]] = tree match
    case Node(value, left, right) =>
      (f(value), left.flatTraverse(f), right.flatTraverse(f)).mapN { (fb, flb, frb) =>
        def recurse(fb: Tree[B]): Eval[Tree[B]] = fb match
          case Node(value, left, right) =>
            (recurse(left), recurse(right)).mapN(Node(value, _, _))
          case Leaf(value) => Eval.now(Node(value, flb, frb))
        recurse(fb).value
      }
    case Leaf(value) => f(value)

  def coflatMap[B](f: Tree[A] => B): Tree[B] =
    coflatTraverse(fa => Eval.now(f(fa))).value

  def coflatTraverse[F[_]: Apply, B](f: Tree[A] => F[B]): F[Tree[B]] = tree match
    case node @ Node(_, left, right) =>
      (f(node), left.coflatTraverse(f), right.coflatTraverse(f)).mapN(Node(_, _, _))
    case leaf => f(leaf).map(Leaf(_))

  def mapWithButton[B](f: Button[A, A] => B): Tree[B] =
    traverseWithButton(c => Eval.now(f(c))).value

  def traverseWithButton[F[_]: Monad, B](f: Button[A, A] => F[B]): F[Tree[B]] =
    def recurse(button: Button[A, A]): F[Tree[B]] =
      (button.left, button.right) match
        case (Some(left), Some(right)) =>
          (recurse(left), recurse(right), f(button)).mapN((l, r, b) => Node(b, l, r))
        case _ => f(button).map(Leaf(_))
    recurse(tree.button)

  def scanPreOrder[B](f: A => B)(g: (B, A) => B): Tree[B] =
    scanPreOrderM(a => Eval.now(f(a)))((b, a) => Eval.now(g(b, a))).value

  def scanPreOrderM[F[_]: Monad, B](f: A => F[B])(g: (B, A) => F[B]): F[Tree[B]] =
    tree match
      case Node(value, left, right) =>
        f(value).flatMap { b =>
          (
            left.scanPreOrderM(g(b, _))(g),
            right.scanPreOrderM(g(b, _))(g)
          ).mapN(Node(b, _, _))
        }
      case Leaf(value) => f(value).map(Leaf(_))

  def scanPreOrderOriented[B](f: A => B)(g: (Oriented[B], A) => B): Tree[B] =
    scanPreOrderOrientedM(a => Eval.now(f(a)))((b, a) => Eval.now(g(b, a))).value

  def scanPreOrderOrientedM[F[_]: Monad, B](f: A => F[B])(
      g: (Oriented[B], A) => F[B]): F[Tree[B]] =
    tree match
      case Node(value, left, right) =>
        f(value).flatMap { b =>
          (
            left.scanPreOrderOrientedM(g(Left(b), _))(g),
            right.scanPreOrderOrientedM(g(Right(b), _))(g)
          ).mapN(Node(b, _, _))
        }
      case Leaf(value) => f(value).map(Leaf(_))

  def zip[B](that: Tree[B]): Tree[(A, B)] =
    zipWithM(that)((a, b) => Eval.now((a, b))).value

  def zipWith[B, Z](that: Tree[B])(f: (A, B) => Z): Tree[Z] =
    zipWithM(that)((a, b) => Eval.now(f(a, b))).value

  def zipWithM[F[_]: Monad, B, Z](that: Tree[B])(f: (A, B) => F[Z]): F[Tree[Z]] =
    (tree, that: Tree[B]) match
      case (Node(a, thisLeft, thisRight), Node(b, thatLeft, thatRight)) =>
        (
          f(a, b),
          thisLeft.zipWithM(thatLeft)(f),
          thisRight.zipWithM(thatRight)(f)
        ).mapN(Node(_, _, _))
      case _ => f(tree.value, that.value).map(Leaf(_))

sealed abstract private[cheshire] class GenTreeInstances:

  given Bitraverse[GenTree] with

    override def bimap[A, B, C, D](fab: GenTree[A, B])(f: A => C, g: B => D): GenTree[C, D] =
      fab.bimap(f, g)

    override def bitraverse[G[_]: Applicative, A, B, C, D](
        fab: GenTree[A, B])(f: A => G[C], g: B => G[D]): G[GenTree[C, D]] =
      fab.bitraverse(f, g)

    override def bifoldLeft[A, B, C](fab: GenTree[A, B], c: C)(
        f: (C, A) => C,
        g: (C, B) => C): C =
      def recurse(fab: GenTree[A, B], c: C): Eval[C] = fab match
        case Node(value, left, right) =>
          for
            l <- recurse(left, c)
            r <- recurse(right, l)
          yield f(r, value)
        case Leaf(value) => Eval.now(g(c, value))
      recurse(fab, c).value

    override def bifoldRight[A, B, C](fab: GenTree[A, B], c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]): Eval[C] = fab match
      case Node(value, left, right) =>
        val c1 = Eval.defer(f(value, c))
        val r = Eval.defer(bifoldRight(right, c1)(f, g))
        Eval.defer(bifoldRight(left, r)(f, g))
      case Leaf(value) => Eval.defer(g(value, c))

  given Bimonad[Tree] with NonEmptyTraverse[Tree] with Align[Tree] with

    override def functor: Functor[Tree] = this

    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def extract[A](x: Tree[A]): A = x.value

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa.map(f)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa.flatMap(f)

    override def coflatMap[A, B](fa: Tree[A])(f: Tree[A] => B): Tree[B] =
      fa.coflatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      val g = (a: A) => Eval.now(f(a))
      def tailRecEval(a: A): Eval[Tree[B]] =
        g(a).flatMap { ab =>
          ab.flatTraverse {
            case Left(a) => tailRecEval(a)
            case Right(b) => Eval.now(pure(b))
          }
        }
      tailRecEval(a).value

    override def foldMap[A, B: Monoid](fa: Tree[A])(f: A => B): B =
      fa.reducePostOrder(f)((l, r, a) => l |+| r |+| f(a))

    override def foldLeft[A, B](fa: Tree[A], b: B)(f: (B, A) => B): B =
      reduceLeftTo(fa)(f(b, _))(f)

    override def foldRight[A, B](fa: Tree[A], lb: Eval[B])(
        f: (A, Eval[B]) => Eval[B]): Eval[B] =
      reduceRightToImpl(fa)(f(_, lb))(f)

    override def reduceLeftTo[A, B](fa: Tree[A])(f: A => B)(g: (B, A) => B): B =
      def recurse(fa: Tree[A])(f: A => B): Eval[B] = fa match
        case Node(value, left, right) =>
          for
            l <- recurse(left)(f)
            r <- recurse(right)(g(l, _))
          yield g(r, value)
        case Leaf(value) => Eval.now(f(value))
      recurse(fa)(f).value

    override def reduceRightTo[A, B](fa: Tree[A])(f: A => B)(
        g: (A, Eval[B]) => Eval[B]): Eval[B] =
      reduceRightToImpl(fa)(a => Eval.later(f(a)))(g)

    private def reduceRightToImpl[A, B](fa: Tree[A])(f: A => Eval[B])(
        g: (A, Eval[B]) => Eval[B]): Eval[B] = fa match
      case Node(value, left, right) =>
        val b = Eval.defer(f(value))
        val r = Eval.defer(reduceRightToImpl(right)(g(_, b))(g))
        Eval.defer(reduceRightToImpl(left)(g(_, r))(g))
      case Leaf(value) => Eval.defer(f(value))

    override def traverse[G[_]: Applicative, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] =
      fa.traverse(f)

    override def flatTraverse[G[_]: Applicative, A, B](fa: Tree[A])(f: A => G[Tree[B]])(
        using FlatMap[Tree]): G[Tree[B]] =
      fa.flatTraverse(f)

    override def nonEmptyTraverse[G[_]: Apply, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] =
      fa.traverse(f)

    override def nonEmptyFlatTraverse[G[_]: Apply, A, B](fa: Tree[A])(f: A => G[Tree[B]])(
        using FlatMap[Tree]): G[Tree[B]] =
      fa.flatTraverse(f)

    override def align[A, B](fa: Tree[A], fb: Tree[B]): Tree[Ior[A, B]] =
      alignEval(fa, fb).value

    private def alignEval[A, B](fa: Tree[A], fb: Tree[B]): Eval[Tree[Ior[A, B]]] =
      (fa, fb) match
        case (Node(a, la, ra), Node(b, lb, rb)) =>
          (
            alignEval(la, lb),
            alignEval(ra, rb)
          ).mapN(Node(Ior.both(a, b), _, _))
        case (Leaf(a), Leaf(b)) => Eval.now(Leaf(Ior.both(a, b)))
        case (Node(a, la, ra), Leaf(b)) =>
          (
            la.traverse(a => Eval.now(Ior.left(a))),
            ra.traverse(a => Eval.now(Ior.left(a)))
          ).mapN(Node(Ior.both(a, b), _, _))
        case (Leaf(a), Node(b, lb, rb)) =>
          (
            lb.traverse(b => Eval.now(Ior.right(b))),
            rb.traverse(b => Eval.now(Ior.right(b)))
          ).mapN(Node(Ior.both(a, b), _, _))

  given NonEmptyParallel.Aux[Tree, ZipTree] =
    new NonEmptyParallel[Tree]:
      type F[X] = ZipTree[X]

      override def flatMap: FlatMap[Tree] = summon[FlatMap[Tree]]
      override def apply: Apply[ZipTree] = summon[Apply[ZipTree]]

      override def parallel: Tree ~> ZipTree =
        new Tree ~> ZipTree:
          override def apply[A](fa: Tree[A]): ZipTree[A] = ZipTree(fa)

      override def sequential: ZipTree ~> Tree =
        new ZipTree ~> Tree:
          override def apply[A](fa: ZipTree[A]): Tree[A] = fa.value

  given [N: Show, L: Show]: Show[GenTree[N, L]] with
    override def show(t: GenTree[N, L]): String = t.show

  given [N: Eq, L: Eq]: Eq[GenTree[N, L]] with
    override def eqv(x: GenTree[N, L], y: GenTree[N, L]): Boolean =
      x === y

opaque type ZipTree[+A] = Tree[A]

object ZipTree:
  def apply[A](tree: Tree[A]): ZipTree[A] = tree
  extension [A](tree: ZipTree[A]) def value: Tree[A] = tree

  given CommutativeApply[ZipTree] with
    override def map[A, B](fa: ZipTree[A])(f: A => B): ZipTree[B] =
      fa.map(f)

    override def ap[A, B](ff: ZipTree[A => B])(fa: ZipTree[A]): ZipTree[B] =
      ff.zipWith(fa)(_(_))

  given [A: Eq]: Eq[ZipTree[A]] = GenTree.given_Eq_GenTree
