package fpinscala.monoids

import fpinscala.parallelism.Nonblocking.*
import language.higherKinds

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid = new Monoid[String]:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A] = new Monoid[List[A]]:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  val intAddition: Monoid[Int] = ???

  val intMultiplication: Monoid[Int] = ???

  val booleanOr: Monoid[Boolean] = ???

  val booleanAnd: Monoid[Boolean] = ???

  def optionMonoid[A]: Monoid[Option[A]] = ???

  def endoMonoid[A]: Monoid[A => A] = ???

  import fpinscala.testing.*
  import Prop.*

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    ???

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    ???

  def ordered(ints: IndexedSeq[Int]): Boolean =
    ???

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    ???

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    ???

  val wcMonoid: Monoid[WC] = ???

  def count(s: String): Int = ???

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    ???

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    ???

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???
end Monoid

trait Foldable[F[_]]:
  import Monoid.*

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    ???

  def toList[A](as: F[A]): List[A] =
    ???

object ListFoldable extends Foldable[List]:
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    ???

object IndexedSeqFoldable extends Foldable[IndexedSeq]:
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    ???

object LazyListFoldable extends Foldable[LazyList]:
  override def foldRight[A, B](as: LazyList[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: LazyList[A])(z: B)(f: (B, A) => B) =
    ???

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

object TreeFoldable extends Foldable[Tree]:
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    ???
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    ???

object OptionFoldable extends Foldable[Option]:
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    ???
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    ???

