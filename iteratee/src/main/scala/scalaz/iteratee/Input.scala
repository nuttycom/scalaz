package scalaz
package iteratee

import LazyOption._
import Iteratee._

/**The input to an iteratee. */
sealed abstract class Input[+E] private () {
  @inline final def fold[Z](empty: => Z, el: (=> E) => Z, eof: => Z): Z = this match {
    case Input.Empty      => empty
    case Input.Element(e) => el(e)
    case Input.Eof        => eof
  }

  @inline final def apply[Z](empty: => Z, el: (=> E) => Z, eof: => Z) =
    fold(empty, el, eof)

  def el: Option[E]

  def elOr[B >: E](e: => B): B

  val isEmpty: Boolean

  val isEl: Boolean

  val isEof: Boolean

  def map[X](f: (=> E) => X): Input[X]

  def flatMap[X](f: (=> E) => Input[X]): Input[X]

  def filter(f: (=> E) => Boolean): Input[E]

  def foreach(f: (=> E) => Unit): Unit

  def forall(p: (=> E) => Boolean): Boolean

  def exists(p: (=> E) => Boolean): Boolean
}

object Input extends InputFunctions with InputInstances {
  def apply[E](e: => E): Input[E] =
    elInput(e)

   private[iteratee] case class Element[E](element: E) extends Input[E] {
    @inline final val el: Option[E] = Some(element)

    @inline final def elOr[B >: E](e: => B): B = element

    @inline final val isEmpty: Boolean = false

    @inline final val isEl: Boolean = true

    @inline final val isEof: Boolean = false

    @inline final def map[X](f: (=> E) => X): Input[X] = Element(f(element))

    @inline final def flatMap[X](f: (=> E) => Input[X]): Input[X] = f(element)

    @inline final def filter(f: (=> E) => Boolean): Input[E] = if (f(element)) this else Empty

    @inline final def foreach(f: (=> E) => Unit) = f(element)

    @inline final def forall(p: (=> E) => Boolean): Boolean = p(element)

    @inline final def exists(p: (=> E) => Boolean): Boolean = p(element)

    @inline override final def toString = element.toString
  }

   private[iteratee] abstract class NonExistentInput extends Input[Nothing] {
    @inline final val el: Option[Nothing] = None

    @inline final def elOr[B >: Nothing](e: => B): B = e

    @inline final def map[X](f: (=> Nothing) => X): Input[X] = this

    @inline final def flatMap[X](f: (=> Nothing) => Input[X]): Input[X] = this

    @inline final def filter(f: (=> Nothing) => Boolean): Input[Nothing] = this

    @inline final def foreach(f: (=> Nothing) => Unit) = ()

    @inline final def forall(p: (=> Nothing) => Boolean): Boolean = true

    @inline final def exists(p: (=> Nothing) => Boolean): Boolean = false
  }

  private[iteratee] case object Empty extends NonExistentInput {
    @inline final val isEmpty: Boolean = true

    @inline final val isEl: Boolean = false

    @inline final val isEof: Boolean = false

    @inline override final def toString = "Empty"
  }

  private[iteratee] case object Eof extends NonExistentInput {
    @inline final val isEmpty: Boolean = false

    @inline final val isEl: Boolean = false

    @inline final val isEof: Boolean = true

    @inline override final def toString = "Eof"
  }
  
}

trait InputInstances {
  import Input._

  implicit val input = new Traverse[Input] with MonadPlus[Input] with Each[Input] with Length[Input] {
    def length[A](fa: Input[A]): Int = if (fa.isEl) 1 else 0

    def point[A](a: => A): Input[A] = elInput(a)

    def traverseImpl[G[_]: Applicative, A, B](fa: Input[A])(f: (A) => G[B]): G[Input[B]] = fa match {
      case Empty      => Applicative[G].point(Empty)
      case Element(e) => Applicative[G].map(f(fa.el.get))(b => elInput(b))
      case Eof        => Applicative[G].point(Eof)
    }
    
    override def foldRight[A, B](fa: Input[A], z: => B)(f: (A, => B) => B): B = fa match {
      case Element(a) => f(a, z)
      case _          => z
    }

    def each[A](fa: Input[A])(f: (A) => Unit) = fa.foreach(a => f(a))

    def plus[A](a: Input[A], b: => Input[A]): Input[A] = a match {
      case Element(_) => a
      case _          => b
    }

     def bind[A, B](fa: Input[A])(f: (A) => Input[B]): Input[B] = fa.flatMap(a => f(a))

     def empty[A]: Input[A] = emptyInput
   }

   implicit def inputMonoid[A](implicit A: Monoid[A]) = new Monoid[Input[A]] {
     def append(a1: Input[A], a2: => Input[A]): Input[A] = a1.fold(
       empty = a2.fold(
         empty = emptyInput
         , el = elInput
         , eof = eofInput
       )
       , el = xa => a2.fold(
         empty = elInput(xa)
         , el = ya => elInput(A.append(xa, ya))
         , eof = eofInput
       )
       , eof = eofInput
     )

     def zero: Input[A] = emptyInput
   }

   implicit def inputEqual[A](implicit A: Equal[A]) = new Equal[Input[A]] {
     def equal(a1: Input[A], a2: Input[A]): Boolean = a1.fold(
       empty = a2.isEmpty
       , el = a => a2.exists(z => A.equal(a, z))
       , eof = a2.isEmpty
     )
   }

   implicit def inputShow[A](implicit A: Show[A]) = new Show[Input[A]] {
     def show(f: Input[A]): List[Char] = f.fold(
       empty = "empty-input"
       , el = a => "el-input(" + A.shows(a) + ")"
       , eof = "eof-input"
     ).toList
   }
}

trait InputFunctions {
  @inline final def emptyInput[E]: Input[E] = Input.Empty
  @inline final def elInput[E](e: => E): Input[E] = Input.Element(e)
  @inline final def eofInput[E]: Input[E] = Input.Eof
}
