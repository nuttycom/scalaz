package scalaz
package typelevel

/**
 * Represents a function `A => F[B]` where `[F: TC]`.
 *
 * Contributed by Eugene Yokota.
 */
trait Func[F[_], TC[F[_]] <: Functor[F], A, B] { self =>
  def runA(a: A): F[B]
  implicit def TC: KTypeClass[TC]
  implicit def F: TC[F]

  import Func._
  import syntax.typelevel.hlist._

  /** alias for andThenA */
  def @>>>[G[_], C](g: Func[G, TC, B, C]) = andThenA(g)

  /** compose `A => F[B]` and `B => G[C]` into `A => F[G[C]]` */
  def andThenA[G[_], C](g: Func[G, TC, B, C]) = g.composeA(self)

  /** alias for composeA */
  def <<<@[G[_], C](g: Func[G, TC, C, A]) = composeA(g)

  /** compose `A => F[B]` and `C => G[A]` into `C => G[F[B]]` */
  def composeA[G[_], C](g: Func[G, TC, C, A]): Func[({type λ[α] = G[F[α]]})#λ, TC, C, B] = new Func[({type λ[α] = G[F[α]]})#λ, TC, C, B] {
    def runA(c: C): G[F[B]] = g.F.map(g.runA(c): G[A]) { a: A => self.runA(a) }
    def F = (g.F <<: self.F <<: TC.idCompose).instance
    def TC = self.TC
  }

  /** alias for productA  */
  def @&&&[G[_]](g: Func[G, TC, A, B]) = productA(g)

  def &&&@[G[_]](g: Func[G, TC, A, B]) = g.productA(self)
  
  /** compose `A => F[B]` and `A => G[B]` into `A => F[B] :: G[B] :: HNil` */
  def productA[G[_]](g: Func[G, TC, A, B]) = consA(g consA hnilfunc[TC, A, B])

  /** prepend `this` to HListFunc `tail` **/
  def consA[T <: TCList](tail: HListFunc[T, TC, A, B]): HListFunc[TCCons[F, T], TC, A, B] = new HListFunc[TCCons[F, T], TC, A, B] {
    def runA(a: A): TCCons[F, T]#Product[B] = self.runA(a) :: tail.runA(a)
    def Product = self.F *: tail.Product
    def TC = self.TC
  }

  def mapA[C](f: B => C): Func[F, TC, A, C] =
    func(a => F.map(self.runA(a))(f))

  def traverse[G[_]](value: G[A])(implicit G: Traverse[G], ev: TC[F] =:= Applicative[F]): F[G[B]] =
    G.traverse(value)(a => self.runA(a))(ev(F))
}

/**
 * Represents a function `A => T#Product[B]` where `[T <: TCList]`.
 *
 * Example:
 * {{{
 * import scalaz._, std.AllInstances._, typelevel._, Func._
 *
 * val list = AppFuncU { (x: Int) => x + 1 } :: AppFuncU { (x: Int) => List(x + 5) } :: AppFunc.HNil
 * list.traverse(List(1, 2, 3))
 * }}}
 */
trait HListFunc[T <: TCList, TC[X[_]] <: Functor[X], A, B] extends Func[T#Product, TC, A, B] { self =>
  def ::[G[_]](g: Func[G, TC, A, B]) = g consA self
  private[scalaz] def Product: KTypeClass.WrappedProduct[TC, T]
  final def F = Product.instance
}

//
// Prioritized Implicits for type class instances
//
trait FuncInstances3 {
  implicit def FuncFunctor[F[_], TC[X[_]] <: Functor[X], R]: Functor[({type λ[α] = Func[F, TC, R, α]})#λ] = new FuncFunctor[F, TC, R] {}
}

trait FuncInstances2 extends FuncInstances3 {
  implicit def FuncPointed[F[_], TC[F[_]] <: Pointed[F], R](implicit TC0: KTypeClass[TC], F0: TC[F]): Pointed[({type λ[α] = Func[F, TC, R, α]})#λ] = new FuncPointed[F, TC, R] {
    implicit def TC: KTypeClass[TC] = TC0
    implicit def F: TC[F] = F0
  }
  implicit def FuncApply[F[_], TC[F[_]] <: Apply[F], R](implicit TC0: KTypeClass[TC], F0: TC[F]): Apply[({type λ[α] = Func[F, TC, R, α]})#λ] = new FuncApply[F, TC, R] {
    implicit def TC: KTypeClass[TC] = TC0
    implicit def F: TC[F] = F0
  }
}

trait FuncInstances1 extends FuncInstances2 {
  implicit def FuncApplicative[F[_], TC[F[_]] <: Applicative[F], R](implicit TC0: KTypeClass[TC], F0: TC[F]): Applicative[({type λ[α] = Func[F, TC, R, α]})#λ] = new FuncApplicative[F, TC, R] {
    implicit def TC: KTypeClass[TC] = TC0
    implicit def F: TC[F] = F0
  }
}

trait FuncInstances extends FuncInstances1 {
}

trait FuncFunctions {
  def func[M[_], TC[M[_]] <: Functor[M], A, B](f: A => M[B])(implicit TC0: KTypeClass[TC], F0: TC[M]): Func[M, TC, A, B] = new Func[M, TC, A, B] {
    def TC = TC0
    def F = F0
    def runA(a: A) = f(a)
  }
  def functorfunc[M[_], A, B](f: A => M[B])(implicit F0: Functor[M]): Func[M, Functor, A, B] = func[M, Functor, A, B](f)  
  def functorfuncU[A, R](f: A => R)(implicit F0: Unapply[Functor, R]): Func[F0.M, Functor, A, F0.A] = new Func[F0.M, Functor, A, F0.A] {
    def TC = KTypeClass[Functor]
    def F = F0.TC
    def runA(a: A) = F0(f(a))
  }
  def pointedfunc[M[_], A, B](f: A => M[B])(implicit F0: Pointed[M]): Func[M, Pointed, A, B] = func[M, Pointed, A, B](f)
  def pointedfuncU[A, R](f: A => R)(implicit F0: Unapply[Pointed, R]): Func[F0.M, Pointed, A, F0.A] = new Func[F0.M, Pointed, A, F0.A] {
    def TC = KTypeClass[Pointed]
    def F = F0.TC
    def runA(a: A) = F0(f(a))
  }
  def applyfunc[M[_], A, B](f: A => M[B])(implicit F0: Apply[M]): Func[M, Apply, A, B] = func[M, Apply, A, B](f)
  def applyfuncU[A, R](f: A => R)(implicit F0: Unapply[Apply, R]): Func[F0.M, Apply, A, F0.A] = new Func[F0.M, Apply, A, F0.A] {
    def TC = KTypeClass[Apply]
    def F = F0.TC
    def runA(a: A) = F0(f(a))
  }
  def appfunc[M[_], A, B](f: A => M[B])(implicit F0: Applicative[M]): Func[M, Applicative, A, B] = func[M, Applicative, A, B](f)
  def appfuncU[A, R](f: A => R)(implicit F0: Unapply[Applicative, R]): Func[F0.M, Applicative, A, F0.A] = new Func[F0.M, Applicative, A, F0.A] {
    def TC = KTypeClass[Applicative]
    def F = F0.TC
    def runA(a: A) = F0(f(a))
  }
  def hnilfunc[TC[X[_]] <: Functor[X], A, B](implicit TC0: KTypeClass[TC]): HListFunc[TCNil, TC, A, B] = new HListFunc[TCNil, TC, A, B] {
    def TC = TC0
    def Product: KTypeClass.WrappedProduct[TC, TCNil] = TC0.emptyProduct
    def runA(a: A) = HNil
  }
}

object Func extends FuncFunctions with FuncInstances {
  def apply[M[_], TC[M[_]] <: Functor[M], A, B](f: A => M[B])(implicit TC0: KTypeClass[TC], F0: TC[M]): Func[M, TC, A, B] = func(f)
}

object FunctorFunc {
  def apply[M[_], A, B](f: A => M[B])(implicit F0: Functor[M]): Func[M, Functor, A, B] = Func.functorfunc(f)
  def HNil[A, B] = Func.hnilfunc[Functor, A, B]
}

object FunctorFuncU {
  def apply[A, R](f: A => R)(implicit F0: Unapply[Functor, R]): Func[F0.M, Functor, A, F0.A] = Func.functorfuncU(f)
}

object PointedFunc {
  def apply[M[_], A, B](f: A => M[B])(implicit F0: Pointed[M]): Func[M, Pointed, A, B] = Func.pointedfunc(f)
  def HNil[A, B] = Func.hnilfunc[Pointed, A, B]
}

object PointedFuncU {
  def apply[A, R](f: A => R)(implicit F0: Unapply[Pointed, R]): Func[F0.M, Pointed, A, F0.A] = Func.pointedfuncU(f)
}

object ApplyFunc {
  def apply[M[_], A, B](f: A => M[B])(implicit F0: Apply[M]): Func[M, Apply, A, B] = Func.applyfunc(f)
  def HNil[A, B] = Func.hnilfunc[Apply, A, B]
}

object ApplyFuncU {
  def apply[A, R](f: A => R)(implicit F0: Unapply[Apply, R]): Func[F0.M, Apply, A, F0.A] = Func.applyfuncU(f)
}

object AppFunc {
  def apply[M[_], A, B](f: A => M[B])(implicit F0: Applicative[M]): Func[M, Applicative, A, B] = Func.appfunc(f)
  def HNil[A, B] = Func.hnilfunc[Applicative, A, B]
}

object AppFuncU {
  def apply[A, R](f: A => R)(implicit F0: Unapply[Applicative, R]): Func[F0.M, Applicative, A, F0.A] = Func.appfuncU(f)
}

//
// Implementation traits for type class instances
//

import Func._

private[scalaz] trait FuncFunctor[F[_], TC[F[_]] <: Functor[F], R] extends Functor[({type λ[α] = Func[F, TC, R, α]})#λ] {
  override def map[A, B](fa: Func[F, TC, R, A])(f: A => B): Func[F, TC, R, B] = fa mapA f
}

private[scalaz] trait FuncPointed[F[_], TC[F[_]] <: Pointed[F], R] extends Pointed[({type λ[α] = Func[F, TC, R, α]})#λ] with FuncFunctor[F, TC, R] {
  implicit def TC: KTypeClass[TC]
  implicit def F: TC[F]
  def point[A](a: => A): Func[F, TC, R, A] = func((r: R) => F.point(a))
}

private[scalaz] trait FuncApply[F[_], TC[F[_]] <: Apply[F], R] extends Apply[({type λ[α] = Func[F, TC, R, α]})#λ] with FuncFunctor[F, TC, R] {
  implicit def TC: KTypeClass[TC]
  implicit def F: TC[F]
  def ap[A, B](fa: => Func[F, TC, R, A])(f: => Func[F, TC, R, (A) => B]): Func[F, TC, R, B] = func(r => F.ap(fa.runA(r))(f.runA(r)))
}

private[scalaz] trait FuncApplicative[F[_], TC[F[_]] <: Applicative[F], R] extends Applicative[({type λ[α] = Func[F, TC, R, α]})#λ] with FuncApply[F, TC, R] with FuncPointed[F, TC, R] {
  implicit def TC: KTypeClass[TC]
  implicit def F: TC[F]
}
