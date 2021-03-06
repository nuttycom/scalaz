package scalaz

////
/**
 * Functors, covariant by nature if not by Scala type.  Their key
 * operation is `map`, whose behavior is constrained only by type and
 * the functor laws.
 *
 * Many useful functors also have natural [[scalaz.Apply]] or
 * [[scalaz.Bind]] operations.  Many also support
 * [[scalaz.Traverse]].
 *
 * @see [[scalaz.Functor.FunctorLaw]]
 */
////
trait Functor[F[_]]  { self =>
  ////

  /** Lift `f` into `F` and apply to `F[A]`. */
  def map[A, B](fa: F[A])(f: A => B): F[B]

  // derived functions

  /** Alias for `map`. */
  def apply[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f)

  /** Lift `f` into `F`. */
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)

  /** Inject `a` to the left of `B`s in `f`. */
  def strengthL[A, B](a: A, f: F[B]): F[(A, B)] = map(f)(b => (a, b))

  /** Inject `b` to the right of `A`s in `f`. */
  def strengthR[A, B](f: F[A], b: B): F[(A, B)] = map(f)(a => (a, b))

  /** Lift `apply(a)`, and apply the result to `f`. */
  def mapply[A, B](a: A)(f: F[A => B]): F[B] = map(f)((ff: A => B) => ff(a))

  /** Twin all `A`s in `fa`. */
  def fpair[A](fa: F[A]): F[(A, A)] = map(fa)(a => (a, a))

  /**
   * Empty `fa` of meaningful pure values, preserving its
   * structure.
   */
  def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ())

  def counzip[A, B](a: F[A] \/ F[B]): F[(A \/ B)] =
    a match {
      case -\/(x) => map(x)(-\/(_))
      case \/-(x) => map(x)(\/-(_))
    }

  /**The composition of Functors `F` and `G`, `[x]F[G[x]]`, is a Functor */
  def compose[G[_]](implicit G0: Functor[G]): Functor[({type λ[α] = F[G[α]]})#λ] = new CompositionFunctor[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /**The product of Functors `F` and `G`, `[x](F[x], G[x]])`, is a Functor */
  def product[G[_]](implicit G0: Functor[G]): Functor[({type λ[α] = (F[α], G[α])})#λ] = new ProductFunctor[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  trait FunctorLaw {
    /** The identity function, lifted, is a no-op. */
    def identity[A](fa: F[A])(implicit FA: Equal[F[A]]): Boolean = FA.equal(map(fa)(x => x), fa)

    /**
     * A series of maps may be freely rewritten as a single map on a
     * composed function.
     */
    def composite[A, B, C](fa: F[A], f1: A => B, f2: B => C)(implicit FC: Equal[F[C]]): Boolean = FC.equal(map(map(fa)(f1))(f2), map(fa)(f2 compose f1))
  }
  def functorLaw = new FunctorLaw {}
  ////
  val functorSyntax = new scalaz.syntax.FunctorSyntax[F] { def F = Functor.this }
}

object Functor {
  @inline def apply[F[_]](implicit F: Functor[F]): Functor[F] = F

  ////

  ////
}

