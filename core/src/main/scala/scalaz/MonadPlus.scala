package scalaz

////
/**
 *
 */
////
trait MonadPlus[F[_]] extends Monad[F] with ApplicativePlus[F] { self =>
  ////
  def filter[A](fa: F[A])(f: A => Boolean) =
    bind(fa)(a => if (f(a)) point(a) else empty[A])

  /** Generalized version of Haskell's `catMaybes` */
  def unite[T[_], A](value: F[T[A]])(implicit T: Foldable[T]): F[A] =
    bind(value)((ta) => T.foldMap(ta)(a => point(a))(monoid[A]))

  trait MonadPlusLaw extends EmptyLaw with MonadLaw {
    def emptyMap[A](f1: A => A)(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(map(empty[A])(f1), empty[A])

    def leftZero[A](f: A => F[A])(implicit FA: Equal[F[A]]): Boolean = {
      FA.equal(bind(empty[A])(f), empty[A])
    }
  }
  trait StrongMonadPlusLaw extends MonadPlusLaw {
    def rightZero[A](f: F[A])(implicit FA: Equal[F[A]]): Boolean = {
      FA.equal(bind(f)(_ => empty[A]), empty[A])
    }
  }
  def monadPlusLaw = new MonadPlusLaw {}
  def strongMonadPlusLaw = new StrongMonadPlusLaw {}
  ////
  val monadPlusSyntax = new scalaz.syntax.MonadPlusSyntax[F] { def F = MonadPlus.this }
}

object MonadPlus {
  @inline def apply[F[_]](implicit F: MonadPlus[F]): MonadPlus[F] = F

  ////

  ////
}

