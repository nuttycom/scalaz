package scalaz
package iteratee

import effect._
import Iteratee._

/**
 * A data sink.
 *
 * Represents a value of type `F[StepT[X, E, F, A]]`
 *
 * @see [[scalaz.iteratee.StepT]]
 *
 * @tparam X The type of the error (mnemonic: e'''X'''ception type)
 * @tparam E The type of the input data (mnemonic: '''E'''lement type)
 * @tparam F The type constructor representing an effect.
 *           The type constructor [[scalaz.Id]] is used to model pure computations, and is fixed as such in the type alias [[scalaz.Iteratee]].
 * @tparam A The type of the calculated result
 */
final class IterateeT[X, E, F[_], A](final val value: F[StepT[X, E, F, A]]) {
  @inline final def foldT[Z](
                cont: (Input[E] => IterateeT[X, E, F, A]) => F[Z]
                , done: (=> A, => Input[E]) => F[Z]
                , err: (=> X) => F[Z]
                )(implicit F: Bind[F]): F[Z] =
    F.bind(value)((s: StepT[X, E, F, A]) => s(cont, done, err))

  /**
   * Run this iteratee
   *
   * @param e A function to calculate a result in case of an Error.
   */
  final def run(e: (=> X) => F[A])(implicit F: Monad[F]): F[A] = {
    F.bind((this &= enumEofT[X, E, F]).value)((s: StepT[X, E, F, A]) => s.fold(
      cont = _ => sys.error("diverging iteratee")
      , done = (a, _) => F.point(a)
      , err = x => e(x)
    ))
  }

  /** An alias for `run` */
  final def apply(e: (=> X) => F[A])(implicit F: Monad[F]): F[A] = run(e)

  /** Like `run`, but uses `Monoid[F[A]].zero` in the error case */
  final def runOrZero(implicit F: Monad[F], FA: Monoid[F[A]]): F[A] = apply(_ => FA.zero)

  final private def flatMapThrough[B](x: IterateeT[X, E, F, A], f: A => IterateeT[X, E, F, B], F: Monad[F]): IterateeT[X, E, F, B] =
    iterateeT(
      F.bind(x.value)((s: StepT[X, E, F, A]) => s.fold[F[StepT[X, E, F, B]]](
        cont = k => F.point(StepT.scont(u => flatMapThrough(k(u), f, F)))
        , done = (a, i) =>
          if (i.isEmpty)
            f(a).value
          else
            F.bind(f(a).value)(_.fold(
              cont = kk => kk(i).value
              , done = (aa, _) => F.point(StepT.sdone[X, E, F, B](aa, i))
              , err = ee => F.point(StepT.serr[X, E, F, B](ee))
            ))
        , err = e => F.point(StepT.serr(e))
      )))

  final def flatMap[B](f: A => IterateeT[X, E, F, B])(implicit F: Monad[F]): IterateeT[X, E, F, B] = {
    flatMapThrough(this, f, F)
  }

  final def map[B](f: A => B)(implicit F: Monad[F]): IterateeT[X, E, F, B] = {
    flatMap(a => StepT.sdone[X, E, F, B](f(a), emptyInput).pointI)
  }

  final def contramap[EE](f: EE => E)(implicit F: Monad[F]): IterateeT[X, EE, F, A] = {
    def step(s: StepT[X, E, F, A]): IterateeT[X, EE, F, A] = s.fold[IterateeT[X, EE, F, A]](
      cont = k => cont((in: Input[EE]) => k(in.map(i => f(i))) >>== step),
      done = (a, i) => done(a, if (i.isEof) eofInput else emptyInput),
      err  = ee => err(ee)
    )

    this >>== step
  }

  /**
   * A generalization of >>== that allows a step function which returns its result in a different, "bigger" monad.
   * The monad for G must perform all the effects of F as part of its evaluation; in the trivial case, of course
   * F and G will have the same monad.
   */
  final def advance[EE, AA, G[_]](f: StepT[X, E, F, A] => IterateeT[X, EE, G, AA])(implicit MO: G |>=| F): IterateeT[X, EE, G, AA] = {
    import MO._
    iterateeT(MO.MG.bind(MO.promote(value))(s => f(s).value))
  }

  final def advanceT[EE, AA, G[_]](f: StepT[X, E, F, A] => G[StepT[X, EE, F, AA]])(implicit MO: G |>=| F): G[StepT[X, EE, F, AA]] = {
    import MO._
    MO.MG.bind(MO.promote(value))(s => f(s))
  }

  /**
   * Combine this Iteratee with an Enumerator-like function.
   *
   * Often used in combination with the implicit views such as `enumStream` and `enumIterator`, for example:
   *
   * {{{
   * head[Unit, Int, Id] >>== Stream.continually(1) // enumStream(Stream.continually(1))
   * }}}
   *
   * @param f An Enumerator-like function. If the type parameters `EE` and `BB` are chosen to be
   *          `E` and `B` respectively, the type of `f` is equivalent to `EnumeratorT[X, E, F, A]`.
   */
  final def >>==[EE, AA](f: StepT[X, E, F, A] => IterateeT[X, EE, F, AA])(implicit F: Bind[F]): IterateeT[X, EE, F, AA] =
    iterateeT(F.bind(value)(s => f(s).value))

  final def %=[O](e: EnumerateeT[X, O, E, F])(implicit m: Monad[F]): IterateeT[X, O, F, A] = 
    (this >>== e[A]).joinI[E, A]

  final def &=(e: EnumeratorT[X, E, F])(implicit F: Bind[F]): IterateeT[X, E, F, A] = 
    this >>== e[A] 

  final private def withResultThrough[EE, B](x: IterateeT[X, E, F, A], f: A => IterateeT[X, EE, F, B], F: Monad[F]): IterateeT[X, EE, F, B] =
    iterateeT(
      F.bind(x.value)((s: StepT[X, E, F, A]) => s.fold[F[StepT[X, EE, F, B]]](
        cont = k => sys.error("diverging iteratee")
        , done = (a, _) => f(a).value
        , err = e => F.point(StepT.serr(e))
      )))

  final def withResult[EE, B](e: EnumeratorT[X, E, F])(f: A => IterateeT[X, EE, F, B])(implicit F: Monad[F]): IterateeT[X, EE, F, B] = {
    withResultThrough(this &= e &= enumEofT, f, F)
  }

  final def mapI[G[_]](f: F ~> G)(implicit F: Functor[F]): IterateeT[X, E, G, A] = {
    def step: StepT[X, E, F, A] => StepT[X, E, G, A] =
      _.fold(
        cont = k => scont[X, E, G, A](k andThen loop)
        , done = (a, i) => sdone[X, E, G, A](a, i)
        , err = e => serr[X, E, G, A](e)
      )
    def loop: IterateeT[X, E, F, A] => IterateeT[X, E, G, A] = i => iterateeT(f(F.map(i.value)(step)))
    loop(this)
  }

  final def finish[O](implicit M: Monad[F]): IterateeT[X, O, F, A] = {
    val M0 = IterateeT.IterateeTMonad[X, O, F]
    def check: StepT[X, E, F, A] => IterateeT[X, O, F, A] = _.fold(
      cont = k => k(eofInput) >>== {
        s => s.mapContOr(_ => sys.error("diverging iteratee"), check(s))
      }
      , done = (a, _) => M0.point(a)
      , err = e => err(e)
    )

    iterateeT(M.bind(value) { s => check(s).value })
  }

  final def up[G[_]](implicit G: Pointed[G], F: Functor[F], FC: CoPointed[F]): IterateeT[X, E, G, A] = {
    mapI(new (F ~> G) {
      def apply[A](a: F[A]) = G.point(FC.copoint(a))
    })
  }

  final def joinI[I, B](implicit outer: IterateeT[X, E, F, A] =:= IterateeT[X, E, F, StepT[X, I, F, B]], M: Monad[F]): IterateeT[X, E, F, B] = {
    val M0 = IterateeT.IterateeTMonad[X, E, F]
    outer(this) flatMap { _.pointI.finish[E] }
  }

  /**
   * Feeds input elements to this iteratee until it is done, feeds the produced value to the 
   * inner iteratee.  Then this iteratee will start over, looping until the inner iteratee is done.
   */
  final def sequenceI(implicit m: Monad[F]): EnumerateeT[X, E, A, F] = 
    new EnumerateeT[X, E, A, F] {
      def apply[B] = {
        def loop = doneOr(checkEof)
        def checkEof: (Input[A] => IterateeT[X, A, F, B]) => IterateeT[X, E, F, StepT[X, A, F, B]] = k =>
          isEof[X, E, F] flatMap {
            eof =>
              if (eof) done(scont(k), eofInput)
              else step(k)
          }
        def step: (Input[A] => IterateeT[X, A, F, B]) => IterateeT[X, E, F, StepT[X, A, F, B]] = k =>
          flatMap (a => k(elInput(a)) >>== loop)
        loop
      }
    }

  final def zipStep[Z](i: IterateeT[X, E, F, Z], in: Input[E])(implicit F: Monad[F]) =
    IterateeT.IterateeTMonadTrans[X, E].liftM(i.foldT[(Either[X, Option[(Z, Input[E])]], IterateeT[X, E, F, Z])](
      cont = k => F.point((Right(None), k(in)))
      , done = (a, x) => F.point((Right(Some((a, x))), done(a, x)))
      , err = e => F.point((Left(e), err(e)))
    ))

  final def zipLoop[B](x: IterateeT[X, E, F, A], y: IterateeT[X, E, F, B])(in: Input[E])(implicit F: Monad[F]): IterateeT[X, E, F, (A, B)] = in(
    el = _ =>
      zipStep(x, in) flatMap {
        case (a, xx) =>
          zipStep(y, in) flatMap {
            case (b, yy) =>
              (a, b) match {
                case (Left(e), _)                                => err(e)
                case (_, Left(e))                                => err(e)
                case (Right(Some((a, e))), Right(Some((b, ee)))) => done((a, b), if (e.isEl) e else ee)
                case _                                           => cont(zipLoop(xx, yy))
              }
          }
      }
    , empty = cont(zipLoop(x, y))
    , eof = (x &= enumEofT[X, E, F]) flatMap (a => (y &= enumEofT[X, E, F]) map (b => (a, b)))
  )

  final def zip[B](other: IterateeT[X, E, F, B])(implicit F: Monad[F]): IterateeT[X, E, F, (A, B)] = {
    cont(zipLoop(this, other))
  }

  def liftI[B](implicit F: Monad[F]): IterateeT[X, B, ({type λ[α] = IterateeT[X, E, F, α] })#λ, A] =
    IterateeT.IterateeTMonadTrans[X, B].liftM[({type λ[α] = IterateeT[X, E, F, α]})#λ, A](this)
}

object IterateeT extends IterateeTFunctions with IterateeTInstances {
  def apply[X, E, F[_], A](s: F[StepT[X, E, F, A]]): IterateeT[X, E, F, A] =
    iterateeT(s)
}

trait IterateeTInstances0 {
  implicit def IterateeTMonad[X, E, F[_]](implicit F0: Monad[F]): Monad[({type λ[α] = IterateeT[X, E, F, α]})#λ] = new IterateeTMonad[X, E, F] {
    implicit def F = F0
  }

  implicit def IterateeMonad[X, E]: Monad[({type λ[α] = Iteratee[X, E, α]})#λ] = IterateeTMonad[X, E, Id]

  implicit def IterateeTMonadTransT[X, E, H[_[_], _]](implicit T0: MonadTrans[H]): MonadTrans[({type λ0[α[_], β] = IterateeT[X, E, ({type λ1[x] = H[α, x]})#λ1, β]})#λ0] = new IterateeTMonadTransT[X, E, H] {
    implicit def T = T0
  }
}

trait IterateeTInstances extends IterateeTInstances0 {
  implicit def IterateeTMonadTrans[X, E]: Hoist[({type λ[α[_], β] = IterateeT[X, E, α, β]})#λ] = new IterateeTHoist[X, E] { }

  implicit def IterateeTHoistT[X, E, H[_[_], _]](implicit T0: Hoist[H]): Hoist[({type λ0[α[_], β] = IterateeT[X, E, ({type λ1[x] = H[α, x]})#λ1, β]})#λ0] = new IterateeTHoistT[X, E, H] {
    implicit def T = T0
  }

  implicit def IterateeTMonadIO[X, E, F[_]](implicit M0: MonadIO[F]): MonadIO[({type λ[α] = IterateeT[X, E, F, α]})#λ] =
    new IterateeTMonadIO[X, E, F] {
      implicit def F = M0
      implicit def G = M0
    }

  implicit def IterateeTContravariant[X, F[_]: Monad, A]: Contravariant[({ type λ[α] = IterateeT[X, α, F, A] })#λ] =
    new Contravariant[({ type λ[α] = IterateeT[X, α, F, A] })#λ] {
      def contramap[E, EE](r: IterateeT[X, E, F, A])(f: EE => E) = r.contramap(f)
    }
}

trait IterateeTFunctions {
  def iterateeT[X, E, F[_], A](s: F[StepT[X, E, F, A]]): IterateeT[X, E, F, A] = new IterateeT[X, E, F, A](s)

  def cont[X, E, F[_] : Pointed, A](c: Input[E] => IterateeT[X, E, F, A]): IterateeT[X, E, F, A] =
    StepT.scont(c).pointI

  def done[X, E, F[_] : Pointed, A](d: => A, r: => Input[E]): IterateeT[X, E, F, A] =
    StepT.sdone(d, r).pointI

  def err[X, E, F[_] : Pointed, A](e: => X): IterateeT[X, E, F, A] =
    StepT.serr(e).pointI

  def tryIO[A, F[_], B](io: IO[B])(implicit MIO: MonadIO[F]): IterateeT[Throwable, A, F, B] = 
    IterateeT(MIO.map(MIO.liftIO(io.catchLeft))(_ match {
      case Right(a) => StepT.sdone(a, emptyInput)
      case Left(err) => StepT.serr(err)
    }))
    
  /**
   * An iteratee that writes input to the output stream as it comes in.  Useful for debugging.
   */
  def putStrTo[X, E](os: java.io.OutputStream)(implicit s: Show[E]): IterateeT[X, E, IO, Unit] = {
    def write(e: E) = IO(os.write(s.shows(e).getBytes))
    foldM(())((_: Unit, e: E) => write(e))
  }

  /**
   * An iteratee that consumes all of the input into something that is PlusEmpty and Pointed.
   */
  def consume[X, E, F[_]: Monad, A[_]: PlusEmpty : Pointed]: IterateeT[X, E, F, A[E]] = {
    import scalaz.syntax.plus._
    def step(e: Input[E]): IterateeT[X, E, F, A[E]] = 
      e.fold(empty = cont(step)
        , el = e => cont(step).map(a => Pointed[A].point(e) <+> a)
        , eof = done(PlusEmpty[A].empty, eofInput[E])
      )   

    cont(step)
  }

  def collectT[X, E, F[_], A[_]](implicit M: Monad[F], mae: Monoid[A[E]], pointed: Pointed[A]): IterateeT[X, E, F, A[E]] = {
    import scalaz.syntax.semigroup._
    def step(e: Input[E]): IterateeT[X, E, F, A[E]] = 
      e.fold(empty = cont(step)
        , el = e => cont(step).map(a => Pointed[A].point(e) |+| a)
        , eof = done(Monoid[A[E]].zero, eofInput[E])
      )   

    cont(step)
  }

  /**An iteratee that consumes the head of the input */
  def head[X, E, F[_] : Pointed]: IterateeT[X, E, F, Option[E]] = {
    def step(s: Input[E]): IterateeT[X, E, F, Option[E]] =
      s(empty = cont(step)
        , el = e => done(Some(e), emptyInput[E])
        , eof = done(None, eofInput[E])
      )
    cont(step)
  }

  def headDoneOr[X, E, F[_] : Monad, B](b: => B, f: E => IterateeT[X, E, F, B]): IterateeT[X, E, F, B] = {
    head[X, E, F] flatMap {
      case None => done(b, eofInput)
      case Some(a) => f(a)
    }
  }

  /**An iteratee that returns the first element of the input */
  def peek[X, E, F[_] : Pointed]: IterateeT[X, E, F, Option[E]] = {
    def step(s: Input[E]): IterateeT[X, E, F, Option[E]]
    = s(el = e => done(Some(e), s),
      empty = cont(step),
      eof = done(None, eofInput[E]))
    cont(step)
  }

  def peekDoneOr[X, E, F[_] : Monad, B](b: => B, f: E => IterateeT[X, E, F, B]): IterateeT[X, E, F, B] = {
    peek[X, E, F] flatMap {
      case None => done(b, eofInput)
      case Some(a) => f(a)
    }
  }

  /**An iteratee that skips the first n elements of the input */
  def drop[X, E, F[_] : Pointed](n: Int): IterateeT[X, E, F, Unit] = {
    def step(s: Input[E]): IterateeT[X, E, F, Unit] =
      s(el = _ => drop(n - 1),
        empty = cont(step),
        eof = done((), eofInput[E]))
    if (n == 0) done((), emptyInput[E])
    else cont(step)
  }

  /**
   * An iteratee that skips elements while the predicate evaluates to true.
   */
  def dropWhile[X, E, F[_] : Pointed](p: E => Boolean): IterateeT[X, E, F, Unit] = {
    def step(s: Input[E]): IterateeT[X, E, F, Unit] =
      s(el = e => if (p(e)) dropWhile(p) else done((), s),
        empty = cont(step),
        eof = done((), eofInput[E]))
    cont(step)
  }

  /**
   * An iteratee that skips elements until the predicate evaluates to true.
   */
  def dropUntil[X, E, F[_] : Pointed](p: E => Boolean): IterateeT[X, E, F, Unit] = dropWhile(!p(_))

  /**
   * An iteratee that consumes at most n elements of the input into something that is PlusEmpty and Pointed.
   */
  def take[X, E, F[_]: Monad, A[_]: PlusEmpty : Pointed](n: Int): IterateeT[X, E, F, A[E]] = {
    import scalaz.syntax.plus._
    def step(e: Input[E], n: Int): IterateeT[X, E, F, A[E]] = {
      if (n > 0) {
        e.fold(empty = cont(step(_: Input[E], n))
               , el = e => cont(step(_: Input[E], n - 1)).map(a => Pointed[A].point(e) <+> a)
               , eof = done(PlusEmpty[A].empty, eofInput[E])) 
      } else {
        done(PlusEmpty[A].empty, eofInput[E])
      }
    }

    cont(step(_: Input[E], n))
  }

  def fold[X, E, F[_] : Pointed, A](init: A)(f: (A, E) => A): IterateeT[X, E, F, A] = {
    def step(acc: A): Input[E] => IterateeT[X, E, F, A] = s =>
      s(el = e => cont(step(f(acc, e))),
        empty = cont(step(acc)),
        eof = done(acc, eofInput[E]))
    cont(step(init))
  }

  def foldM[X, E, F[_], A](init: A)(f: (A, E) => F[A])(implicit m: Monad[F]): IterateeT[X, E, F, A] = {
    def step(acc: A): Input[E] => IterateeT[X, E, F, A] = s =>
      s(el = e => IterateeT.IterateeTMonadTrans[X, E].liftM(f(acc, e)) flatMap (a => cont(step(a))),
        empty = cont(step(acc)),
        eof = done(acc, eofInput[E]))
    cont(step(init))
  }

  /**
   * An iteratee that counts and consumes the elements of the input
   */
  def length[X, E, F[_] : Pointed]: IterateeT[X, E, F, Int] = fold(0)((a, _) => a + 1)

  /**
   * An iteratee that checks if the input is EOF.
   */
  def isEof[X, E, F[_] : Pointed]: IterateeT[X, E, F, Boolean] = cont(in => done(in.isEof, in))

  def sum[X, E: Monoid, F[_]: Monad]: IterateeT[X, E, F, E] = 
    foldM[X, E, F, E](Monoid[E].zero)((a, e) => Pointed[F].point(Monoid[E].append(a, e)))
}

//
// Type class implementation traits
//

private[scalaz] trait IterateeTMonad[X, E, F[_]] extends Monad[({type λ[α] = IterateeT[X, E, F, α]})#λ] {
  implicit def F: Monad[F]

  def point[A](a: => A) = StepT.sdone(a, emptyInput).pointI
  override def map[A, B](fa: IterateeT[X, E, F, A])(f: (A) => B): IterateeT[X, E, F, B] = fa map f
  def bind[A, B](fa: IterateeT[X, E, F, A])(f: A => IterateeT[X, E, F, B]): IterateeT[X, E, F, B] = fa flatMap f
}

private[scalaz] trait IterateeTHoist[X, E] extends Hoist[({type λ[β[_], α] = IterateeT[X, E, β, α]})#λ] {
  trait IterateeTF[F[_]] {
    type λ[α] = IterateeT[X, E, F, α]
  }

  def hoist[F[_]: Monad, G[_]](f: F ~> G) = new (IterateeTF[F]#λ ~> IterateeTF[G]#λ) {
    def apply[A](fa: IterateeT[X, E, F, A]): IterateeT[X, E, G, A] = fa mapI f
  }

  def liftM[G[_] : Monad, A](ga: G[A]): IterateeT[X, E, G, A] =
    iterateeT(Monad[G].map(ga)(sdone[X, E, G, A](_, emptyInput)))

  implicit def apply[G[_] : Monad]: Monad[IterateeTF[G]#λ] = IterateeT.IterateeTMonad[X, E, G]
}

private[scalaz] trait IterateeTMonadIO[X, E, F[_]] extends MonadIO[({type λ[α] = IterateeT[X, E, F, α]})#λ] with IterateeTMonad[X, E, F] {
  implicit def F: MonadIO[F]
  
  def liftIO[A](ioa: IO[A]) = MonadTrans[({type λ[α[_], β] = IterateeT[X, E, α, β]})#λ].liftM(F.liftIO(ioa))
}

private[scalaz] trait IterateeTMonadTransT[X, E, H[_[_], _]] extends MonadTrans[({type λ0[α[_], β] = IterateeT[X, E, ({type λ1[x] = H[α, x]})#λ1, β]})#λ0] {
  implicit def T: MonadTrans[H]

  def liftM[G[_]: Monad, A](ga: G[A]): IterateeT[X, E, ({type λ[α] = H[G, α]})#λ, A] = 
    IterateeT.IterateeTMonadTrans[X, E].liftM[({type λ[α] = H[G, α]})#λ, A](T.liftM(ga))(T[G])

  def apply[G[_]: Monad]: Monad[({type λ0[α0] = IterateeT[X, E, ({type λ1[α1] = H[G, α1]})#λ1, α0]})#λ0] = 
    IterateeT.IterateeTMonad[X, E, ({type λ[α] = H[G, α]})#λ](T[G])
}

private[scalaz] trait IterateeTHoistT[X, E, H[_[_], _]] extends Hoist[({type λ0[α[_], β] = IterateeT[X, E, ({type λ1[x] = H[α, x]})#λ1, β]})#λ0] with IterateeTMonadTransT[X, E, H] {
  implicit def T: Hoist[H]

  def hoist[M[_]: Monad, N[_]](f: M ~> N) = new (({type λ0[α0] = IterateeT[X, E, ({type λ1[α1] = H[M, α1]})#λ1, α0]})#λ0 ~> ({type λ0[α0] = IterateeT[X, E, ({type λ1[α1] = H[N, α1]})#λ1, α0]})#λ0) {
    def apply[A](fa: IterateeT[X, E, ({type λ[α] = H[M, α]})#λ, A]): IterateeT[X, E, ({type λ[α] = H[N, α]})#λ, A] = 
      fa.mapI[({type λ[α] = H[N, α]})#λ](T.hoist[M, N](f))(T[M])
  }
}
