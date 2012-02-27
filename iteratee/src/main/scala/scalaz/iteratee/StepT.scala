package scalaz
package iteratee

import Iteratee._

/**
 * The current state of an Iteratee, one of:
 *  - '''cont''' Waiting for more data
 *  - '''done''' Already calculated a result
 *  - '''err''' Error, unable to calculate a result
 *
 * @tparam X The type of the error (mnemonic: e'''X'''ception type)
 * @tparam E The type of the input data (mnemonic: '''E'''lement type)
 * @tparam F The type constructor representing an effect.
 *           The type constructor [[scalaz.Id]] is used to model pure computations, and is fixed as such in the type alias [[scalaz.Step]].
 * @tparam A The type of the calculated result
 */
sealed abstract class StepT[X, E, F[_], A] private () {
  @inline final def fold[Z](
    cont: (Input[E] => IterateeT[X, E, F, A]) => Z
    , done: (=> A, => Input[E]) => Z
    , err: (=> X) => Z
  ): Z = this match {
    // TODO: This is ugly because of SI-5359. Fix when we move to 2.10
    case c: StepT.Cont[X, E, F, A] => cont(c.contf)
    case d: StepT.Done[X, E, F, A] => done(d.a, d.r)
    case e: StepT.Err[X, E, F, A]  => err(e.x)
  }

  /** An alias for `fold` */
  @inline final def apply[Z](
                cont: (Input[E] => IterateeT[X, E, F, A]) => Z
                , done: (=> A, => Input[E]) => Z
                , err: (=> X) => Z
                ): Z = fold(cont, done, err)

  def cont: Option[Input[E] => IterateeT[X, E, F, A]]

  def contOr(k: => Input[E] => IterateeT[X, E, F, A]): Input[E] => IterateeT[X, E, F, A]

  def mapContOr[Z](k: (Input[E] => IterateeT[X, E, F, A]) => Z, z: => Z): Z

  @inline final def mapCont(k: (Input[E] => IterateeT[X, E, F, A]) => IterateeT[X, E, F, A])(implicit F: Pointed[F]): IterateeT[X, E, F, A] =
    mapContOr[IterateeT[X, E, F, A]](k, pointI)

  def doneValue: LazyOption[A]

  def doneValueOr(a: => A): A

  def mapDoneValueOr[Z](k: (=> A) => Z, z: => Z): Z

  def doneInput: LazyOption[Input[E]]

  def doneInputOr(a: => Input[E]): Input[E]

  def mapDoneInputOr[Z](k: (=> Input[E]) => Z, z: => Z): Z

  def err: LazyOption[X]

  def errOr(x: => X): X

  def mapErrOr[Z](k: (=> X) => Z, z: => Z): Z

  def >-[Z](cont: => Z, done: => Z, err: => Z): Z

  @inline final def pointI(implicit P: Pointed[F]): IterateeT[X, E, F, A] =
    iterateeT(P.point(this))
}

// object StepT is in the implicit scope for EnumeratorT, so we mix in EnumeratorTInstances here.
object StepT extends StepTFunctions with EnumeratorTInstances {
  private[this] val ToNone: ((=> Any) => None.type) = x => None
  private[this] val ToNone1: (Any => None.type) = x => None
  private[this] val ToNone2: ((=> Any, => Any) => None.type) = (x, y) => None

  private[iteratee] case class Cont[X, E, F[_], A] (private[iteratee] val contf: Input[E] => IterateeT[X, E, F, A]) extends StepT[X, E, F, A] {
    @inline final def cont = Some(contf)

    @inline final def contOr(k: => Input[E] => IterateeT[X, E, F, A]) = contf

    @inline final def mapContOr[Z](k: (Input[E] => IterateeT[X, E, F, A]) => Z, z: => Z): Z = k(contf)

    @inline final def doneValue = LazyOption.lazyNone[A]

    @inline final def doneValueOr(a: => A): A = a

    @inline final def mapDoneValueOr[Z](k: (=> A) => Z, z: => Z): Z = z

    @inline final def doneInput: LazyOption[Input[E]] = LazyOption.lazyNone[Input[E]]

    @inline final def doneInputOr(a: => Input[E]): Input[E] = a

    @inline final def mapDoneInputOr[Z](k: (=> Input[E]) => Z, z: => Z): Z = z

    @inline final def err: LazyOption[X] = LazyOption.lazyNone[X]

    @inline final def errOr(x: => X): X = x

    @inline final def mapErrOr[Z](k: (=> X) => Z, z: => Z): Z = z

    @inline final def >-[Z](cont: => Z, done: => Z, err: => Z): Z = cont
  }

  private[iteratee] case class Done[X, E, F[_], A] (private[iteratee] val a: A, private[iteratee] val r: Input[E]) extends StepT[X, E, F, A] {
    @inline final def cont = None

    @inline final def contOr(k: => Input[E] => IterateeT[X, E, F, A]) = k

    @inline final def mapContOr[Z](k: (Input[E] => IterateeT[X, E, F, A]) => Z, z: => Z): Z = z

    @inline final def doneValue = LazyOption.lazySome(a)

    @inline final def doneValueOr(a: => A): A = a

    @inline final def mapDoneValueOr[Z](k: (=> A) => Z, z: => Z): Z = k(a)

    @inline final def doneInput: LazyOption[Input[E]] = LazyOption.lazySome(r)

    @inline final def doneInputOr(a: => Input[E]): Input[E] = r

    @inline final def mapDoneInputOr[Z](k: (=> Input[E]) => Z, z: => Z): Z = k(r)

    @inline final def err: LazyOption[X] = LazyOption.lazyNone[X]

    @inline final def errOr(x: => X): X = x

    @inline final def mapErrOr[Z](k: (=> X) => Z, z: => Z): Z = z

    @inline final def >-[Z](cont: => Z, done: => Z, err: => Z): Z = done
  }

  private[iteratee] case class Err[X, E, F[_], A] (private[iteratee] val x: X) extends StepT[X, E, F, A] {
    @inline final def cont = None

    @inline final def contOr(k: => Input[E] => IterateeT[X, E, F, A]) = k

    @inline final def mapContOr[Z](k: (Input[E] => IterateeT[X, E, F, A]) => Z, z: => Z): Z = z

    @inline final def doneValue = LazyOption.lazyNone[A]

    @inline final def doneValueOr(a: => A): A = a

    @inline final def mapDoneValueOr[Z](k: (=> A) => Z, z: => Z): Z = z

    @inline final def doneInput: LazyOption[Input[E]] = LazyOption.lazyNone[Input[E]]

    @inline final def doneInputOr(a: => Input[E]): Input[E] = a

    @inline final def mapDoneInputOr[Z](k: (=> Input[E]) => Z, z: => Z): Z = z

    @inline final def err: LazyOption[X] = LazyOption.lazySome(x)

    @inline final def errOr(ox: => X): X = x

    @inline final def mapErrOr[Z](k: (=> X) => Z, z: => Z): Z = k(x)

    @inline final def >-[Z](cont: => Z, done: => Z, err: => Z): Z = err
  }
}

trait StepTFunctions {
  @inline final def scont[X, E, F[_], A](c: Input[E] => IterateeT[X, E, F, A]): StepT[X, E, F, A] = StepT.Cont(c)
  @inline final def sdone[X, E, F[_], A](d: => A, r: => Input[E]): StepT[X, E, F, A] = StepT.Done(d, r)
  @inline final def serr[X, E, F[_], A](e: => X): StepT[X, E, F, A] = StepT.Err(e)
}
