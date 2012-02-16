package scalaz
package iteratee

import effect._
import Iteratee._
import Ordering.{EQ, GT, LT}

trait Enumeratee2T[X, J, K, I, F[_]] {
  type IterateeM[α] = IterateeT[X, K, F, α]
  type StepM[α] = StepT[X, I, F, α]
  type InputI = Input[I]

  def apply[A]: StepM[A] => IterateeT[X, J, IterateeM, StepM[A]]
}

trait Enumeratee2TFunctions {
  import scalaz.syntax.Syntax.bind._
  import scalaz.syntax.Syntax.order._

  @inline private def lift[X, J, K, F[_]: Monad, A](iter: IterateeT[X, K, F, A]): IterateeT[X, J, ({type λ[α] = IterateeT[X, K, F, α] })#λ, A] =
    IterateeT.IterateeTMonadTrans[X, J].liftM[({type λ[α] = IterateeT[X, K, F, α]})#λ, A](iter)

  def cogroupI[X, J, K, F[_]](implicit M: Monad[F], order: (J, K) => Ordering): Enumeratee2T[X, J, K, Either3[J, (J, K), K], F] =
    new Enumeratee2T[X, J, K, Either3[J, (J, K), K], F] {
      def apply[A] = {
        // Used to 'replay' values from the right for when values from the left order equal
        // in the pathological case, this degrades to the cartesian product, which will blow up
        // your memory. Sorry!
        def advance(j: J, buf: List[K], s: StepT[X, Either3[J, (J, K), K], F, A]): IterateeT[X,Either3[J,(J, K),K],F,A] = {
          s mapCont { contf =>
            buf match {
              case k :: Nil if order(j, k) == EQ => contf(elInput(Middle3((j, k))))
              case k :: ks  if order(j, k) == EQ => contf(elInput(Middle3((j, k)))) >>== (advance(j, ks, _))
              case _ => contf(elInput(Left3(j)))
            }
          }
        }

        def step(s: StepM[A], rbuf: List[K]): IterateeT[X, J, IterateeM, StepM[A]] = {
          s.fold[IterateeT[X, J, IterateeM, StepM[A]]](
            cont = contf => {
              for {
                leftOpt  <- peek[X, J, IterateeM]
                rightOpt <- lift[X, J, K, F, Option[K]](peek[X, K, F])
                a <- (leftOpt, rightOpt) match {
                  case (left, Some(right)) if left.forall(order(_, right) == GT) =>
                    for {
                      _ <- lift[X, J, K, F, Option[K]](head[X, K, F])
                      a <- iterateeT[X, J, IterateeM, StepM[A]](contf(elInput(Right3(right))) >>== (step(_, Nil).value))
                    } yield a

                  case (Some(left), right) if right.forall(order(left, _) == LT) =>
                    for {
                      _ <- head[X, J, IterateeM]
                      a <- iterateeT[X, J, IterateeM, StepM[A]](advance(left, rbuf, scont(contf)) >>== (step(_, rbuf).value))
                    } yield a

                  case (Some(left), Some(right)) =>
                    for {
                      _ <- lift[X, J, K, F, Option[K]](head[X, K, F])
                      a <- step(s, if (rbuf.headOption.exists(order(left, _) == EQ)) right :: rbuf else right :: Nil)
                    } yield a

                  case _ => done[X, J, IterateeM, StepM[A]](s, eofInput)
                }
              } yield a
            },
            done = (a, r) => done[X, J, IterateeM, StepM[A]](sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
            err  = x => err(x)
          )
        }

        step(_, Nil)
      }
    }

  def joinI[X, J, K, F[_]](implicit M: Monad[F], ord: (J, K) => Ordering): Enumeratee2T[X, J, K, (J, K), F] =
    new Enumeratee2T[X, J, K, (J, K), F] {
      def apply[A] = {
        def cstep(step: StepT[X, (J, K), F, A]): StepT[X, Either3[J, (J, K), K], F, StepT[X, (J, K), F, A]] = step.fold(
          cont = contf => scont { in: Input[Either3[J, (J, K), K]] =>
            val nextInput = in.flatMap(_.middleOr(emptyInput[(J, K)]) { elInput(_) })

            contf(nextInput) >>== (s => cstep(s).pointI)
          },
          done = (a, r) => sdone(sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
          err  = x => serr(x)
        )

        (step: StepT[X, (J, K), F, A]) => cogroupI.apply(cstep(step)) flatMap { endStep[X, J, K, (J, K), F, A] }
      }
    }

  def mergeI[X, B: Order, E <: List[B], F[_]: Monad]: Enumeratee2T[X, E, E, E, F] = 
    new Enumeratee2T[X, E, E, E, F] {
      type Surplus = Either[E,E]


      def apply[A] = {
        def step(s: StepM[A], previousSurplus: Option[Surplus]): IterateeT[X, E, IterateeM, StepM[A]] = {
          def mergeChunks(contf: Input[E] => IterateeT[X,E,F,A], left: Option[E], right: Option[E]): IterateeT[X, E, IterateeM, StepM[A]] = {
            val (merged, surplus) = (left, right) match {
              case (Some(l), Some(r)) => {
                def innerMerge(l: E, r: E, acc: E): (Option[E], Option[Surplus]) = (l.headOption, r.headOption) match {
                  case (Some(x), Some(y)) if x < y => innerMerge(l.tail, r, acc :+ x)
                  case (Some(x), Some(y))          => innerMerge(l, r.tail, acc :+ y)
                  case (None, None)                => (Some(acc), None) // Technically this should only happen if the initial chunks are both Nil
                  case (None, _)                   => (Some(acc), Some(Right(r)))
                  case (_, None)                   => (Some(acc), Some(Left(l)))
                }

                innerMerge(l, r, List.empty[B])
              }
              case (None, None) => (None, None)
              case (None, r) => (r, None)
              case (l, None) => (l, None)
            }

            iterateeT[X, E, IterateeM, StepM[A]](contf(merged.map(elInput(_)).getOrElse(eofInput)) >>== (step(_, surplus).value))
          }

          s.fold[IterateeT[X, E, IterateeM, StepM[A]]](
            cont = contf => {
              previousSurplus match {
                case Some(Left(left)) => {
                  for {
                    rightOpt <- lift[X, E, E, F, Option[E]](head[X, E, F])
                    a        <- mergeChunks(contf, Some(left), rightOpt)
                  } yield a
                }
                case Some(Right(right)) => {
                  for {
                    leftOpt  <- head[X, E, IterateeM]
                    a        <- mergeChunks(contf, leftOpt, Some(right))
                  } yield a
                }
                case None => {
                  for {
                    leftOpt  <- head[X, E, IterateeM]
                    rightOpt <- lift[X, E, E, F, Option[E]](head[X, E, F])
                    a        <- mergeChunks(contf, leftOpt, rightOpt)
                  } yield a
                }
              }
            },
            done = (a, r) => done[X, E, IterateeM, StepM[A]](sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
            err  = x => err(x)
          )
        }

        step(_, None)
      }
    }

  def parFoldI[X, J, K, F[_]](f: K => J)(implicit order: (J, K) => Ordering, m: Monoid[J], M: Monad[F]): Enumeratee2T[X, J, K, J, F] =
    new Enumeratee2T[X, J, K, J, F] {
      def apply[A] = {
        def cstep(step: StepT[X, J, F, A]): StepT[X, Either3[J, (J, K), K], F, StepT[X, J, F, A]]  = step.fold(
          cont = contf => scont { in: Input[Either3[J, (J, K), K]] =>
            val nextInput = in map {
              case Left3(j) => j
              case Middle3((j, k)) => m.append(j, f(k))
              case Right3(k) => m.zero
            }

            contf(nextInput) >>== (s => cstep(s).pointI)
          },
          done = (a, r) => sdone(sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
          err  = x => serr(x)
        )

        (step: StepT[X, J, F, A]) => cogroupI.apply(cstep(step)) flatMap { endStep[X, J, K, J, F, A] }
      }
    }

  private def endStep[X, J, K, EE, F[_]: Monad, A](sa: StepT[X, Either3[J, (J, K), K], F, StepT[X, EE, F, A]]) = {
    IterateeT.IterateeTMonadTransT[X, J, ({ type λ[β[_], α] = IterateeT[X, K, β, α] })#λ].liftM(sa.pointI.run(x => err[X, EE, F, A](x).value))
  }
}

object Enumeratee2T extends Enumeratee2TFunctions

// vim: set ts=4 sw=4 et:
