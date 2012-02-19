package scalaz
package iteratee

import effect._
import Iteratee._
import Ordering.{EQ, GT, LT}

import scala.collection.immutable.Vector

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

  def cogroupI[X, J, K, F[_]](implicit M: Monad[F], order: (J, K) => Ordering, orderJ : Order[J], orderK: Order[K]): Enumeratee2T[X, Vector[J], Vector[K], Vector[Either3[J, (J, K), K]], F] =
    new Enumeratee2T[X, Vector[J], Vector[K], Vector[Either3[J, (J, K), K]], F] {
      type Buffer = (Vector[J],Vector[K])
      type Result = Either3[J,(J,K),K]

      def apply[A] = {
        /* This method is responsible for grouping two chunks up to the point where either is exhausted.
         * If sides are exhausted but no more input is available it will completely use the chunks. */
        def innerGroup(lBuffer: Vector[J], finalLeft: Boolean, rBuffer: Vector[K], finalRight: Boolean, acc: Vector[Result]): (Vector[Result], Option[Buffer]) = {
          // Pull identical elements off of each side pre-compare
          def identityPartition[E](elements: Vector[E]): (Vector[E],Vector[E]) = 
            elements.headOption.map(h => elements.splitAt(elements.lastIndexOf(h) + 1)).getOrElse((Vector(),Vector()))

          val (leftHeads, leftRest) = identityPartition(lBuffer)
          val (rightHeads, rightRest) = identityPartition(rBuffer)

          // If we have empty "rest" on either side and this isn't the final group (e.g. input available on that side), we've hit a chunk boundary and need more data
          if ((leftRest.isEmpty && ! finalLeft) || (rightRest.isEmpty && ! finalRight)) {
            (acc, Some((lBuffer, rBuffer)))
          } else {
            (leftHeads.headOption, rightHeads.headOption) match {
              // If leftHeads and rightHeads are equal, we need a cross-product
              case (Some(l), Some(r)) if order(l,r) == EQ => innerGroup(leftRest, finalLeft, rightRest, finalRight, acc ++ leftHeads.flatMap(l => rightHeads.map(r => Middle3((l,r)))))

              // If leftHeads is less, accumulate it and recurse on the left remainder and full right side
              case (Some(l), Some(r)) if order(l,r) == LT => innerGroup(leftRest, finalLeft, rBuffer, finalRight, acc ++ leftHeads.map(Left3(_)))

              // And vice-versa for rightHeads
              case (Some(l), Some(r))  => innerGroup(lBuffer, finalLeft, rightRest, finalRight, acc ++ rightHeads.map(Right3(_)))

              // When we've expired one side we simply map the whole side. This should only be reached when finalLeft or finalRight are true for both sides
              case (_, None) => (acc ++ lBuffer.map(Left3(_)), None)
              case (None, _) => (acc ++ rBuffer.map(Right3(_)), None)
            }
          }
        }

        def outerGroup(left: Option[Vector[J]], right: Option[Vector[K]], b: Buffer): Option[(Vector[Result], Option[Buffer])] = {
          val (lBuffer, rBuffer) = b

          (left, right) match {
            case (Some(leftChunk), Some(rightChunk)) => Some(innerGroup(lBuffer ++ leftChunk, false, rBuffer ++ rightChunk, false, Vector()))
            case (Some(leftChunk), None)             => Some(innerGroup(lBuffer ++ leftChunk, false, rBuffer, true, Vector()))
            case (None, Some(rightChunk))            => Some(innerGroup(lBuffer, true, rBuffer ++ rightChunk, false, Vector()))
            // Clean up remaining buffers when we run out of input
            case (None, None) if ! (lBuffer.isEmpty && rBuffer.isEmpty) => Some(innerGroup(lBuffer, true, rBuffer, true, Vector()))
            case (None, None)                        => None
          }
        }

        def step(s: StepM[A], buffers: Buffer): IterateeT[X, Vector[J], IterateeM, StepM[A]] = {
          s.fold[IterateeT[X, Vector[J], IterateeM, StepM[A]]](
            cont = contf => {
              def matchOuter: PartialFunction[Option[(Vector[Result], Option[Buffer])], IterateeT[X, Vector[J], IterateeM, StepM[A]]] = {
                case Some((newChunk,newBuffers)) => {
                  iterateeT[X, Vector[J], IterateeM, StepM[A]](contf(elInput(newChunk)) >>== (step(_, newBuffers.getOrElse((Vector(), Vector()))).value))
                }
                case None => done[X, Vector[J], IterateeM, StepM[A]](s, eofInput)
              }

              def readBoth = 
                for {
                  leftOpt  <- head[X, Vector[J], IterateeM]
                  rightOpt <- lift[X, Vector[J], Vector[K], F, Option[Vector[K]]](head[X, Vector[K], F])
                  a <- matchOuter(outerGroup(leftOpt, rightOpt, buffers))
                } yield a

              def readLeft = 
                for {
                  leftOpt  <- head[X, Vector[J], IterateeM]
                  a <- matchOuter(outerGroup(leftOpt, Some(Vector()), buffers)) // Need to pass an empty buffer on right since this isn't terminal
                } yield a

              def readRight = 
                for {
                  rightOpt <- lift[X, Vector[J], Vector[K], F, Option[Vector[K]]](head[X, Vector[K], F])
                  a <- matchOuter(outerGroup(Some(Vector()), rightOpt, buffers)) // Need to pass en empty buffer on the left since it isn't terminal
                } yield a

              /* Conditionally read from both sides as needed:
               * 1. No buffer on either side means two reads
               * 2. Buffer on one side means read the other side, and read the existing side if the buffer's first element is the same as its last element
               * 3. Buffer on both sides means read the side that has 1st element == last element */
              buffers match {
                // No buffer on either side means we need a fresh read on both sides
                case (Vector(), Vector()) => readBoth
                // Buffer only on one side means a definite read on the other, and conditional on the existing if b.head == b.last
                case (Vector(), right) if orderK.equal(right.head, right.last) => readBoth
                case (Vector(), right) => readLeft
                case (left, Vector())  if orderJ.equal(left.head, left.last)   => readBoth
                case (left, Vector()) => readRight
                // Buffer on both sides means conditional on each side having an equivalent series
                case (left, right) if orderJ.equal(left.head, left.last) && orderK.equal(right.head, right.last) => readBoth
                case (left, _) if orderJ.equal(left.head, left.last) => readLeft
                case (_, right) if orderK.equal(right.head, right.last) => readRight
              }
            },
            done = (a, r) => done[X, Vector[J], IterateeM, StepM[A]](sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
            err  = x => err[X, Vector[J], IterateeM, StepM[A]](x)
          )
        }

        step(_, (Vector(),Vector()))
      }
    }

  def joinI[X, J, K, F[_]](implicit M: Monad[F], ord: (J, K) => Ordering, orderJ: Order[J], orderK: Order[K]): Enumeratee2T[X, Vector[J], Vector[K], Vector[(J, K)], F] = {
    new Enumeratee2T[X, Vector[J], Vector[K], Vector[(J, K)], F] {
      def apply[A] = {
        def cstep(step: StepT[X, Vector[(J, K)], F, A]): StepT[X, Vector[Either3[J, (J, K), K]], F, StepT[X, Vector[(J, K)], F, A]] = step.fold(
          cont = contf => scont { in: Input[Vector[Either3[J, (J, K), K]]] =>
            val nextInput = in.map(els => els.flatMap(_.middleOr(Option.empty[(J,K)])(Some(_))))

            contf(nextInput) >>== (s => cstep(s).pointI)
          },
          done = (a, r) => sdone(sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
          err  = x => serr(x)
        )

        (step: StepT[X, Vector[(J, K)], F, A]) => cogroupI.apply(cstep(step)) flatMap { endStep[X, J, K, Vector[(J, K)], F, A] }
      }
    }
  }

  def mergeI[X, B, F[_]](implicit M: Monad[F], ord: Order[B]): Enumeratee2T[X, Vector[B], Vector[B], Vector[B], F] = {
    type E = Vector[B]
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

                innerMerge(l, r, Vector.empty[B])
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
  }

  def parFoldI[X, J, K, F[_]](f: K => J)(implicit order: (J, K) => Ordering, orderJ: Order[J], orderK: Order[K], m: Monoid[J], M: Monad[F]): Enumeratee2T[X, Vector[J], Vector[K], Vector[J], F] =
    new Enumeratee2T[X, Vector[J], Vector[K], Vector[J], F] {
      def apply[A] = {
        def cstep(step: StepT[X, Vector[J], F, A]): StepT[X, Vector[Either3[J, (J, K), K]], F, StepT[X, Vector[J], F, A]]  = step.fold(
          cont = contf => scont { in: Input[Vector[Either3[J, (J, K), K]]] =>
            val nextInput = in map { _.map {
              case Left3(j) => j
              case Middle3((j, k)) => m.append(j, f(k))
              case Right3(k) => m.zero
            }}

            contf(nextInput) >>== (s => cstep(s).pointI)
          },
          done = (a, r) => sdone(sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
          err  = x => serr(x)
        )

        (step: StepT[X, Vector[J], F, A]) => cogroupI.apply(cstep(step)) flatMap { endStep[X, J, K, Vector[J], F, A] }
      }
    }

  private def endStep[X, J, K, EE, F[_]: Monad, A](sa: StepT[X, Vector[Either3[J, (J, K), K]], F, StepT[X, EE, F, A]]) = {
    IterateeT.IterateeTMonadTransT[X, Vector[J], ({ type λ[β[_], α] = IterateeT[X, Vector[K], β, α] })#λ].liftM(sa.pointI.run(x => err[X, EE, F, A](x).value))
  }
}

object Enumeratee2T extends Enumeratee2TFunctions

// vim: set ts=4 sw=4 et:
