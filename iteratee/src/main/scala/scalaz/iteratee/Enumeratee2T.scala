package scalaz
package iteratee

import effect._
import Iteratee._
import Ordering.{EQ, GT, LT}

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer

trait Enumeratee2T[X, J, K, I, F[_]] {
  type IterateeM[α] = IterateeT[X, K, F, α]
  type StepM[α] = StepT[X, I, F, α]
  type InputI = Input[I]

  def apply[A]: StepM[A] => IterateeT[X, J, IterateeM, StepM[A]]
}

trait Enumeratee2TChunkedFunctions {
  import scalaz.syntax.Syntax.bind._
  import scalaz.syntax.Syntax.order._


  def cogroupIChunked[X, J, K, F[_]](implicit M: Monad[F], order: (J, K) => Ordering, jOrder : Order[J], kOrder: Order[K]): Enumeratee2T[X, Vector[J], Vector[K], Vector[Either3[J, (J, K), K]], F] =
    new Enumeratee2T[X, Vector[J], Vector[K], Vector[Either3[J, (J, K), K]], F] {
      type Buffer = (Vector[J],Vector[K])
      type Result = Either3[J,(J,K),K]
      type ContFunc[A] = Input[Vector[Result]] => IterateeT[X, Vector[Result], F, A]

      /* This method is responsible for grouping two chunks up to the point where either is exhausted.
       * If sides are exhausted but no more input is available it will completely use the chunks. */
      def innerGroup(lBuffer: Vector[J], finalLeft: Boolean, rBuffer: Vector[K], finalRight: Boolean, acc: ArrayBuffer[Result], orderJ: Order[J], orderK: Order[K]): (ArrayBuffer[Result], Option[Buffer]) = {
        //TODO: Use iterators instead of indexed access into the vector
        // Determine where the lead sequence of identical elements ends
        def identityPartitionIndex[E](elements: Vector[E], order: Order[E]): Int =
          if (elements.length == 0) {
            0
          } else {
            var i = 1
            while (i < elements.length && order(elements(0), elements(i)) == EQ) {
              i += 1
            }
            i
          }

        val leftRestIndex = identityPartitionIndex(lBuffer, orderJ)
        val rightRestIndex = identityPartitionIndex(rBuffer, orderK)

        // If we have empty "rest" on either side and this isn't the final group (e.g. input available on that side), we've hit a chunk boundary and need more data
        if ((leftRestIndex == lBuffer.length && ! finalLeft) || (rightRestIndex == rBuffer.length && ! finalRight)) {
          (acc, Some((lBuffer, rBuffer)))
        } else if (lBuffer.length == 0) {
          // We've expired the left side, so we just map the whole right side. This should only be reached when finalLeft or finalRight are true for both sides
          (acc ++ rBuffer.map(Right3(_)), None)
        } else if (rBuffer.length == 0) {
          // We've expired the right side, so we just map the whole left side. This should only be reached when finalLeft or finalRight are true for both sides
          (acc ++ lBuffer.map(Left3(_)), None)
        } else {
          // At this point we compare heads to determine how to group the leading identical sequences from both sides
          order(lBuffer(0), rBuffer(0)) match {
            case EQ => {
              // When equal, we need to generate the cartesian product of both sides
              val buffer = new Array[Result](leftRestIndex * rightRestIndex)
              var i = 0
              while (i < leftRestIndex) {
                var j = 0
                while (j < rightRestIndex) { 
                  buffer(i * rightRestIndex + j) = Middle3(lBuffer(i), rBuffer(j))
                  j += 1
                }
                i += 1
              }
              innerGroup(lBuffer.drop(leftRestIndex), finalLeft, rBuffer.drop(rightRestIndex), finalRight, acc ++ buffer, orderJ, orderK)
            }
            case LT => {
              // Accumulate the left side first and recurse on the remainder of the left side and the full right side
              val buffer = new Array[Result](leftRestIndex)
              var i = 0
              while (i < leftRestIndex) { buffer(i) = Left3(lBuffer(i)); i += 1 }

              innerGroup(lBuffer.drop(leftRestIndex), finalLeft, rBuffer, finalRight, acc ++ buffer, orderJ, orderK)
            }
            case GT => {
              // Accumulate the right side first and recurse on the remainder of the right side and the full left side
              val buffer = new Array[Result](rightRestIndex)
              var i = 0
              while (i < rightRestIndex) { buffer(i) = Right3(rBuffer(i)); i += 1 }

              innerGroup(lBuffer, finalLeft, rBuffer.drop(rightRestIndex), finalRight, acc ++ buffer, orderJ, orderK)
            }
          }
        }
      }

      def outerGroup(left: Option[Vector[J]], right: Option[Vector[K]], b: Buffer, orderJ: Order[J], orderK: Order[K]): Option[(ArrayBuffer[Result], Option[Buffer])] = {
        val (lBuffer, rBuffer) = b

        (left, right) match {
          case (Some(leftChunk), Some(rightChunk)) => Some(innerGroup(lBuffer ++ leftChunk, false, rBuffer ++ rightChunk, false, ArrayBuffer.empty[Result], orderJ, orderK))
          case (Some(leftChunk), None)             => Some(innerGroup(lBuffer ++ leftChunk, false, rBuffer, true, ArrayBuffer.empty[Result], orderJ, orderK))
          case (None, Some(rightChunk))            => Some(innerGroup(lBuffer, true, rBuffer ++ rightChunk, false, ArrayBuffer.empty[Result], orderJ, orderK))
          // Clean up remaining buffers when we run out of input
          case (None, None) if ! (lBuffer.isEmpty && rBuffer.isEmpty) => Some(innerGroup(lBuffer, true, rBuffer, true, ArrayBuffer.empty[Result], orderJ, orderK))
          case (None, None)                        => None
        }
      }

      def readBoth[A](s: StepM[A], contf: ContFunc[A], buffers: Buffer, orderJ: Order[J], orderK: Order[K]): IterateeT[X, Vector[J], IterateeM, StepM[A]] = 
        for {
          leftOpt  <- head[X, Vector[J], IterateeM]
          rightOpt <- head[X, Vector[K], F].liftI[Vector[J]]
          a <- matchOuter[A](s, contf, orderJ, orderK)(outerGroup(leftOpt, rightOpt, buffers, orderJ, orderK))
        } yield a

      def readLeft[A](s: StepM[A], contf: ContFunc[A], buffers: Buffer, orderJ: Order[J], orderK: Order[K]): IterateeT[X, Vector[J], IterateeM, StepM[A]] = 
        for {
          leftOpt  <- head[X, Vector[J], IterateeM]
          a <- matchOuter[A](s, contf, orderJ, orderK)(outerGroup(leftOpt, Some(Vector()), buffers, orderJ, orderK)) // Need to pass an empty buffer on right since this isn't terminal
        } yield a

      def readRight[A](s: StepM[A], contf: ContFunc[A], buffers: Buffer, orderJ: Order[J], orderK: Order[K]): IterateeT[X, Vector[J], IterateeM, StepM[A]] = 
        for {
          rightOpt <- head[X, Vector[K], F].liftI[Vector[J]]
          a <- matchOuter[A](s, contf, orderJ, orderK)(outerGroup(Some(Vector()), rightOpt, buffers, orderJ, orderK)) // Need to pass en empty buffer on the left since it isn't terminal
        } yield a

      def matchOuter[A](s: StepM[A], contf: ContFunc[A], orderJ: Order[J], orderK: Order[K]): PartialFunction[Option[(ArrayBuffer[Result], Option[Buffer])], IterateeT[X, Vector[J], IterateeM, StepM[A]]] = {
        case Some((newChunk,newBuffers)) => {
          iterateeT[X, Vector[J], IterateeM, StepM[A]](contf(elInput(Vector(newChunk: _*))) >>== (step(_, newBuffers.getOrElse((Vector(), Vector())), orderJ, orderK).value))
        }
        case None => done[X, Vector[J], IterateeM, StepM[A]](s, eofInput)
      }

      def step[A](s: StepM[A], buffers: Buffer, orderJ: Order[J], orderK: Order[K]): IterateeT[X, Vector[J], IterateeM, StepM[A]] = {
        s.fold[IterateeT[X, Vector[J], IterateeM, StepM[A]]](
          cont = contf => {
            /* Conditionally read from both sides as needed:
             * 1. No buffer on either side means two reads
             * 2. Buffer on one side means read the other side, and read the existing side if the buffer's first element is the same as its last element
             * 3. Buffer on both sides means read the side that has 1st element == last element */
            buffers match {
              // No buffer on either side means we need a fresh read on both sides
              case (Vector(), Vector()) => readBoth(s, contf, buffers, orderJ, orderK)
              // Buffer only on one side means a definite read on the other, and conditional on the existing if b.head == b.last
              case (Vector(), right) if orderK.equal(right.head, right.last) => readBoth(s, contf, buffers, orderJ, orderK)
              case (Vector(), right) => readLeft(s, contf, buffers, orderJ, orderK)
              case (left, Vector())  if orderJ.equal(left.head, left.last)   => readBoth(s, contf, buffers, orderJ, orderK)
              case (left, Vector()) => readRight(s, contf, buffers, orderJ, orderK)
              // Buffer on both sides means conditional on each side having an equivalent series
              case (left, right) if orderJ.equal(left.head, left.last) && orderK.equal(right.head, right.last) => readBoth(s, contf, buffers, orderJ, orderK)
              case (left, _) if orderJ.equal(left.head, left.last) => readLeft(s, contf, buffers, orderJ, orderK)
              case (_, right) if orderK.equal(right.head, right.last) => readRight(s, contf, buffers, orderJ, orderK)
            }
          },
          done = (a, r) => done[X, Vector[J], IterateeM, StepM[A]](sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
          err  = x => err[X, Vector[J], IterateeM, StepM[A]](x)
        )
      }

      def apply[A] = {
        step[A](_, (Vector(),Vector()), jOrder, kOrder)
      }
    }

  def joinIChunked[X, J, K, F[_]](implicit M: Monad[F], ord: (J, K) => Ordering, orderJ: Order[J], orderK: Order[K]): Enumeratee2T[X, Vector[J], Vector[K], Vector[(J, K)], F] = {
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

        (step: StepT[X, Vector[(J, K)], F, A]) => cogroupIChunked.apply(cstep(step)) flatMap { endStepChunked[X, J, K, Vector[(J, K)], F, A] }
      }
    }
  }

  def mergeIChunked[X, B, F[_]](implicit M: Monad[F], ord: Order[B]): Enumeratee2T[X, Vector[B], Vector[B], Vector[B], F] = {
    type E = Vector[B]
    new Enumeratee2T[X, E, E, E, F] {
      type Surplus = Either[E,E]

      def mergedToInput(e: E): Input[E] = elInput(e)

      def innerMerge(l: E, r: E, acc: E, order: Order[B]): (Option[E], Option[Surplus]) = {
        var lIndex = 0
        var rIndex = 0

        var buffer = ArrayBuffer[B]()

        // Merge both sides as long as both sides have input
        while (lIndex < l.length && rIndex < r.length) {
          if (order.order(l(lIndex), r(rIndex)) == LT) {
            buffer += l(lIndex)
            lIndex += 1
          } else {
            buffer += r(rIndex)
            rIndex += 1
          }
        }

        // Determine the surplus based on which side ran out first
        val surplus = if (l.length == 0 && r.length == 0) {
          None
        } else {
          Some(if (lIndex < l.length) {
            Left(l.drop(lIndex))
          } else {
            Right(r.drop(rIndex))
          })
        }

        (Some(Vector(buffer: _*)), surplus)
      }

      def mergeChunks[A](contf: Input[E] => IterateeT[X,E,F,A], left: Option[E], right: Option[E], order: Order[B]): IterateeT[X, E, IterateeM, StepM[A]] = {
        val (merged, surplus) = (left, right) match {
          case (Some(l), Some(r)) => {

            innerMerge(l, r, Vector.empty[B], order)
          }
          case (None, None) => (None, None)
            case (None, r) => (r, None)
              case (l, None) => (l, None)
        }


        iterateeT[X, E, IterateeM, StepM[A]](contf(merged.map(mergedToInput).getOrElse(eofInput)) >>== nextStep(surplus, order))
      }

      def nextStep[A](surplus: Option[Surplus], order: Order[B])(v: StepM[A]) = step(v, surplus, order).value

      def step[A](s: StepM[A], previousSurplus: Option[Surplus], order: Order[B]): IterateeT[X, E, IterateeM, StepM[A]] = {

        s.fold[IterateeT[X, E, IterateeM, StepM[A]]](
          cont = contf => {
            previousSurplus match {
              case Some(Left(left)) => {
                for {
                  rightOpt <- head[X, E, F].liftI[E]
                  a        <- mergeChunks(contf, Some(left), rightOpt, order)
                } yield a
              }
              case Some(Right(right)) => {
                for {
                  leftOpt  <- head[X, E, IterateeM]
                  a        <- mergeChunks(contf, leftOpt, Some(right), order)
                } yield a
              }
              case None => {
                for {
                  leftOpt  <- head[X, E, IterateeM]
                  rightOpt <- head[X, E, F].liftI[E]
                  a        <- mergeChunks(contf, leftOpt, rightOpt, order)
                } yield a
              }
            }
          },
          done = (a, r) => done[X, E, IterateeM, StepM[A]](sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
          err  = x => err(x)
        )
      }

      def apply[A] = {
        step(_, None, ord)
      }
    }
  }

  def parFoldIChunked[X, J, K, F[_]](f: K => J)(implicit order: (J, K) => Ordering, orderJ: Order[J], orderK: Order[K], m: Monoid[J], M: Monad[F]): Enumeratee2T[X, Vector[J], Vector[K], Vector[J], F] =
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

        (step: StepT[X, Vector[J], F, A]) => cogroupIChunked.apply(cstep(step)) flatMap { endStepChunked[X, J, K, Vector[J], F, A] }
      }
    }

  private def endStepChunked[X, J, K, EE, F[_]: Monad, A](sa: StepT[X, Vector[Either3[J, (J, K), K]], F, StepT[X, EE, F, A]]):
  IterateeT[X, Vector[J], ({ type λ[α] = IterateeT[X, Vector[K], F, α] })#λ, StepT[X, EE, F, A]] = {
    IterateeT.IterateeTMonadTransT[X, Vector[J], ({ type λ[β[_], α] = IterateeT[X, Vector[K], β, α] })#λ].liftM(sa.pointI.run(x => err[X, EE, F, A](x).value))
  }
}

object Enumeratee2T extends Enumeratee2TChunkedFunctions

// vim: set ts=4 sw=4 et:
