package scalaz
package iteratee

import Iteratee._
import Enumeratee2T._

import scala.annotation.tailrec
import scalaz.syntax.Syntax.bind._
import scalaz.syntax.Syntax.order._

trait ForallM[P[_[_]]] {
  def apply[F[_]: Monad]: P[F]
}

abstract class EnumeratorP[X, E, F[_]] { self =>
  def apply[G[_]](implicit MO: MonadPartialOrder[G, F]): EnumeratorT[X, E, G]

  def map[B](f: E => B): EnumeratorP[X, B, F] = 
    new EnumeratorP[X, B, F] {
      def apply[G[_]](implicit MO: MonadPartialOrder[G, F]) = {
        import MO._
        self[G].map(f)
      }
    }

  def flatMap[B](f: E => EnumeratorP[X, B, F]) = 
    new EnumeratorP[X, B, F] {
      def apply[G[_]](implicit MO: G |>=| F): EnumeratorT[X, B, G] = {
        import MO._
        self[G].flatMap(e => f(e).apply[G])
      }
    }

  def collect[B](pf: PartialFunction[E, B]) = 
    new EnumeratorP[X, B, F] {
      def apply[G[_]](implicit MO: G |>=| F): EnumeratorT[X, B, G] = {
        import MO._
        self[G].collect(pf)
      }
    }

  def uniq(implicit ord: Order[E]) = 
    new EnumeratorP[X, E, F] {
      def apply[G[_]](implicit MO: G |>=| F): EnumeratorT[X, E, G] = {
        import MO._
        self[G].uniq
      }
    }

  def uniqChunk[B](implicit ord: Order[B], ev: E =:= Vector[B]) =
    new EnumeratorP[X, Vector[B], F] {
      def apply[G[_]](implicit MO: G |>=| F): EnumeratorT[X, Vector[B], G] = {
        import MO._
        self[G].uniqChunk
      }
    }

  def zipWithIndex = 
    new EnumeratorP[X, (E, Long), F] {
      def apply[G[_]](implicit MO: G |>=| F): EnumeratorT[X, (E, Long), G] = {
        import MO._
        self[G].zipWithIndex
      }
    }

  def zipChunkWithIndex[B](implicit ev: E =:= Vector[B]) = 
    new EnumeratorP[X, Vector[(B, Long)], F] {
      def apply[G[_]](implicit MO: G |>=| F): EnumeratorT[X, Vector[(B, Long)], G] = {
        import MO._
        self[G].zipChunkWithIndex
      }
    }

  def :^[A, B, C](other: EnumeratorP[X, C, F])(implicit evE: E =:= Vector[A], evC: C =:= Vector[B]): EnumeratorP[X, Vector[(A, B)], F] = 
    new EnumeratorP[X, Vector[(A, B)], F] {
      def apply[G[_]](implicit MO: G |>=| F) = {
        import MO._
        cross(self[G].map(evE), other[G].map(evC))
      }
    }

  def ^:[A, B, C](other: EnumeratorP[X, C, F])(implicit evE: E =:= Vector[A], evC: C =:= Vector[B]): EnumeratorP[X, Vector[(A, B)], F] =
    new EnumeratorP[X, Vector[(A, B)], F] {
      def apply[G[_]](implicit MO: G |>=| F) = {
        import MO._
        cross(self[G].map(evE), other[G].map(evC))
      }
    }

  def join[B](other: EnumeratorP[X, E, F])(implicit order: Order[B], m: Monad[F], ev: E =:= Vector[B]): EnumeratorP[X, Vector[(B, B)], F] =
    EnumeratorP.joinE[X, B, B, F](m, order.order).apply(self.map(ev), other.map(ev))

  def merge[B](other: EnumeratorP[X, E, F])(implicit o: Order[B], m: Monad[F], ev: E =:= Vector[B]) = 
    EnumeratorP.mergeE[X, B, F].apply(self.map(ev), other.map(ev))
}

trait EnumeratorPFunctions {
  def empty[X, E, F[_]]: EnumeratorP[X, E, F] = new EnumeratorP[X, E, F] {
    def apply[G[_]](implicit MO: MonadPartialOrder[G, F]) = {
      import MO._
      EnumeratorT.empty[X, E, G]
    }
  }

  def perform[X, E, F[_], B](f: F[B]): EnumeratorP[X, E, F] = new EnumeratorP[X, E, F] {
    def apply[G[_]](implicit MO: MonadPartialOrder[G, F]) = {
      import MO._
      EnumeratorT.perform[X, E, G, B](MO.promote(f))
    }
  }

  def enumPStream[X, E, F[_]: Monad](xs : Stream[E]): EnumeratorP[X, E, F] = new EnumeratorP[X, E, F] {
    def apply[G[_]](implicit MO: MonadPartialOrder[G, F]): EnumeratorT[X, E, G] = {
      import MO._
      enumStream[X, E, G](xs)
    }
  }

  def liftE2[X, J, K, I, F[_]](e2t: ForallM[({type λ[β[_]] = Enumeratee2T[X, J, K, I, β]})#λ]): (EnumeratorP[X, J, F], EnumeratorP[X, K, F]) => EnumeratorP[X, I, F] = {
    (e1: EnumeratorP[X, J, F], e2: EnumeratorP[X, K, F]) => new EnumeratorP[X, I, F] {
      def apply[G[_]](implicit MO: MonadPartialOrder[G, F]): EnumeratorT[X, I, G] = 
        new EnumeratorT[X, I, G] {
          import MO._
          implicit val IOrd = MO.transform[({ type λ[β[_], α] = IterateeT[X, K, β, α] })#λ]
          lazy val enum1 = e1[({ type λ[α] = IterateeT[X, K, G, α]})#λ]
          lazy val enum2 = e2[G]

          def apply[A] = {
            (step: StepT[X, I, G, A]) => iterateeT(((e2t[G].apply(step) &= enum1).run(err _) &= enum2).run(x => MO.promote(Monad[F].point(serr(x)))))
          }
        }
    }
  }

  def cogroupE[X, J, K, F[_]](implicit M: Monad[F], ord: (J, K) => Ordering) = liftE2[X, Vector[J], Vector[K], Vector[Either3[J, (J, K), K]], F] {
    new ForallM[({type λ[β[_]] = Enumeratee2T[X, Vector[J], Vector[K], Vector[Either3[J, (J, K), K]], β]})#λ] {
      def apply[G[_]: Monad] = cogroupI[X, J, K, G]
    }
  }

  def joinE[X, J, K, F[_]](implicit M: Monad[F], ord: (J, K) => Ordering) = liftE2[X, Vector[J], Vector[K], Vector[(J, K)], F] { 
    new ForallM[({type λ[β[_]] = Enumeratee2T[X, Vector[J], Vector[K], Vector[(J, K)], β]})#λ] {
      def apply[G[_]: Monad] = joinI[X, J, K, G]
    }
  }

  def mergeE[X, B, F[_]](implicit o: Order[B], fm: Monad[F]) = liftE2[X, Vector[B], Vector[B], Vector[B], F] { 
    type E = Vector[B]
    new ForallM[({type λ[β[_]] = Enumeratee2T[X, E, E, E, β]})#λ] {
      def apply[G[_]: Monad] = mergeI[X, B, G]
    }
  }

  def mergeAll[X, B, F[_]](enumerators: EnumeratorP[X, Vector[B], F]*)(implicit o: Order[B], fm: Monad[F]): EnumeratorP[X, Vector[B], F] = { 
    type E = Vector[B]
    @tailrec def mergeOne(e: EnumeratorP[X, E, F], es: List[EnumeratorP[X, E, F]]): EnumeratorP[X, E, F] = es match {
      case x :: xs => mergeOne(e merge x, xs) 
      case Nil => e
    }   

    enumerators.toList match {
      case x :: xs => mergeOne(x, xs) 
      case Nil => empty[X, E, F]
    }   
  }
}

object EnumeratorP extends EnumeratorPFunctions

// vim: set ts=4 sw=4 et:
