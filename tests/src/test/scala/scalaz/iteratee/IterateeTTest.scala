package scalaz
package iteratee

import std.AllInstances._
import Iteratee._
import Enumeratee2T._
import effect._

class IterateeTTest extends Spec {
  "head" in {
    (head[Unit, Int, Id] &= enumStream(Stream(1, 2, 3))).runOrZero must be_===(Some(1))
  }

  "consume" in {
    (consume[Unit, Int, Id, List] &= enumStream(Stream(1, 2, 3))).runOrZero must be_===(List(1, 2, 3))
  }

  "fold in constant stack space when using a trampolined monad" in {
    //using IO because it's trampolined
    (fold[Unit, Int, IO, Int](0){ case (a,v) => a + v } &= enumStream[Unit, Int, IO](Stream.fill(10000)(1))).runOrZero.unsafePerformIO must be_===(10000)
  }

  "chain dissimilar iteratees through the end of an enumerator" in {
    val nextIter = consume[Unit, Int, Id, List].withResult(enumStream(Stream(1, 2, 3))) {
      l => fold[Unit, String, Id, List[String]](l.map(_.toString)) { case (a, v) => v :: a }
    } 
    
    (nextIter &= enumStream(Stream("hello", "world"))).runOrZero must be_===(List("world", "hello", "1", "2", "3"))
  }

  "finish" in {
    (sum[Unit, Int, Id] &= enumStream(Stream(1, 2, 3))).finish.value must_== StepT.Done[Unit, Int, Id, Int](6, emptyInput)
  }

  object instances {
    object iterateet {
      def monad[F[_]: Monad, X, E] = Monad[({type λ[α] = IterateeT[X, E, F, α]})#λ]
      def liftIO[F[_]: MonadIO, X, E] = LiftIO[({type λ[α] = IterateeT[X, E, F, α]})#λ]
      def monadIO[F[_]: MonadIO, X, E] = MonadIO[({type λ[α] = IterateeT[X, E, F, α]})#λ]
    }

    object iteratee {
      def monad[X, E, F] = Monad[({type λ[α] = Iteratee[X, E, α]})#λ]
    }
  }
}
