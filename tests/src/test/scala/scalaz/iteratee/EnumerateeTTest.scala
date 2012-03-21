package scalaz
package iteratee

import std.AllInstances._
import Iteratee._
import Enumeratee2T._
import effect._

class EnumerateeTTest extends Spec {
  "map" in {
    val iter = sum[Unit, Int, Id] %= EnumerateeT.map[Unit, String, Int, Id]((s: String) => s.toInt)
    (iter &= enumStream(Stream("1", "2", "3"))).runOrZero must be_===(6)
  }

  "uniqChunk" in {
    val iter = consume[Unit, Vector[Int], Id, List] %= EnumerateeT.uniqChunk[Unit, Int, Id]
    (iter &= enumStream(Stream(Vector(1, 2, 2), Vector(2, 3), Vector(4, 4, 5, 5, 6)))).run(_ => sys.error("...")).flatten.toList must be_===(List(1, 2, 3, 4, 5, 6))
  }
}


// vim: set ts=4 sw=4 et:
