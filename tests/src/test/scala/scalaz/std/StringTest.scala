package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

class StringTest extends Spec {
  checkAll(monoid.laws[String])
  checkAll(isEmpty.laws[({type λ[α] = String})#λ])

  checkAll(order.laws[String].withProp("benchmark", order.scalaOrdering[String]))
}
