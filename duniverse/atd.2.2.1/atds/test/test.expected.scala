// Automatically generated; do not edit
package com.mylife

import argonaut._, Argonaut._

package object test {

/**
 * Common utility interface.
 */
trait Atds {

  /**
   * Get the Argonaut JSON representation.
   * Please use the argonaut encoder rather than calling this directly.
   */
  protected def toArgonaut: argonaut.Json

  // These may be optimized later, and the dependency on Argonaut could be removed.

  /**
   * Get the JSON string representation.
   * @return The JSON string.
   */
  def toJson: String = toArgonaut.nospaces

  /**
   * Write the JSON representation to a buffer.
   */
  def toJsonBuffer(out: java.lang.StringBuilder): Unit = out.append(toJson)

}

object Atds {

  implicit def argonautCodecAtds[A <: Atds] = new argonaut.EncodeJson[A] {
    override def encode(a: A) = a.toArgonaut
  }

}

/**
 * Construct objects of type e.
 */
sealed abstract class E extends Atds

/**
 * Define tags for sum type e.
 */
object E {
  case object Alpha extends E {
    override protected def toArgonaut: argonaut.Json = jString("Alpha")
  }
  case object Beta extends E {
    override protected def toArgonaut: argonaut.Json = jString("Beta")
  }
}

type Es = List[E]

case class SimpleRecord(
  int_field : Int,
  opt : Option[Boolean]
) extends Atds {

  override protected def toArgonaut: Json = Json(
    "int_field" := int_field,
    "opt" := opt
  )
}

/**
 * Construct objects of type sample_sum.
 */
sealed abstract class SampleSum extends Atds

/**
 * Define tags for sum type sample_sum.
 */
object SampleSum {
  case object SimpleTag extends SampleSum {
    override protected def toArgonaut: argonaut.Json = jString("Simple_tag")
  }
  case class Bool(data: Boolean) extends SampleSum {
    override protected def toArgonaut: argonaut.Json = argonaut.Json.array(
      jString("Bool"),
      data.asJson
     )
  }
  case class Integer(data: Int) extends SampleSum {
    override protected def toArgonaut: argonaut.Json = argonaut.Json.array(
      jString("Int"),
      data.asJson
     )
  }
  case class Float(data: Double) extends SampleSum {
    override protected def toArgonaut: argonaut.Json = argonaut.Json.array(
      jString("Float"),
      data.asJson
     )
  }
  case class S(data: String) extends SampleSum {
    override protected def toArgonaut: argonaut.Json = argonaut.Json.array(
      jString("String"),
      data.asJson
     )
  }
  case class SimpleRec(data: SimpleRecord) extends SampleSum {
    override protected def toArgonaut: argonaut.Json = argonaut.Json.array(
      jString("Simple_rec"),
      data.asJson
     )
  }
  case class ComplexRecord(data: ComplexRecord) extends SampleSum {
    override protected def toArgonaut: argonaut.Json = argonaut.Json.array(
      jString("Complex_record"),
      data.asJson
     )
  }
  case class RecordWithDefaults(data: RecordWithDefaults) extends SampleSum {
    override protected def toArgonaut: argonaut.Json = argonaut.Json.array(
      jString("Record_with_defaults"),
      data.asJson
     )
  }
}

/**
 * wibble
 */
case class ComplexRecord(
  b : Boolean,
  i : Int,
  s : String,
  l : List[Boolean],
  m : List[List[Int]],
  sample_sum : SampleSum,
  class_ : Option[Int],
  final_ : Option[Int],
  kase : String,
  l2 : List[RecordWithDefaults]
) extends Atds {

  override protected def toArgonaut: Json = Json(
    "b" := b,
    "i" := i,
    "s" := s,
    "l" := l,
    "m" := m,
    "sample_sum" := sample_sum,
    "class" := class_,
    "final" := final_,
    "case" := kase,
    "l2" := l2
  )
}

case class RecordWithDefaults(
  b : Boolean = false,
  i : Int = 0,
  s : String = "",
  o : Option[Boolean] = None,
  l : List[Boolean] = Nil,
  e : E
) extends Atds {

  override protected def toArgonaut: Json = Json(
    "b" := b,
    "i" := i,
    "s" := s,
    "o" := o,
    "l" := l,
    "e" := e
  )
}

case class BiggerRecord(
  b : Boolean = false,
  i : Int = 0,
  s : String = "",
  o : Option[Boolean] = None,
  l : List[Boolean] = Nil,
  e : E,
  more : String
) extends Atds {

  override protected def toArgonaut: Json = Json(
    "b" := b,
    "i" := i,
    "s" := s,
    "o" := o,
    "l" := l,
    "e" := e,
    "more" := more
  )
}

type Rec = RecordWithDefaults

type RWD = RecordWithDefaults

/**
 * Construct objects of type bigger_sum.
 */
sealed abstract class BiggerSum extends Atds

/**
 * Define tags for sum type bigger_sum.
 */
object BiggerSum {
  case object Alpha extends BiggerSum {
    override protected def toArgonaut: argonaut.Json = jString("Alpha")
  }
  case object Beta extends BiggerSum {
    override protected def toArgonaut: argonaut.Json = jString("Beta")
  }
  case object Gamma extends BiggerSum {
    override protected def toArgonaut: argonaut.Json = jString("Gamma")
  }
}

type Street = (Int, String)

type City = String

type State = String

case class Addr(
  street : Street,
  city : String,
  state : String
) extends Atds {

  override protected def toArgonaut: Json = Json(
    "street" := street,
    "city" := city,
    "state" := state
  )
}

}
