import org.junit.Test;
import org.junit.Assert._;
import com.mylife.test._;
import argonaut._, Argonaut._

class AtdsTest {

  @Test
  def testSimpleSum() {
    val i: SampleSum = SampleSum.Integer(42)
    assertEquals("""["Int",42]""", i.asJson.nospaces)

    val b = SampleSum.Bool(true)
    assertEquals("""["Bool",true]""", b.asJson.nospaces)

    val s = SampleSum.SimpleTag
    assertEquals("\"Simple_tag\"", s.asJson.nospaces)
  }

  @Test
  def testDefaults() {
    val r = RecordWithDefaults(e = E.Alpha)
    assertEquals("""{"e":"Alpha","s":"","i":0,"b":false,"l":[],"o":null}""", r.asJson.nospaces)
  }

  @Test
  def testSumWithRecord {
    val r = SimpleRecord(57, None)
    val s = SampleSum.SimpleRec(r)

    val rstr = """{"int_field":57,"opt":null}"""
    assertEquals(rstr, r.asJson.nospaces)
    assertEquals(s"""["Simple_rec",$rstr]""", s.asJson.nospaces)
  }

}
object AtdsTest {

  def main(args: Array[String]) {
    org.junit.runner.JUnitCore.main("AtdsTest");
  }

}
