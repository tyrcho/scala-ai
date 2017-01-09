package ai

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BackPackSpec extends FlatSpec with Matchers {

  "a backpack" should "find optimal items in a simple case" in {
    val items=Seq((3,20), (4,30), (4,35))
    BackPack.best[(Int,Int)](items, 10,  _._1, _._2) shouldBe Seq((4,30), (4,35)) 
  }

 
}