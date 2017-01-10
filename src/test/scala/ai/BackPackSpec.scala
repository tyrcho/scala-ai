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
  
  it should "find optimal items in a case with repeats" in {
    val items=Seq((5,100), (5,100), (3,0),(4,60),(2,20),(3,36),(3,36),(3,36))
    BackPack.best[(Int,Int)](items, 10,  _._1, _._2) shouldBe Seq((5,100),(5,100)) 
  }
  
//      [['name' : "attack with 45", 'cost' : 5, 'value' : 97.5],
//	 ['name' : "attack with 45", 'cost' : 5, 'value' : 97.5]],
//	function(){ return chooseOptions([
//		['name' : "helmet", 'cost' : 3, 'value' : 0],
//		['name' : "cure", 'cost' : 4, 'value' : 62.4],
//		['name' : "bandage", 'cost' : 2, 'value' : 20.0],
//		['name' : "attack with 45", 'cost' : 5, 'value' : 97.5],
//		['name' : "attack with 45", 'cost' : 5, 'value' : 97.5],
//		['name' : "attack with spark chip", 'cost' : 3, 'value' : 36.0],
//		['name' : "attack with spark chip", 'cost' : 3, 'value' : 36.0],
//		['name' : "attack with spark chip", 'cost' : 3, 'value' : 36.0]], 10);
//

 
}