package tests

import render.geomeries.{Missed, UnitSphere, Hit}
import render.maths._

/**
 * Created by wjydzh1 on 12/16/2014.
 */
class GeometryTest extends MyTest {
  "A unitSphere" when {
    "ray hit checked" should {
      "miss" in {
        val pz3 = Vector3.back * 3
        UnitSphere.checkHit(pz3, Vector3.up) shouldBe Missed
        UnitSphere.checkHit(pz3, Vector3.back) shouldBe Missed
      }
      "hit" in {
        UnitSphere.checkHit(Vector3.back * 3, Vector3.front) shouldBe Hit(2,true)
        UnitSphere.checkHit(Vector3.zero,Vector3.right) shouldBe Hit(1,false)
      }
    }

    "get hit info" should {
      "pass test" in {
        val geoInfo = UnitSphere.geoInfo(Vector3.up,true,null)
        geoInfo.normal shouldBe Vector3.up

        val insideInfo=UnitSphere.geoInfo(Vector3.up,false,null)
        insideInfo.normal shouldBe Vector3.down
      }

    }
  }
}
