package tests

import render.maths._

/**
 * Created by wjydzh1 on 1/6/2015.
 */
class GeoTransformTests extends MyTest{
  "A translation transform" when{
    val offset=Vector3(1,2,3)
    val transform=InvGeoTransform.translation(offset)
    "transform points" should{
      "translate them" in{
        val p1=Vector3(0,0,0)
        transform.transformPoint(p1) shouldBe p1-offset
        val p2=Vector3(-3,2,9.5)
        transform.transformPoint(p2) shouldBe p2-offset
      }
    }
    "transform vectors" should{
      "not affect them" in{
        val p1=Vector3.zero
        transform.transformDirec(p1) shouldBe p1
        val p2=Vector3(2,3,4)
        transform.transformDirec(p2) shouldBe(p2)
      }
    }
  }
}
