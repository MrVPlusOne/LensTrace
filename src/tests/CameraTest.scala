package tests

import org.scalatest.{ShouldMatchers, WordSpec}
import render.maths._
import render.objects._
import math.sqrt

class CameraTest extends MyTest {
  val identityTransform=new StaticTransform(Vector3.zero,Vector3.front,0)
  val camera=new Camera(identityTransform,Vector2(60,45),1)

  "A camera" when{
    "is %s".format(camera) should{
      "shoot front ray"in{
        Ray(Event.origin,Vector3.front,1) shouldBe camera.shootRay(Vector2(0.5,0.5),0)
      }
      "shoot another ray" in{
        val shooted: Ray = camera.shootRay(Vector2(1, 1), 5)
        shooted.direction.closeTo(Vector3(sqrt(3),1,-1).normalized) shouldBe true
        shooted.origin.t shouldBe 5
      }
    }
  }
}
