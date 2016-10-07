package tests

import render.maths.{Event, Ray, Vector3}
import render.objects.{Radiance, ConeLight}

/**
 * Created by wjydzh1 on 12/31/2014.
 */
class LightTest extends MyTest{
  "a ConeLight" when{
    val radiance=Radiance.red
    val cone=new ConeLight(Vector3.down,10,radiance)
    "radiance at" should{
      "give zero radiance" in{
        val ray1=Ray(Event.origin,Vector3.left)
        val ray2=Ray(Event.origin,Vector3.down)
        val zero=Radiance.zero
        cone.radianceAt(ray1) shouldBe zero
        cone.radianceAt(ray2) shouldBe zero
      }
      "give white radiance" in{
        val ray1=Ray(Event.origin,Vector3.up)
        val ray2=Ray(Event.origin,Vector3.up+Vector3.left*0.1)
        cone.radianceAt(ray1) shouldBe radiance
        cone.radianceAt(ray2) shouldBe radiance
      }
    }
  }
}
