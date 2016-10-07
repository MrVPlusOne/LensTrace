package tests

import render.geomeries.{GeoInfo, UnitSphere}
import render.maths._
import render.objects._

/**
 * Created by wjydzh1 on 12/23/2014.
 */
class RelaSceneTest extends MyTest{
  "A scene with only one sphere at origin" when{
    val background=Radiance.blue
    val redMat=new Mat{
      override def shade(localRay: Ray, checkResult: HitCheckResult, info: GeoInfo, scene: RelaScene, sample: RandomSample, simplify: Int, instance: HitInstance): RadianceSet = Radiance.red
    }
    val sampler=new SimpleRandom
    val sphere=new SimpleInstance(UnitSphere,Vector3.right,redMat)
    val scene=new RelaScene(List(sphere),Nil,background)
    "traceRay" should{
      "return background color" in{
        val result: TraceResult = scene.traceRay(Ray(Event(Vector3.back * 3, 0), Vector3.up), sampler)
        result.radianceSet.radiance shouldBe background
        result.globalEvent shouldBe Event.positiveInf
      }
      "return red color" in{
        scene.traceRay(Ray(Event(Vector3.right + Vector3.back * 2, 0), Vector3.front), sampler).radianceSet.radiance shouldBe Radiance.red
      }
    }
    "sphere hitChecked" should{
      "return right normal" in{
        val result=sphere.hitCheck(Ray(Event(Vector3.right+Vector3.back*2,0),Vector3.front)).get
        result.isOut shouldBe true
        result.globalEvent shouldBe Event(Vector3.right+Vector3.back,1)
        result.localEvent shouldBe Event(Vector3.back,1)
      }
    }
  }
}
