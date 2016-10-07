package tests

import render.maths._

/**
 * Created by wjydzh1 on 12/23/2014.
 */
class TransformTest extends MyTest{
  "A StaticTransfrom" when{
    "equals Identity" should{
      val transform=new StaticTransform(Vector3.zero,Vector3.front,0)
      "do nothing" in{
        val v=Vector3(1,2,3)
        transform.transformPoint(v) shouldBe v
        transform.transformLightDirec(v) shouldBe v
        transform.transformEvent(Event(v,5)) shouldBe Event(v,5)
      }
    }
    "looks to left" should{
      val transform=new StaticTransform(Vector3.right*5,Vector3.left,10)
      "transform" in{
        val v=Vector3.front
        transform.transformPoint(v) shouldBe v+Vector3.right*5
        transform.transformLightDirec(v) shouldBe Vector3.left
        transform.transformEvent(Event(Vector3.zero,2)) shouldBe Event(Vector3.right*5,10+2)
      }
    }
  }
}
