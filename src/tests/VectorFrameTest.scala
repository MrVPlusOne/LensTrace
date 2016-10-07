package tests

import render.maths._

class VectorFrameTest extends MyTest{
  "a trivial frame" when{
    val frame=VectorFrame.fromNormal(Vector3.up)
    "transform to global" should{
      "give identity result" in{
        val vecs=List(Vector3.left,Vector3.right,Vector3.up,Vector3.back,Vector3.front)
        vecs.map(v=>frame.localToGlobal(v) shouldBe v)
      }
    }
  }
  "another frame" when{
    val frame=VectorFrame.fromNormal(Vector3.left)
    "transform to global" should{
      "give transfromed result" in{
        frame.localToGlobal(Vector3.up) shouldBe Vector3.left
        frame.localToGlobal(Vector3.right) shouldBe Vector3.back
        frame.localToGlobal(Vector3.front) shouldBe Vector3.up
      }
    }
  }
}
