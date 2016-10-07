package tests

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

/**
 * Created by wjydzh1 on 12/16/2014.
 */
abstract class MyTest extends WordSpec with ShouldMatchers{
  val tolerance=1e-8
  def shouldNear(a:Double,b:Double)=a shouldBe(b +- tolerance)
}
