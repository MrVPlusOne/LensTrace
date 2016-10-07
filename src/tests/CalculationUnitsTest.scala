package tests

import render.gui.CalcMaster

import scala.actors.Actor
import scala.actors.Actor._

/**
 * Created by wjydzh1 on 12/24/2014.
 */
class CalculationUnitsTest extends MyTest{
  "A CalcMaster" when{
    "calculating a sum" should{
      val s=self
      def sendResult(r:Int)= s ! r
      val antialiasing=3

      val master=new CalcMaster[Range,Int](10,antialiasing,5) {
        val ranges:Array[Range]= (for(i<-0 until partNumber)yield{Range(i*10+1,(i+1)*10)}).toArray
        var sum=0

        override def dataToSend(partIndex: Int,turn:Int): Range = ranges(partIndex)

        override def receiveResult(result: Int): Unit = sum+=result

        override def coreMission(data: Range): Int = {
          val num=data.end-data.start+1
          (data.start+data.end)*num/2
        }

        override def callBack(status: EndingStatus): Unit = sendResult(sum)
      }
      "give right answer" in {
        master.start()
        receive {
          case sum: Int => sum shouldBe antialiasing*antialiasing*(1 + 100) * 100 / 2
        }

      }
    }
  }
}
