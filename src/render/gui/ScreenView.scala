package render.gui


import java.awt.image.BufferedImage

import render.maths.{SimpleRandom, RandomSample, Vector2, Ray}
import render.objects.{RadianceSet, Radiance, Camera, RelaScene}
import render.utils.Timer
import scala.swing._

class ScreenView(model:ScreenModel) extends Panel{
  val width=model.width
  val invWidth=1.0/width
  val height=model.height
  val invHeight=1.0/height

  val renderingImg= new BufferedImage(width,height,BufferedImage.TYPE_INT_ARGB)

  var renderingMaster:Option[CalcMaster[RenderingMission,RenderingResult]]=None
  
  preferredSize=new Dimension(width,height)

  override def paintComponent(graphics2D: Graphics2D)={
    graphics2D.drawImage(renderingImg,0,0,width,height,null)
  }

  def cancelRender(): Unit ={
    renderingMaster match{
      case None=>println("No active mission!")
      case Some(calcMaster)=>calcMaster.cancelMission()
    }
  }

  def clearScreen(): Unit ={
    renderingImg.setRGB(0,0,width,height,(0 until width*height).map(i=>new Color(0.7f,0.7f,1f).getRGB).toArray,0,width)
    repaint()
  }

  def startRender(scene: RelaScene,camera: Camera,antiAliasing:Int,
                  maxProgress:Int=>Unit,setProgress:Int=>Unit,setTimeLeft:Double=>Unit,startAction:()=>Unit,endAction:()=>Unit)={
    val startTime=System.currentTimeMillis()
    def getTimeUsed= System.currentTimeMillis()-startTime
    def calcTotalTime(progress:Int,max:Int):Double= {
      val timeUsed=getTimeUsed
      timeUsed*(max.toDouble/progress)/1000.0
    }

    clearScreen()

    val partitionList=model.cut(width/ScreenView.preferredCutSize+1,height/ScreenView.preferredCutSize+1)

    val master= new CalcMaster[RenderingMission,RenderingResult](
      partitionList.length,antiAliasing, ScreenView.coreNum) {
      maxProgress(this.maxIndex)

      override def dataToSend(partIndex: Int,turn:Int): RenderingMission = RenderingMission(
        partitionList(partIndex),turn)

      override def callBack(status: EndingStatus): Unit = {
        println(s"\nRendering $status!")
        endAction()
      }

      override def receiveResult(result: RenderingResult): Unit = {
        setProgress(this.getProgress)
        setTimeLeft(calcTotalTime(this.getProgress,this.maxIndex))

        val invTurn=1f/(result.turn+1)
        def radianceToColorInt(set:RadianceSet):Int={
          def round(f:Float):Int={
            val int=(f*255).toInt
            if(int>255) 255 else int
          }
          val complexPart=RadianceSet.color*set.complex.length.toFloat
          val rad=set.radiance+complexPart
          new Color(round(rad.r),round(rad.g),round(rad.b)).getRGB
        }

        val p=result.portion
        val (startX,startY,w,h)=(p.xFrom,p.yFrom,p.width,p.height)
        val portionData=p.portionData

        val ints=(for(r<-0 until h;
            c<-0 until w)yield{
          val radianceSet: RadianceSet = result.array(r)(c)
          val radiance=radianceSet.radiance
//          assert(radiance.r>=0&&radiance.g>=0&&radiance.b>=0,s"negative radiance : $radianceSet")
          val positive = radiance.r >= 0 && radiance.g >= 0 && radiance.b >= 0
          val set = if(positive){
            radianceSet
          }else{
            println(s"negative radiance : $radianceSet")
            radianceSet.copy(radiance = Radiance(1f,0f,1f))
          }

          portionData(r)(c)+=set
          val effectData=portionData(r)(c)*invTurn
          radianceToColorInt(effectData)
        }).toArray

        val img=new BufferedImage(w,h,BufferedImage.TYPE_INT_ARGB)
        img.setRGB(0,0,w,h,ints,0,w)
        renderingImg.getGraphics.drawImage(img,startX,startY,null)
        repaint()
      }

      val randomSample:RandomSample=new SimpleRandom()

      override def coreMission(data: RenderingMission): RenderingResult = {
        val portion=data.portion
        val w: Int = portion.width
        val h: Int = portion.height
        def shootRay(x:Int,y:Int,group:Int):Ray= {
          val Vector2(dx,dy)=randomSample.getSample(data.turn,group)
          camera.shootRay(
            Vector2(invWidth*(x+dx+portion.xFrom),invHeight*(height-(y+dy+portion.yTo))),
            localTime = 0.0
          )
        }

        val array:Array[Array[RadianceSet]]= (for(i<-0 until h) yield new Array[RadianceSet](w)).toArray
        for(y<-0 until h;x<-0 until w){
          val ray=shootRay(x,y,y*w+x)
          val radianceSet=scene.traceRay(ray, randomSample).radianceSet
          array(y)(x)=radianceSet
        }

        RenderingResult(array,portion,data.turn)
      }
    }
    
    renderingMaster=Some(master)
    master.start()
    startAction()
  }

}

object ScreenView{
  val coreNum=8
  var preferredCutSize=50
}

case class RenderingMission(portion: ScreenPortion,turn:Int)
case class RenderingResult(array:Array[Array[RadianceSet]],portion: ScreenPortion,turn:Int)



