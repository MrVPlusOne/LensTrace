package render.gui

import javax.swing.border.EmptyBorder

import render.maths._
import render.objects.Camera
import render.worlds.Worlds

import scala.swing._


class CameraPanel(camera:Camera) extends BoxPanel(Orientation.Vertical){

  private var window=Vector2.zero
  private var eye=Vector3.zero
  private var lookAt=Vector3.front
  private var time=0.0
  private var rec=0
  private val eyePanel=new Vector3Panel(eye,(p:Vector3Panel)=>eye=p.getVector3)
  private val lookAtPanel=new Vector3Panel(lookAt,(p:Vector3Panel)=>lookAt=p.getVector3)
  private val timeBox=new DoubleBox(time,t=>time=t)
  private val recBox=new IntBox(rec,i=>rec=i)

  setCamera(camera)

  def setCamera(camera: Camera):Unit={
    window=camera.window

    eyePanel.setVector3(camera.transform.pos)
    lookAtPanel.setVector3(camera.transform.lookAt)
    timeBox value_= camera.transform.time
    recBox value_= camera.maxRec
  }

  def relaEyePos=eye-lookAt
  def rotateX(delta:Double):Unit= {
    val rotation=Rotation.rotate(Vector3.up,delta)
    val newEyePos=rotation(relaEyePos)
    eyePanel.setVector3(lookAt+newEyePos)
  }
  def rotateY(delta:Double):Unit= {
    val rela=if(relaEyePos==Vector3.up) relaEyePos+Vector3.back*0.001 else relaEyePos
    val left = (rela cross Vector3.up).normalized
    val rotation=Rotation.rotate(left,delta)
    eyePanel.setVector3(lookAt+rotation(rela))
  }
  def zoom(percent:Double): Unit ={
    val newPercent= 1-percent
    eyePanel.setVector3(lookAt+relaEyePos*newPercent)
  }

  def horiBox(elements:Component*)=new BoxPanel(Orientation.Horizontal){
    elements.foreach(e=>contents+=e)
    border=new EmptyBorder(2,2,2,2)
  }
  def label(s:String)=new Label(s){preferredSize=new Dimension(75,25)}
  contents+=(
      new Label("Camera Settings"){border=new EmptyBorder(5,10,5,10)},
      horiBox(label("Eye point"),eyePanel),
      new IncreasePanel("Horizontal Rotation (deg)",10,d=>rotateX(d*Const.DegToPi)),
      new IncreasePanel("Vertical Rotation   (deg)",5,d=>rotateY(d*Const.DegToPi)),
      new IncreasePanel("Zoom (%)",20,d=>zoom(d/100)),
      horiBox(label("Look at"),lookAtPanel),
      horiBox(label("time"),timeBox),
      horiBox(label("Recursive"),recBox)
    )

  def getCamera = Camera(new StaticTransform(eye,lookAt,time),window,rec)
}


object CameraPanelTest extends SimpleSwingApplication{
  override def top=new MainFrame(){
    contents=new CameraPanel(Worlds.colorfulGlassBall.camera)
  }
}
