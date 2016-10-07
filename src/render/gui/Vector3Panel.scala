package render.gui

import javax.swing.border.EmptyBorder

import render.maths.Vector3

import scala.swing._
import scala.swing.event.EditDone

/**
 * relative to a vector3
 */
class Vector3Panel(v0:Vector3,edited:Vector3Panel=>Unit) extends BoxPanel(Orientation.Horizontal){

  def label(title:String)=new Label(title){border=Preset.labelBoarder}
  def box(d:Double)=new DoubleBox(d,d=>edited(this)){preferredSize=Preset.doubleBoxSize}
  val boxes=(0 until 3).map(i=>box(v0(i)))
  contents+=(label("x"),boxes(0),label("y"),boxes(1),label("z"),boxes(2))

  def getVector3={
    val xyz=boxes.map(box=>box.value)
    Vector3(xyz(0),xyz(1),xyz(2))
  }

  def setVector3(v:Vector3): Unit ={
    (0 until 3).map(i=>boxes(i) value= v(i))
  }
}

class DoubleBox(init:Double,edited:Double=>Unit) extends NumBox[Double](init,edited,s=>s.toDouble,d=>"%.3f".format(d))
class IntBox(init:Int,edited:Int=>Unit) extends NumBox[Int](init,edited,s=>s.toInt,int=>int.toString)

class NumBox[T](private var v:T,edited:T=>Unit,toNumFunc:String=>T,toStringFunc:T=>String) extends TextField{
  text=v.toString

  def value=v
  def value_=(newValue:T): Unit = {
    v = newValue
    text = toStringFunc(value)
    edited(value)
  }

  listenTo(this)
  reactions+={
    case EditDone(_)=>{
      try{
        val newValue=toNumFunc(text)
        if(newValue!=value){
          value_=(newValue)
        }
      }catch {
        case e:NumberFormatException=>text=toStringFunc(value)
      }
    }
  }
}

object Vector3PanelTest extends SimpleSwingApplication{
  override def top: Frame = new MainFrame(){
    contents=new Vector3Panel(Vector3(1,2,3),vp=>println(vp.getVector3)){setVector3(Vector3(4,5,6))}
  }
}