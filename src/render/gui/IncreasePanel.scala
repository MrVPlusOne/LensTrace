package render.gui

import scala.swing._
import scala.swing.event.ButtonClicked

class IncreasePanel(label:String,delta:Double,changeAction:Double=>Unit) extends BoxPanel(Orientation.Horizontal){
  val deltaBox=new DoubleBox(delta,d=>()){preferredSize=Preset.doubleBoxSize}
  val increaseButton=new Button("+")
  val decreaseButton=new Button("-")
  contents+=(new Label(label){border=Preset.labelBoarder},deltaBox,increaseButton,decreaseButton)

  listenTo(increaseButton,decreaseButton)

  reactions+={
    case ButtonClicked(`increaseButton`)=>changeAction(deltaBox.value)
    case ButtonClicked(`decreaseButton`)=>changeAction(-deltaBox.value)
  }
}

object IncreasePanelTest extends SimpleSwingApplication{
  override def top: Frame = new MainFrame(){
    contents=new IncreasePanel("Let's go",5,d=>println(s"Changed: $d"))
  }
}