package render.gui

import java.awt.{Color, Dimension}
import javax.swing.border.{LineBorder, EmptyBorder}
import render.maths.Vector3
import render.objects.{Camera, RelaScene}
import render.worlds.{Worlds, RenderOption}
import scala.swing._
import scala.swing.event._

object Main extends SimpleSwingApplication{

  override def top: Frame = new MainFrame(){
    title= "Rendering Control [Einstein]"
    contents = new ControlPanel()
    resizable=false
  }
}


class ControlPanel extends BoxPanel(Orientation.Vertical){
  val screenView=new ScreenView(new ScreenModel(600,600))

  val frame=new MainFrame(){
    contents=new BoxPanel(Orientation.Horizontal){
      title="RenderingResult"
      contents+=screenView
    }
  }
  frame.visible=true

  private val startButton: Button = new Button("Start")
  private val stopButton:Button= new Button("Stop")

  private val progressBar=new ProgressBar(){label="Progress";labelPainted=true}
  private val timeUseLabel=new Label("Time Used"){preferredSize = new Dimension(150,40)}
  private val timeEstLabel=new Label("Total Time Est"){preferredSize= new Dimension(150,40)}

  private def setMaxProgress(m:Int)=progressBar.max=m
  private def setProgress(p:Int)= {
    progressBar.value = p
    progressBar.label = "%s/%s".format(p, progressBar.max)
  }
  private def setTotalTime(leftTime:Double)={
    timeEstLabel.text="Total time est: %.1fs".format(leftTime)
  }

  def availableScenes(): Seq[RenderOption] = List(
    Worlds.withCylinder,
    Worlds.colorfulGlassBall,
    Worlds.pyramidAndGlassBall,
    Worlds.twoBallInterfere,
    Worlds.highAccuracy
  )

  private val sceneSelector=new ComboBox[RenderOption](availableScenes())
  private var currentSelection=sceneSelector.selection.item
  private val cameraPanel=new CameraPanel(currentSelection.camera){border=new EmptyBorder(10,10,10,10)}

  private def selectScene()={
    println("selection changed! ")
    val newSelect=sceneSelector.selection.item
    if(newSelect!=currentSelection){
      cameraPanel.setCamera(newSelect.camera)
      currentSelection=newSelect
    }
  }

  contents += new FlowPanel(){
    border=new EmptyBorder(20,20,10,20)
    contents+=new Label("Scene:"){border=new EmptyBorder(5,10,5,10)}
    contents+=sceneSelector
  }
  contents+=cameraPanel

  contents += progressBar
  contents += new FlowPanel(){
    contents += timeUseLabel
    contents += timeEstLabel
  }
  contents += new FlowPanel{
    contents+=startButton
    contents+=stopButton
  }
  contents+=new Label("Written by Jiayi Wei, USTC"){foreground=Color.blue}
  
  listenTo(sceneSelector.selection,startButton,stopButton)
  
  reactions+={
    case SelectionChanged(`sceneSelector`)=>selectScene()
    case ButtonClicked(`startButton`)=> startRendering(currentSelection.relaScene,cameraPanel.getCamera)
    case ButtonClicked(`stopButton`)=> screenView.cancelRender()
  }

  def startRendering(scene:RelaScene,camera:Camera):Unit = {
    import render.utils.Timer
    val timer=new Timer(100,timeUse=>{
      timeUseLabel.text="Time used: %.1fs".format(timeUse/1000.0)
    })
    def startAction(): Unit ={
      timer.start()
    }
    startButton.enabled=false
    def endAction(): Unit ={
      timer.stop()
      startButton.enabled=true
    }

    screenView.startRender(scene,camera,4,setMaxProgress,setProgress,setTotalTime,startAction,endAction)
  }

}
