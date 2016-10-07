package render.gui


import scala.actors._
import scala.actors.Actor._

abstract class CalcMaster[Send,Receive]
  (val partNumber:Int, val antiAliasing:Int, val coreNumber:Int) extends Actor{
  
  def dataToSend(partIndex:Int,turn:Int):Send
  def receiveResult(result:Receive):Unit
  def coreMission(data:Send):Receive
  def callBack(status:EndingStatus):Unit

  val maxIndex=antiAliasing*antiAliasing*partNumber
  var index=0
  def getProgress:Int=index


  val cores=new Array[Core](coreNumber)
  private var canceled=false

  override def act(): Unit = {
    for(i<-0 until coreNumber){
      val core = Core(i)
      cores(i)=core
      core.start()
    }
    handleRequest()
  }

  def sendMsg(core:OutputChannel[Any],ins: MasterInstructions[Send])=core ! ins

  def cancelMission():Unit={
    canceled=true
  }
  
  def handleRequest(): Unit ={
    //distribute computation
    while(index<maxIndex&& !canceled) {
      def sendData()={
        val newMission=dataToSend(index % partNumber,index/partNumber)
        sendMsg(sender,MissionData(newMission))
      }
      
      receive {
        case mData:MissionData[Receive] => {
          receiveResult(mData.data)
          sendData()
        }
        case Awake => {
          sendData()
        }
      }
      index+=1
    }
    //time to end
    for(i<-0 until coreNumber){
      receive{
        case mData:MissionData[Receive] => {
          receiveResult(mData.data)
          sendMsg(sender,Stop)
        }
      }
    }
    callBack(if(canceled) Canceled else Succeeded)
  }

  case class Core(id:Int) extends Actor{
    val master=CalcMaster.this

    def sendToMaster(coreInstructions: CoreInstructions[Receive])=master ! coreInstructions

    override def act(): Unit = {
      sendToMaster(Awake)
      var stopped=false
      while(!stopped) {
        receive {
          case mData: MissionData[Send] => {
            val result = coreMission(mData.data)
            sendToMaster(MissionData(result))
          }
          case Stop => {
            println(s"***core $id stopped")
            stopped=true
          }
        }
      }
    }
  }
  sealed trait EndingStatus
  object Succeeded extends EndingStatus{
    override def toString="Succeeded"
  }
  object Canceled extends EndingStatus{
    override def toString="Canceled"
  }

  sealed trait MasterInstructions[+D]
  object Stop extends MasterInstructions[Nothing]

  sealed trait CoreInstructions[+T]
  object Awake extends CoreInstructions[Nothing]

  case class MissionData[D](data:D) extends MasterInstructions[D] with CoreInstructions[D]

}






