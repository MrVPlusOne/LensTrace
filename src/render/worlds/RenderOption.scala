package render.worlds

import render.objects.{Camera, RelaScene}

/**
 * Created by wjydzh1 on 1/3/2015.
 */
abstract class RenderOption{
  def relaScene:RelaScene
  def camera: Camera
  def worldName:String

  override def toString=worldName
}
