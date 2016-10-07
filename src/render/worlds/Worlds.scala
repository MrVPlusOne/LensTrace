package render.worlds

import render.geomeries._
import render.maths.{InvGeoTransform, Vector2, StaticTransform, Vector3}
import render.objects._
import render.maths.Rotation

object Worlds {
  val groundChecker=new DiffuseMat(
    ColorFunctions.checkerPattern(Radiance.white*0.8f,Radiance.white*0.05f,0.5,0.5),
    AngleFunctions.simpleDiffuse,
    40, 5
  )

  val pyramidAndGlassBall=new RenderOption {
    override def worldName: String = "Pyramid And Glass Ball"

    override def relaScene: RelaScene = {
      val background=Radiance.zero

      val blueDiffuse=new DiffuseMat(
        ColorFunctions.pureColorSingleSide(Radiance(0.2f,0.2f,0.9f)),
        AngleFunctions.simpleDiffuse,
        10, 5)
      val reflectiveBlue=new Reflective(
        ColorFunctions.pureColorSingleSide(Radiance(0.5f,0.3f,1f))
      )
      val refractiveRed=new Refractive(
        1.6,0.1,ColorFunctions.pureColorSingleSide(Radiance.red),
        Radiance(0.2f,1.6f,1.6f),absort = 0.1
      )

      val V3=Vector3
      val sphere=new SimpleInstance(UnitSphere,Vector3.zero,refractiveRed)
      val ground=new SimpleInstance(Plane,Vector3.down,groundChecker)
      val triangle=new Triangle(Vector3(-2,-1,-2),Vector3(2,-1,-2),Vector3(0,3,-1.5))
      val mirror=new SimpleInstance(triangle,Vector3.zero,reflectiveBlue)
      val pyramid=TriangularMeshes.pyramid.map(t=>new SimpleInstance(t,Vector3(2,-1,1),reflectiveBlue)).toList

      val coneLight=new ConeLight(Vector3.down+Vector3.right+Vector3.front,15,Radiance.white*45)
      new RelaScene(List(sphere,ground)++pyramid,List(coneLight),background)
    }

    override def camera: Camera = {
      new Camera(
        new StaticTransform(Vector3(3,3,-5),Vector3.zero,0),
        Vector2(20,20),4)
    }
  }
  val colorfulGlassBall=new RenderOption {
    override def worldName: String = "Colorful Glass Ball"

    override def relaScene: RelaScene = {
      val background=Radiance.zero

      val blueDiffuse=new DiffuseMat(
        ColorFunctions.pureColorSingleSide(Radiance(0.2f,0.2f,0.9f)),
        AngleFunctions.simpleDiffuse,
        10, 5)
      val reflectiveBlue=new Reflective(
        ColorFunctions.pureColorSingleSide(Radiance(0.2f,0.3f,0.8f))
      )
      val colorfulMat=new Colorful(
        Spectrum.linearFunc(1.4,1.63),
        d=>0.5,
        Spectrum.linearSpectrum,
        0.1,ColorFunctions.pureColorSingleSide(Radiance.white),
        4
      )
      val refractiveRed=new Refractive(1.5,0.25,ColorFunctions.pureColorSingleSide(Radiance.zero),Radiance(0.05f,0.6f,0.6f),0.1)

      val V3=Vector3

      import render.maths.{InvGeoTransform=>Trans}
      val sphereTrans=Trans.mix(Vector3(1,1,1),Rotation.Identity,Vector3.zero)
      val sphere=new ComplexInstance(UnitSphere,sphereTrans,colorfulMat)
      val groundMat = new MixMat(List((groundChecker,0.6f),(blueDiffuse,0.3f)))  //This mat is really ugly!
      val ground=new ComplexInstance(Plane,Trans.translation(Vector3.down),groundChecker)

      val pyramid=TriangularMeshes.pyramid.map(t=>new SimpleInstance(t,Vector3(2,-0.99,1),colorfulMat))

      val coneLight=new ConeLight(Vector3.down*2+Vector3.right+Vector3.front,15,Radiance.white*50)
      new RelaScene(List(sphere,ground)++pyramid,List(coneLight),background)
    }

    override def camera: Camera = {
      new Camera(
        new StaticTransform(Vector3(3,2.5,-5.3),Vector3(1,0,0),0),
        Vector2(20,20),3)
    }
  }

  val withCylinder=new RenderOption {
    override def worldName: String = "With Cylinder"

    override def relaScene: RelaScene = {
      val background=Radiance.zero

      val wallMat=new DiffuseMat(
        ColorFunctions.pureColorSingleSide(Radiance(0.7f,0.1f,0.1f)),
        angleFunc = AngleFunctions.simpleDiffuse,
        sampleNum = 20,
        sampleSimplify = 5
      )

      val wall2Mat=new DiffuseMat(
        ColorFunctions.pureColorSingleSide(Radiance(0.1f,1.0f,0.1f)),
        angleFunc = AngleFunctions.simpleDiffuse,
        sampleNum = 20,
        sampleSimplify = 5
      )

      val colorfulMat=new Colorful(
        Spectrum.linearFunc(1.5,1.6),
        d=>0.2,
        Spectrum.linearSpectrum,
        0.1,ColorFunctions.pureColorSingleSide(Radiance.white*0.8f),
        60
      )


      import render.maths.{Vector3 => V3}


      val sphere=new SimpleInstance(UnitSphere,V3.zero,colorfulMat)

      val groundSize=16.0
      val groundMat =new DiffuseMat(
        ColorFunctions.checkerPattern(Radiance.white*0.8f,Radiance.white*0.05f,0.5/groundSize,0.5/groundSize),
        AngleFunctions.simpleDiffuse,
        125, 5
      )

      val groundMat_ = new MixMat(List(
        groundMat->0.9f,
        new Reflective(ColorFunctions.pureColorSingleSide(Radiance.white))->0.1f))

      val groundTrans=InvGeoTransform.mix(V3(groundSize,1,groundSize),Rotation.Identity,V3(-groundSize/2,-1,-groundSize/2))
      val ground=new ComplexInstance(UnitSquare,groundTrans,groundMat)

      val wallTrans=InvGeoTransform.mix(V3(4,1,10),Rotation.rotateZ(math.Pi/2),Vector3(-3,3,-5))
      val wall=new ComplexInstance(UnitSquare,wallTrans,wallMat)

      val wall2Trans=InvGeoTransform.mix(V3(4,1,10),Rotation.rotateZ(-math.Pi/2),Vector3(5,-1,-5))
      val wall2 = new ComplexInstance(UnitSquare,wall2Trans,wall2Mat)

      val cylinderTrans=InvGeoTransform.mix(V3(1,3,1),Rotation.Identity,V3(2,-1.01,1))
      val cylinder=new ComplexInstance(Cylinder,cylinderTrans,colorfulMat)

      val lightMat = new PureColor(Radiance.white * 100)

      val light1 = new SimpleInstance(UnitSphere, V3(2,3,6), lightMat)

      val pyramid=TriangularMeshes.pyramid.map(t=>new SimpleInstance(t,V3(2,-0.99,-1.0),colorfulMat)).toList

      val coneLight=new ConeLight(V3.down*0.5+V3.right+V3.front,10,Radiance.white*8)
      val coneLight2=new ConeLight(V3.down*0.5+V3.left+V3.front,10,Radiance.white*8)
      val vertical=new ConeLight(V3(1,-1,1),20,Radiance.white*10)

      new RelaScene(List(sphere,ground,/*wall, wall2,*/ cylinder, light1)/*++pyramid*/,List(),background)
    }

    override def camera: Camera = {
      new Camera(
        new StaticTransform(Vector3(1.102,4.950,-8.773),Vector3(1,0,0),0),
        Vector2(20,20),4)
    }
  }

  val twoBallInterfere=new RenderOption {
    override def worldName: String = "Two ball interfere"

    val h=30
    override def relaScene: RelaScene = {
      val ballMat=new PureColor(RadianceSet(Radiance.zero,Vector2(15,0)))
      val groundMat=new DiffuseMat(
        ColorFunctions.pureColorSingleSide(Radiance.white,0.9f),
        AngleFunctions.simpleDiffuse,
        40,5)

      val ball1=new ComplexInstance(CylinderSide,InvGeoTransform.mix(Vector3.unit,Rotation.rotateX(math.Pi/2),Vector3(-5,h,0)),ballMat)
      val ball2=new ComplexInstance(CylinderSide,InvGeoTransform.mix(Vector3.unit,Rotation.rotateX(math.Pi/2),Vector3(5,h,0)),ballMat)
      val ground=new SimpleInstance(Plane,Vector3.zero,groundMat)

      new RelaScene(List(ball1,ball2,ground),Nil,RadianceSet.zero)
    }

    override def camera: Camera = new Camera(
      new StaticTransform(Vector3(0,3*h,-3*h),Vector3(0,0,0),0),
      Vector2(30,30),4)
  }

  val highAccuracy=new RenderOption {
    override def worldName: String = "High Accuracy"

    override def relaScene: RelaScene = {
      val background=Radiance.zero

      val wallMatRed=new DiffuseMat(
        ColorFunctions.pureColorSingleSide(Radiance(1.0f,0.1f,0.1f)),
        angleFunc = AngleFunctions.simpleDiffuse,
        sampleNum = 20,
        sampleSimplify = 5
      )

      val wallMatGreen=new DiffuseMat(
        ColorFunctions.pureColorSingleSide(Radiance(0.1f,0.8f,0.1f)),
        angleFunc = AngleFunctions.simpleDiffuse,
        sampleNum = 20,
        sampleSimplify = 5
      )

      val wallMatYellow=new DiffuseMat(
        ColorFunctions.pureColorSingleSide(Radiance(1.0f,1.0f,0.7f)),
        angleFunc = AngleFunctions.simpleDiffuse,
        sampleNum = 50,
        sampleSimplify = 5
      )

      val colorfulMat=new Colorful(
        Spectrum.linearFunc(1.5,1.6),
        d=>0.2,
        Spectrum.linearSpectrum,
        0.1,ColorFunctions.pureColorSingleSide(Radiance.white*0.8f),
        6
      )

      val lightMat = new PureColor(Radiance.white * 30)

      val backgroundMat=new DiffuseMat(
        ColorFunctions.checkerPattern(Radiance.white*0.85f,Radiance.white*0.05f,0.5/4,0.5/4),
        AngleFunctions.simpleDiffuse,
        40, 5
      )

      import render.maths.{Vector3 => V3}

      val sphereTrans=InvGeoTransform.mix(V3(0.5,0.5,0.5),Rotation.Identity,V3(0,0.5,0))
      val sphere=new ComplexInstance(UnitSphere,sphereTrans,colorfulMat)

      val groundSize=4.0
      val lightSize = 1.0

      val lightTrans=InvGeoTransform.mix(V3(lightSize,1,lightSize),Rotation.Identity,V3(-lightSize/2,4,-lightSize/2))
      val lightSquare = new ComplexInstance(UnitSquare, lightTrans, lightMat)

      val groundTrans=InvGeoTransform.mix(V3(groundSize,1,groundSize),Rotation.Identity,V3(-groundSize/2,0,-groundSize/2))
      val ground=new ComplexInstance(UnitSquare,groundTrans,wallMatYellow)

      val wallUpTrans=InvGeoTransform.mix(V3(groundSize,1,groundSize),Rotation.Identity,V3(-groundSize/2,4.05,-groundSize/2))
      val wallUp=new ComplexInstance(UnitSquare,wallUpTrans,wallMatYellow)

      val wallRTrans=InvGeoTransform.mix(V3(4,1,4),Rotation.rotateZ(math.Pi/2),Vector3(2,0,-2))
      val wallR=new ComplexInstance(UnitSquare,wallRTrans,wallMatRed)

      val wallLTrans=InvGeoTransform.mix(V3(4,1,4),Rotation.rotateZ(math.Pi/2),Vector3(-2,0,-2))
      val wallL=new ComplexInstance(UnitSquare,wallLTrans,wallMatGreen)

      val wallBackTrans=InvGeoTransform.mix(V3(4,1,4),Rotation.rotateX(math.Pi/2),Vector3(-2,0,2))
      val wallBack=new ComplexInstance(UnitSquare,wallBackTrans,backgroundMat)

      val cylinderTrans=InvGeoTransform.mix(V3(1,2,1),Rotation.Identity,V3(2,-0.99,1))
      val cylinder=new ComplexInstance(Cylinder,cylinderTrans,colorfulMat)

      val pyramid=TriangularMeshes.pyramid.map(t=>new SimpleInstance(t,V3(2,-0.99,1),colorfulMat)).toList

      val coneLight=new ConeLight(V3.down*2.0+V3.right+V3.front,10,Radiance.white*15)
      val vertical=new ConeLight(V3(1,-1,1),20,Radiance.white*10)



      new RelaScene(List(sphere,ground ,wallL, wallR, wallUp, wallBack, lightSquare)/*++pyramid*/,List(),background)
    }

    override def camera: Camera = {
      new Camera(
        new StaticTransform(Vector3(0,2,-6),Vector3(0,2,0),0),
        Vector2(20,20),4)
    }
  }


}
