namespace Raytracer

open System.Threading

type Raytracer(width, height, scene:Scene) =
    let screenWidth = width
    let screenHeight =height
    let inverseAspectRatio = float height / float width
    let linearScreenBuffer = Array.create (width * height) 0
    
    let fwidth = float width * inverseAspectRatio
    let (halfHeight, halfWidth) = (float height / 2.0, float fwidth / 2.0)
    let (invDoubleHeight, invDoubleWidth) = (1.0 / float height * 2.0, 1.0 / float fwidth * 2.0)

    let intersections ray = 
        List.fold (fun currentMin (head:SceneObject) ->
            let isect = head.Intersects ray
            match currentMin, isect with
            | None, None -> None
            | None, Some(isect) -> Some(head, isect)
            | Some(o,currentMin), None -> Some(o, currentMin)
            | Some(o,currentMin:Intersection), Some(isect) ->  
                Some(if currentMin.Dist < isect.Dist then o,currentMin else head, isect)
        ) None (scene.Objects)
        

    //Get our sorted list of intersections and if theres any data pick that - otherwise return none
    let testRay ray = 
        let intersections = intersections ray
        match intersections with
        | Some(_, isect) -> isect.Dist
        | None -> 0.0
                 
    let getNaturalColor(sceneObject : SceneObject, position, normal, rd) = 
        let computeColorForLight (light : Light) = 
            let ldis = light.Position - position
            let livec = Vector3D.Normalize ldis
            let neatIsect = testRay (Ray(position, livec))
            let isInShadow = ((neatIsect > Vector3D.Mag(ldis)) || (neatIsect = 0.0))
            if isInShadow = true then
                let illum = Vector3D.Dot(livec, normal)
                let lcolor = if illum > 0.0 then light.Color * illum else Color.RGB(0.0,0.0,0.0)
                let specular = Vector3D.Dot(livec, Vector3D.Normalize(rd))
                
                let scolor = if specular > 0.0 then light.Color * System.Math.Pow(specular, sceneObject.Surface.Roughness)
                                             else Color.RGB(0.0,0.0,0.0)
                (sceneObject.Surface.Diffuse(position)* lcolor)+(sceneObject.Surface.Specular(position)* scolor)
            else
                Color.get_Zero()
        
        scene.Lights|>Seq.map (fun light-> computeColorForLight light)|>Seq.sum
        
    let rec traceRay(ray, scene, depth :int) =
        match intersections ray with
        | Some(sceneObject, isect) -> shade(sceneObject, isect, depth) 
        | None -> Color.get_Zero()
      
    and getReflectionColor (sceneObject : SceneObject, pos, norm, rd, depth) = 
        traceRay(Ray(pos,rd), scene, depth + 1) * sceneObject.Surface.Reflect(pos)
         

    and shade (sceneObject : SceneObject, isect:Intersection, depth) =
        let d = isect.Ray.Direction
        let pos = (d * isect.Dist)+isect.Ray.Position
        let normal = sceneObject.Normal(pos)
        let reflectDir = d - (normal * (Vector3D.Dot(normal, d) * 2.0))
        
        let natural = getNaturalColor(sceneObject, pos, normal, reflectDir)
        match depth with
        | depth when depth < 3 -> natural + getReflectionColor(sceneObject, pos + (reflectDir * 0.001), normal, reflectDir, depth)
        | _ -> natural + Color.RGB(0.5,0.5,0.5)
        
    let recenterX (x : int) = 
        let dx = float x 
        (dx - halfWidth) * (invDoubleWidth)

    let recenterY (y : int) = 
        let dy = float y 
        let height = float height
        -(dy - (halfHeight)) * (invDoubleHeight)

    let getPoint (x,y, camera:Camera) = 
        Vector3D.Normalize(camera.Forward + ((camera.Right * recenterX(x))+(camera.Up * recenterY(y))))
        
    member this.Render() = 
        let traceray y = 
            let basex = y*width
            for x in 0..width-1 do 
                let c = basex+x
                linearScreenBuffer.[c]<-traceRay(
                     Ray(scene.Camera.Position, getPoint(x,y, scene.Camera)),
                     scene, 0).ToColor()

        let tracerayasync y = async { traceray y }
            
        [|for y in 0..(height - 1) -> tracerayasync y|]|>Async.Parallel|>Async.RunSynchronously|>ignore
        linearScreenBuffer