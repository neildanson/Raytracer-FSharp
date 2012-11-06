open Raytracer
open System.Diagnostics

let width = 1280
let height = 720

type ShinySurface() = 
    interface Surface with
        member surface.Diffuse pos = Color.RGB(1.0,1.0,1.0)
        member surface.Specular pos =  Color.RGB(0.5, 0.5, 0.5)
        member surface.Reflect pos = 0.3
        member surface.Roughness = 50.0
    
let shiny = ShinySurface()

let scene = { Lights = [ Light(Vector3D(4.0,3.0,0.0), Color.RGB(0.2 ,0.2, 0.2));
                          Light(Vector3D(4.0,5.0,2.0), Color.RGB(0.5 ,0.5, 0.5)); ]; 
              Objects = [Sphere(Vector3D(0.0, 1.0, 5.0), 1.0, shiny);
                          Sphere( Vector3D(2.0, 1.0, 5.0), 1.0, shiny);
                          Plane(Vector3D(0.0, 1.0, 0.0), -1.1, shiny) ]; 
              Camera = Camera( Vector3D(0.0, 0.0,-3.0), Vector3D(0.0, 0.0, 100.0 )) } 

let raytracer = Raytracer(width, height, scene)

let sw = Stopwatch()
sw.Start()
let result = raytracer.Render()
sw.Stop()

open System.Drawing
open System.Runtime.InteropServices

let bm = new Bitmap(width, height)

let data = bm.LockBits(Rectangle(0,0,width, height), Imaging.ImageLockMode.WriteOnly, Imaging.PixelFormat.Format32bppArgb)
Marshal.Copy(result, 0,data.Scan0, result|>Array.length)

bm.UnlockBits(data)

bm.Save("fsresult.bmp", Imaging.ImageFormat.Bmp)

printf "Elapsed Time %s" (sw.Elapsed.ToString())

System.Console.ReadLine()|>ignore