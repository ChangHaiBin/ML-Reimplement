module Problem03

open MathNet.Numerics.LinearAlgebra
open DrawDigits
open System

let directory =
    @"D:\Alternate Desktop\randomcrap\20180128Folder\Machine Learning\Reimplemented\Ex03\"

let filepath1 = 
    directory + "theta1.csv"

let filepath2 = 
    directory + "theta2.csv"

let filepath3 = 
    directory + "X.csv"

let filepath4 = 
    directory + "Y.csv"

let xMatrix = 
    filepath3
    |> BasicIO.ReadCsvString
    |> Seq.map (Seq.map (float))
    |> Matrix.Build.DenseOfRows 

let minShade = xMatrix |> Matrix.toSeq |> Seq.min
let maxShade = xMatrix |> Matrix.toSeq |> Seq.max
let normalize x = (x - minShade) / (maxShade - minShade)
    
let yResult =
    filepath4
    |> BasicIO.ReadCsvString
    |> Seq.map (Seq.exactlyOne)
    |> Seq.map (int)
    |> Seq.length

let Sigmoid x =
    1.0 / (1.0 + exp (- x))

let UnregularizedCost yArray thetaColumnMatrix =
    xMatrix * thetaColumnMatrix
    |> Matrix.toColArrays
    |> Array.exactlyOne
    |> Array.map Sigmoid
    |> Array.map2 (fun yInd htx ->
        - yInd * log(htx) - (1.0 * yInd) * log (1.0 - htx)
    ) yArray
    |> Array.average


let RegularizedCost yArray





let rnd = new System.Random()


[1 .. 10]
|> List.iter (fun _ ->
    xMatrix 
    |> Matrix.toRowSeq
    |> Seq.item (rnd.Next(0,5000))
    |> Vector.toSeq
    |> Seq.map normalize
    |> Seq.toList
    |> showDigit
    )
    