module Problem03

open MathNet.Numerics.LinearAlgebra
open DrawDigits
open System
open Common

let directory =
    @"D:\Alternate Desktop\Machine Learning\ML-Reimplement\Reimplemented\Ex03\"

let filepath1 = 
    directory + "theta1.csv"

let filepath2 = 
    directory + "theta2.csv"

let filepath3 = 
    directory + "X.csv"

let filepath4 = 
    directory + "Y.csv"


let xAugMatrix =
    let xMatrix = 
        filepath3
        |> BasicIO.ReadCsvString
        |> Seq.map (Seq.map (float))
        |> Matrix.Build.DenseOfRows 
    let rowCount = 
        xMatrix
        |> Matrix.rowCount
    xMatrix
    |> Matrix.toColArrays
    |> Array.append [|Array.replicate rowCount 1.0|]
    |> Matrix.Build.DenseOfColumnArrays

let yResult =
    filepath4
    |> BasicIO.ReadCsvString
    |> Seq.map (Seq.exactlyOne)
    |> Seq.map (int)

let yArray1 =
    yResult
    |> Seq.map (fun x ->
        if x = 1 then 1.0 else 0.0
    )
    |> Seq.toArray

let Sigmoid x =
    1.0 / (1.0 + exp (- x))

let UnregularizedCost yArray (xAugMatrix:Matrix<float>) thetaColumnMatrix =
    xAugMatrix * thetaColumnMatrix
    |> Matrix.toColArrays
    |> Array.exactlyOne
    |> Array.map Sigmoid
    |> Array.map2 (fun yInd htx ->
        - yInd * log(htx) - (1.0 * yInd) * log (1.0 - htx)
    ) yArray
    |> Array.average

let theta0 =
    let numCols = 
        xAugMatrix 
        |> Matrix.columnCount
    [|Array.replicate numCols 0.0|]
    |> Matrix.Build.DenseOfColumnArrays
let result1 = UnregularizedCost yArray1 xAugMatrix theta0


let RegularizedCost lambda yArray xAugMatrix thetaColumnMatrix =
    let m = 
        xAugMatrix
        |> Matrix.rowCount

    thetaColumnMatrix
    |> Matrix.toColArrays
    |> Array.exactlyOne
    |> Array.tail
    |> Array.map (fun x -> x * x)
    |> Array.sum
    |> (*) (lambda / 2.0 / (float m))
    |> (+) (UnregularizedCost yArray xAugMatrix thetaColumnMatrix)

let UnregularizedGradient yArray xAugMatrix thetaColumnMatrix =
    let htxy =
        let yColumnMatrix =
            [|yArray|]
            |> Matrix.Build.DenseOfColumnArrays
        xAugMatrix * thetaColumnMatrix - yColumnMatrix
    let m =
        xAugMatrix
        |> Matrix.rowCount
        |> float
    (1.0 / m) * xAugMatrix.Transpose() * htxy

let RegularizedGradient lambda yArray xAugMatrix thetaColumnMatrix =
    let matrixCopy =
        thetaColumnMatrix |> Matrix.Build.DenseOfMatrix
    // MUTABLE!!!!!
    matrixCopy.[0,0] <- 0.0
    let m =
        xAugMatrix
        |> Matrix.rowCount
        |> float
    (lambda / m) * matrixCopy
    |> (+) (UnregularizedGradient yArray xAugMatrix thetaColumnMatrix)

let hint =
    [|0.0;-3.32244; -3.2533; -4.99246; -2.41864; 0.611636; -3.63388; -2.00936; -8.40921; -5.36087; -6.11075|]
let trainParameter n =
    let yArray =
        yResult
        |> Seq.map (fun i ->
            if i = n then 1.0 else 0.0
        )
        |> Seq.toArray
    
    let theta0 =
        let numCols = 
            xAugMatrix 
            |> Matrix.columnCount
        let kickStart =
            hint.[n]
        (kickStart+ 10.0) :: (List.replicate (numCols - 1) 10.0)

    let f thetaList =
        thetaList
        |> List.toArray
        |> Array.map (fun x -> x - 10.0)
        |> fun column -> [|column|]
        |> Matrix.Build.DenseOfColumnArrays
        |> UnregularizedCost yArray xAugMatrix
    
    NumericalSolver.NelderMeadFindMinimum f theta0
    |> Seq.map (fun x -> x - 10.0)
    |> Seq.toArray
    |> fun column -> [|column|]
    |> Matrix.Build.DenseOfColumnArrays

let trainedParameter10 =
    [1 .. 10]
    |> List.map trainParameter
    
let testing =
    trainedParameter10
    |> List.map (fun thetaParameter ->
        xAugMatrix * thetaParameter
        |> Matrix.map Sigmoid
        |> Matrix.toColArrays
        |> Array.exactlyOne
        |> Array.toList
    )
    |> Seq.transpose
    |> Seq.map (fun probSeq ->
        probSeq 
        |> Seq.toList 
        |> List.zip [1 .. 10]
        |> List.maxBy snd
        |> fst
    )
    |> Seq.zip yResult
    |> Seq.filter (fun (x,y) -> x = y)
    |> Seq.length


// xMatrix range already contains 1.0. Adding a column of ones will not hurt.
let minShade = xAugMatrix |> Matrix.toSeq |> Seq.min
let maxShade = xAugMatrix |> Matrix.toSeq |> Seq.max
let normalize x = (maxShade - x) / (maxShade - minShade)
let rnd = new System.Random()


[1 .. 10]
|> List.iter (fun _ ->
    xAugMatrix 
    |> Matrix.toColArrays
    |> Array.tail
    |> Matrix.Build.DenseOfColumnArrays
    |> Matrix.toRowSeq
    |> Seq.item (rnd.Next(0,5000))
    |> Vector.toSeq
    |> Seq.map normalize
    |> Seq.toList
    |> showDigit
    )
    

//////////////////////////////////////////////////////////////////////////////////////

let theta1 =
    filepath1
    |> BasicIO.ReadCsvString
    |> Seq.map (Seq.map (float))
    |> Matrix.Build.DenseOfRows 
// 25 x 401

let theta2 =
    filepath2
    |> BasicIO.ReadCsvString
    |> Seq.map (Seq.map (float))
    |> Matrix.Build.DenseOfRows 
// 10 x 26

let predictionByNeuralNet =
    xAugMatrix * theta1.Transpose() 
    |> Matrix.map (Sigmoid)
    |> Matrix.augmentMatrix
    |> (*) <| theta2.Transpose()
    |> Matrix.map (Sigmoid)
    |> Matrix.toRowSeqSeq
    |> Seq.map (fun xSeq ->
        xSeq 
        |> Seq.mapi (fun i x -> (i+1,x))
        |> Seq.maxBy snd
        |> fst
    )
    |> Seq.toArray
    
let result = 
    predictionByNeuralNet
    |> Array.map2 (fun x y -> x = y) (Seq.toArray yResult)
    |> Array.length
    