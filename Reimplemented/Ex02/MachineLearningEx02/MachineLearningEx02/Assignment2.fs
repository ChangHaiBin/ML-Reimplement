module Assignment2

open BasicIO
open ChartingFSharp
open ChartingFSharp.CreateChartObject
open NumericalSolver

open FSharp.Charting
open Common

type Points =
    {
        X: double;
        Y: double;
        Indicator: int;
    }

// Section 1.1
let filePath =
    @"D:\Alternate Desktop\randomcrap\20180128Folder\Machine Learning ReImplemented\MachineLearning Reimplement\MachineLearningEx02\ex2data1.txt"
let data =
    filePath
    |> ReadCsvString 
    |> Seq.map (fun dataSeq ->
        dataSeq
        |> Seq.toList
        |> function 
        | [xString;yString;indicatorString] ->
            {
                X = xString |> double;
                Y = yString |> double;
                Indicator = indicatorString |> int
            }
        | _ -> failwith "Data not as formatted"
    )
let graph1 = 
    data
    |> Seq.filter (fun x -> x.Indicator = 1)
    |> Seq.map (fun x -> (x.X,x.Y))
    |> CreateChartObject.CreateGraphForPoints
let graph2 = 
    data
    |> Seq.filter (fun x -> x.Indicator = 0)
    |> Seq.map (fun x -> (x.X,x.Y))
    |> CreateChartObject.CreateGraphForPoints
let plotGraph1 =
    Chart.Combine ([graph1;graph2])
    |> Chart.WithXAxis(Min=30.0, Max=100.0)
    |> Chart.WithYAxis(Min=30.0, Max = 100.0)
    |> fun x -> x.ShowChart()
    |> ignore
    ()
    
////////////////////////////////////////////////////////////////////////////////////////  
let inline DivBy x y = y / x
let Sigmoid x =
    1.0 / (1.0 + exp (-x))
let H_theta thetaList xSeq =
    let [theta0;theta1;theta2] = thetaList 

    xSeq
    |> Seq.map2 (*) [|theta1; theta2|]
    |> Seq.sum
    |> (+) theta0
    |> Sigmoid

let CostFunction thetaList xSeqSeq ySeq =
    let dataLength = xSeqSeq |> Seq.length |> double
    ySeq
    |> Seq.map2 (fun xSeq y ->
        let h_theta_x = H_theta thetaList xSeq

        -y * log (h_theta_x)
        - (1.0 - y) * log(1.0 - h_theta_x)
    ) xSeqSeq
    |> Seq.sum
    |> DivBy dataLength

let CostFirstDiff thetaList xSeqSeq ySeq =
    let dataLength = xSeqSeq |> Seq.length |> double
    ySeq
    |> Seq.map2 (fun xSeq y ->
        let h_theta_x = H_theta thetaList xSeq
            
        xSeq
        |> Seq.map ((*) (h_theta_x - y))
    ) xSeqSeq
    |> Seq.reduce (Seq.map2 (+))
    |> Seq.map (DivBy dataLength)
let testCostFunction =
    CostFunction [0.0;0.0;0.0] (data |> Seq.map (fun a -> [|a.X;a.Y|])) 
        (data |> Seq.map (fun a -> a.Indicator |> double))
        // As expected, around 0.69314 when theta = [|0.0;0.0;0.0|]


let minimizedPoint =
    let f thetaList = 
        CostFunction (thetaList) (data |> Seq.map (fun a -> [|a.X;a.Y|])) (data |> Seq.map (fun a -> a.Indicator |> double))
    NelderMeadFindMinimum f [-25.16; 0.20; 0.20]
// [|-25.16144992; 0.2062328081; 0.2014727066|]
let costFunctionAtMinimizingPoint =
    CostFunction (minimizedPoint|> Seq.toList) (data |> Seq.map (fun a -> [|a.X;a.Y|])) (data |> Seq.map (fun a -> a.Indicator |> double))




    
// TODO: use minimizing function to find:
// [-25.161272; 0.206233; 0.201470]
let predictionResult = 
    data
    |> Seq.map (fun x -> [x.X; x.Y])
    |> Seq.map (H_theta [-25.161272; 0.206233; 0.201470] )
    |> Seq.map (fun x -> if x < 0.5 then 0 else 1)
    |> Seq.map2 (fun data predictedIndicator -> data.Indicator = predictedIndicator ) data
    |> Seq.filter (id)
    |> Seq.length
let graph3 =
    let xMin = 
        data
        |> Seq.map (fun x -> x.X)
        |> Seq.min
    let xMax = 
        data
        |> Seq.map (fun x -> x.X)
        |> Seq.max
    let diff = (xMax - xMin) / 1000.0
    [|xMin ..  diff .. xMax|]
    |> Seq.map (fun x -> 
        match minimizedPoint |> Seq.toArray with
        | [|c; xSlope; ySlope|] ->
            (x, (-c - xSlope * x) / ySlope)
        | _ -> failwith "WTF"
    )
    |> CreateChartObject.CreateGraphForPoints
let plotGraph2 = 
    Chart.Combine ([graph1;graph2;graph3])
    |> Chart.WithXAxis(Min=30.0, Max=100.0)
    |> Chart.WithYAxis(Min=30.0, Max = 100.0)
    |> fun x -> x.ShowChart()

///////////////////////////////////////////////////////////////////////////////////////
    
let filePath2 =
    @"D:\Alternate Desktop\randomcrap\20180128Folder\Machine Learning ReImplemented\MachineLearning Reimplement\MachineLearningEx02\ex2data2.txt"
let data2 =
    filePath2
    |> ReadCsvString 
    |> Seq.map (fun dataSeq ->
        dataSeq
        |> Seq.toList
        |> function 
        | [xString;yString;indicatorString] ->
            {
                X = xString |> double;
                Y = yString |> double;
                Indicator = indicatorString |> int
            }
        | _ -> failwith "Data not as formatted"
    )
    |> Seq.toList

    
let graph11 = 
    data2
    |> Seq.filter (fun x -> x.Indicator = 1)
    |> Seq.map (fun x -> (x.X,x.Y))
    |> CreateChartObject.CreateGraphForPoints
let graph12 = 
    data2
    |> Seq.filter (fun x -> x.Indicator = 0)
    |> Seq.map (fun x -> (x.X,x.Y))
    |> CreateChartObject.CreateGraphForPoints
    
let plotGraph3 = 
    Chart.Combine ([graph11;graph12])
    |> Chart.WithXAxis(Min= -1.0, Max=1.5)
    |> Chart.WithYAxis(Min= -0.8, Max = 1.2)
    |> fun x -> x.ShowChart()

    
let mapFeature (x:Points) =
    [0 .. 6] |> List.collect (fun i -> 
    [0 .. i] |> List.map (fun j ->
        (x.X ** (float (i - j))) * (x.Y ** (float j))
    )
    )

let mapFeatureXY x y =
    [0 .. 6] |> List.collect (fun i -> 
    [0 .. i] |> List.map (fun j ->
        (x ** (float (i - j))) * (y ** (float j))
    )
    )

let costFunction (lambda:float) (thetaList: List<float>) (data: List<Points>) =
    let m = thetaList |> List.length |> float
    let Sigmoid x =
        1.0 / (1.0 + exp (-x))
        
    data2
    |> List.map (fun x -> 
        let y = x.Indicator |> float
            
        x
        |> mapFeature 
        |> List.sumProduct thetaList
        |> Sigmoid
        |> fun ht_x ->
            -y * log(ht_x) - (1.0 - y) * log(1.0 - ht_x)
    )
    |> List.average
    |> (+) (lambda / 2.0 / m * (thetaList |> List.sumBy (fun x -> x * x)))



    
let test1 = costFunction 0.0 (List.replicate 28 0.0) data2

let lambda = 1.0
// Change to 0.0 and 100.0 for Case 2 and Case 3
let f thetaList =
    costFunction 100.0 thetaList data2

let thetaMinimized =
    NelderMeadFindMinimum f (List.replicate 28 0.0)
    |> Seq.toList

let thetaFurthurMinimized =
    NelderMeadFindMinimum f (thetaMinimized)
    |> Seq.toList

let manualPoints =
    List.allPairs [-1.0 .. 0.05 .. 1.5] [-0.8 .. 0.05 .. 1.2]
    |> List.map (fun (x,y) ->
        mapFeatureXY x y
        |> List.sumProduct thetaFurthurMinimized
        |> fun score -> (x,y,score)
    )

let region1 = 
    manualPoints
    |> List.filter (fun (x,y,score) -> score >= 0.0)
    |> List.map (fun (x,y,_) -> (x,y))
    |> CreateChartObject.CreateGraphForPoints

let region2 = 
    manualPoints
    |> List.filter (fun (x,y,score) -> score < 0.0)
    |> List.map (fun (x,y,_) -> (x,y))
    |> CreateChartObject.CreateGraphForPoints


let plotGraph4 = 
    Chart.Combine ([graph11;graph12;region2])
    |> Chart.WithXAxis(Min= -1.0, Max=1.5)
    |> Chart.WithYAxis(Min= -0.8, Max = 1.2)
    |> fun x -> x.ShowChart()


    