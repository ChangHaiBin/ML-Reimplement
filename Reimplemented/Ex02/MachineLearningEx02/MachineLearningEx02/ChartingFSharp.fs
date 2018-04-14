module ChartingFSharp


open FSharp.Charting;
open MathNet.Numerics.LinearAlgebra
// https://fslab.org/FSharp.Charting/PointAndLineCharts.html

// Add reference to System.Drawing
// Add reference to Windows.Forms
// Add reference to DataVisualization

let SimpleTimeSeries ySeq =
    let yLength = ySeq |> Seq.length
    [1 .. yLength]
    |> List.map (double)
    |> Seq.zip <| ySeq
    |> Chart.Line
    |> fun result -> result.ShowChart()

module CreateChartObject =
    let CreateGraphForFunction (left: double) right diff f = 
        let numSegments =
            (right - left) / diff 
            |> int  
        [0 .. numSegments]    // lamppost error.
        |> List.map (double)
        |> List.map (fun n -> left + diff * n)
        |> List.map (fun x -> (x,f x))
        |> Chart.Line
        
    let CreateGraphForPoints (xySeq:seq<(double * double)>) =
        Chart.Point(xySeq)

    let CreateGraphForxSeqySeq (xSeq: seq<double>) (ySeq: seq<double>) =
        Seq.zip xSeq ySeq
        |> Chart.Point
        
    let CreateGraphForTwoColumnMatrix (xyMatrix: Matrix<double>) =
        if xyMatrix.ColumnCount <> 2 then failwith "expected 2 column Matrix."
        let xyColArrArr =
            xyMatrix
            |> Matrix.toColArrays
        let xArr =
            xyColArrArr.[0]
        let yArr = xyColArrArr.[1]
        CreateGraphForxSeqySeq xArr yArr
        
    let CreateGraphForSimpleTimeSeries ySeq =
        let yLength = ySeq |> Seq.length
        [1 .. yLength]
        |> List.map (double)
        |> Seq.zip <| ySeq
        |> Chart.Line

module AutoPlotting =
    let SimplePlotGraph (left: double) right diff f =
        CreateChartObject.CreateGraphForFunction (left: double) right diff f
        |> fun x -> x.ShowChart().Show()
    
    let SimplePlotPoints (xySeq:seq<(double * double)>) =
        let xSmall, xLarge =
            xySeq
            |> Seq.map (fst)
            |> fun xSeq -> (Seq.min xSeq, Seq.max xSeq)
        
        let ySmall, yLarge =
            xySeq
            |> Seq.map (snd)
            |> fun ySeq -> (Seq.min ySeq, Seq.max ySeq)

        let xMin = xSmall - 0.05 * (xLarge - xSmall)
        let xMax = xLarge + 0.05 * (xLarge - xSmall)
        let yMin = ySmall - 0.05 * (yLarge - ySmall)
        let yMax = yLarge + 0.05 * (yLarge - ySmall)
        Chart.Point(xySeq).WithXAxis(Max = xMax, Min = xMin).WithYAxis(Max = yMax, Min = yMin).ShowChart().Show()
        
    let SimplePlotxSeqySeq (xSeq: seq<double>) (ySeq: seq<double>) =
        Seq.zip xSeq ySeq
        |> SimplePlotPoints
     
    let SimplePlotTwoColumnMatrix (xyMatrix: Matrix<double>) =
        if xyMatrix.ColumnCount <> 2 then failwith "expected 2 column Matrix."
        let xyColArrArr =
            xyMatrix
            |> Matrix.toColArrays
        let xArr =
            xyColArrArr.[0]
        let yArr = xyColArrArr.[1]
        SimplePlotxSeqySeq xArr yArr
        
    let SimpleTimeSeries ySeq =
        CreateChartObject.CreateGraphForSimpleTimeSeries ySeq
        |> fun result -> result.ShowChart().Show()








