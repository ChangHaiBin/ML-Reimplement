module Assignment1

open BasicIO
open MathNet.Numerics.LinearAlgebra
open Common
open ChartingFSharp
open MathNet.Numerics.LinearAlgebra.Solvers
open FSharp.Charting

let Section2 =

    let dataMatrix = 
        "C:\Users\chang\Desktop\ProgrammingTest\Machine Learning\Machine Learning FSharp\Machine Learning ex1\MachineLearningEx1\ex1data1.txt"
        |> ReadCsvString
        |> SeqSeq.map double
        |> RowSeqSeqToMatrix

    let xSeq = dataMatrix.Column(0) |> Vector.toSeq
    let ySeq =dataMatrix.Column(1) |> Vector.toSeq
    let dataLength = xSeq |> Seq.length |> double

    let xCol = [|xSeq|] |> ColSeqSeqToMatrix
    let yCol = [|ySeq|] |> ColSeqSeqToMatrix

    let augXMatrix =
        xCol 
        |> Matrix.map (fun _ -> 1.0)
        |> fun allOnesCol -> allOnesCol.Append(xCol)

    let scatterPlotWindowsForm = AutoPlotting.SimplePlotTwoColumnMatrix dataMatrix
    scatterPlotWindowsForm

    let CostComputingFunction (theta0:float,theta1 : float) =
        (xCol * theta1 + theta0) - yCol
        |> Matrix.map (fun z -> z ** 2.0)
        |> Matrix.sum
        |> fun z -> z / 2.0 / dataLength
    

    let Section_2_2_3 =
        CostComputingFunction (0.0, 0.0)

    let Section_2_2_4 =
        let alpha = 0.01
        let iterations = 1500
        let theta0_start, theta1_start = (0.0, 0.0)

        let UpdateFunction (theta0:double ,theta1: double) _ =
            let yColPredict =
                xCol * theta1 + theta0
            let thetaOldMatrix =
                [[theta0];[theta1]] |> matrix

            let thetaNewMatrix =
                augXMatrix.TransposeThisAndMultiply(yColPredict - yCol)
                |> (*) (alpha/dataLength)
                |> fun rhs -> thetaOldMatrix - rhs
            let (theta0_new, theta1_new) = (thetaNewMatrix.Item(0,0), thetaNewMatrix.Item(1,0))
            (theta0_new, theta1_new)
        
        let (theta0_end, theta1_end) =
            [1 .. iterations]
            |> List.fold (UpdateFunction) (theta0_start, theta1_start)
        (theta0_end, theta1_end)
        //CostComputingFunction (theta0_start, theta1_start)

    let (theta0_end, theta1_end) = Section_2_2_4

    let linearFunction x = x * theta1_end + theta0_end

    let cost = CostComputingFunction (theta0_end, theta1_end)

    let combinedChart = 
        Chart.Combine [
            CreateChartObject.CreateGraphForTwoColumnMatrix dataMatrix;
            CreateChartObject.CreateGraphForFunction 4.0 24.0 0.1 linearFunction
        ]
    combinedChart.WithXAxis(Min = 4.0, Max = 24.0).WithYAxis( Min = -5.0,Max=25.0).ShowChart().Show()

    ()

/////////////////////////////////////////////////////////////////////////////////////

// Section 3.1
let Normalize (xSeq: seq<double>) =
    let mean = Seq.average xSeq
    let stdev = (SeqSpecial.stdev) xSeq
    xSeq 
    |> Seq.map (fun x -> (x - mean) / stdev)

let NormalizeEachColumn (xMatrix: Matrix<double>) =
    xMatrix
    |> Matrix.mapCols (fun _ vect ->
        vect
        |> Vector.toSeq
        |> Normalize
        |> Vector.Build.DenseOfEnumerable
    )

let NormalizeEachColumn_Mean_Stdev (xMatrix:Matrix<double>) =
    let normalizedMatrix = NormalizeEachColumn xMatrix
    let colSeqSeq = 
        xMatrix 
        |> MatrixToColSeqSeq
    let meanSeq = Seq.map (Seq.average) colSeqSeq
    let stdevSeq = Seq.map (SeqSpecial.stdev) colSeqSeq
    normalizedMatrix, meanSeq, stdevSeq

/////// Section 3.2


let MultiLinearCostComputingFunction (xAugMatrix:Matrix<double>) yCol (thetaMatrix: Matrix<double>) =
    let nData = Matrix.rowCount yCol |> double
    xAugMatrix * thetaMatrix - yCol
    |> fun (diffCol: Matrix<double>) -> diffCol.TransposeThisAndMultiply(diffCol).[0,0]
    |> fun sumSquare -> sumSquare / (2.0 * nData)
    
let MultiLinearGradientDescent (xAugMatrix:Matrix<double>) (yCol:Matrix<double>) 
    (thetaCol_initial: Matrix<double>) (alpha:double) iteration =
    let nData = yCol |> Matrix.rowCount |> double

    [1 ..iteration]
    |> Seq.scan (fun (thetaCol_old:Matrix<double>) _ ->
        thetaCol_old - xAugMatrix.TransposeThisAndMultiply(xAugMatrix * thetaCol_old - yCol) * alpha / nData
    ) thetaCol_initial

let Section3 =
    let dataMatrix = 
        "C:\Users\chang\Desktop\ProgrammingTest\Machine Learning\Machine Learning FSharp\Machine Learning ex1\MachineLearningEx1\ex1data2.txt"
        |> ReadCsvString
        |> SeqSeq.map double
        |> RowSeqSeqToMatrix
    
    // NormalizeEachColumn_Mean_Stdev

    let yCol = 
        dataMatrix.Column(2)
        |> Vector.toSeq
        |> fun ySeq -> [|ySeq|]
        |> Matrix.Build.DenseOfColumns
    let xMatrix = dataMatrix.RemoveColumn(2)
    let xNormalized, meanSeq, stdevSeq = NormalizeEachColumn_Mean_Stdev xMatrix
    let xAugMatrix =
        let rowCount = Matrix.rowCount xNormalized
        let onesCol = Matrix.Build.Dense(rowCount,1,1.0)
        onesCol.Append(xNormalized)
    let theta_start = [[0.0];[0.0];[0.0]] |> matrix
    let alpha = 0.01
    let iterations = 400

    let theta_overTime =
        MultiLinearGradientDescent xAugMatrix yCol theta_start alpha iterations

    let PlotCostOverIteration  = 
        theta_overTime
        |> Seq.tail
        |> Seq.map (MultiLinearCostComputingFunction xAugMatrix yCol)
        |> AutoPlotting.SimpleTimeSeries

    let exactSolution =
        (xAugMatrix.TransposeThisAndMultiply(xAugMatrix).Inverse()) * (xAugMatrix.TransposeThisAndMultiply(yCol))
        
    ()
    









