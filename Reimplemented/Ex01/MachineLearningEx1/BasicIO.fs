module BasicIO


open MathNet.Numerics.LinearAlgebra
open Common


// WARNING: CsvLogic could get more complicated. with escape characters, quotes inside quotes, etc.
type readCsvMatrix = {
        ColumnNames: seq<string>;
        DataMatrix: Matrix<double>
    }

let ReadLines filePath = System.IO.File.ReadLines(filePath)
let WriteLines filePath stringSeq
    = System.IO.File.WriteAllLines(filePath,stringSeq |> Seq.toArray)
/////////////////////////////////////
let ReadAndSplitBySeparator (separator: char) (filePath: string) =
    filePath
    |> ReadLines
    |> Seq.map (fun line -> line.Split(separator) |> Array.toSeq)
    
let ReadCsvString (filePath:string) =
    filePath
    |> ReadAndSplitBySeparator ','
let ReadTsvString (filePath:string) =
    filePath
    |> ReadAndSplitBySeparator '\t'
//////////////////////////////////////  
let SplitDataToHeaderAndDoubleMatrix (stringSeqSeq: seq<seq<string>>) =
    let header = 
        stringSeqSeq 
        |> Seq.head
    let dataMatrix = 
        stringSeqSeq 
        |> Seq.tail
        |> Seq.map (Seq.map (double))
        |> RowSeqSeqToMatrix
    (dataMatrix,header)
///////////////////////////////////////
let ReadCsvMatrixWithHeader (filePath:string) =
    filePath
    |> ReadAndSplitBySeparator ','
    |> SplitDataToHeaderAndDoubleMatrix
let ReadTsvDoubleTableWithHeader (filePath:string) =
    filePath
    |> ReadAndSplitBySeparator '\t'
    |> SplitDataToHeaderAndDoubleMatrix

//////////////////////////////////////////
let AddBackSeparator (separator: string) (stringRowSeqSeq: seq<seq<string>>) =
    stringRowSeqSeq
    |> Seq.map (String.concat separator)

let WriteCsvString (filePath:string) (stringRowSeqSeq: seq<seq<string>>) =
    stringRowSeqSeq
    |> AddBackSeparator ","
    |> WriteLines filePath
let WriteTsvString (filePath:string) (stringRowSeqSeq: seq<seq<string>>) =
    stringRowSeqSeq
    |> AddBackSeparator "\t"
    |> WriteLines filePath

////////////////////////////////////////////////
let MergeHeaderWithDataMatrix (columnNames : seq<string>) (dataMatrix: Matrix<double>) = 
    dataMatrix
    |> MatrixToRowSeqSeq
    |> Seq.map (Seq.map string)
    |> Seq.append ([|columnNames|])

let WriteCsvMatrix (filePath:string) (columnNames : seq<string>) (dataMatrix: Matrix<double>) = 
    dataMatrix
    |> MergeHeaderWithDataMatrix columnNames
    |> WriteCsvString filePath
let WriteTsvMatrix (filePath:string) (columnNames : seq<string>) (dataMatrix: Matrix<double>) =
    dataMatrix
    |> MergeHeaderWithDataMatrix columnNames
    |> WriteTsvString filePath
    
/////////////////////////////////////////////////
let WriteCsvSeq (filePath:string) (columnName: string) dataSeq =
    dataSeq
    |> Seq.map string
    |> Seq.append [|columnName|]
    |> WriteLines filePath