module Common

open System
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics

// f(_,_,_) <!> [x1;x2] = [f(x1,_,_); f(x2,_,_)]
//
// [f(x1,_,_); f(x2,_,_)] <%> [y1;y2] = [f(x1,y1,_);f(x2,y2,_)]

module General =
    let inline (<!>) f xSeq =
        Seq.map f xSeq

    let inline (<%>) fSeq xSeq =
        if Seq.length fSeq <> Seq.length xSeq then failwith "Seq length are different! Error in applying Map2"
        Seq.map2 (id) fSeq xSeq

    /// [f;g] <%%> [a;b;c] = [[f(a);f(b);f(c)];[g(a);g(b);g(c)]]
    let inline (<%%>) fSeq xSeq =
        Seq.map (fun f -> f <!> xSeq) fSeq

    /// [f;g] <%%%> [a;b;c] = [f(a);f(b);f(c);g(a);g(b);g(c)]
    let inline (<%%%>) fSeq xSeq =
        Seq.collect (fun f -> f <!> xSeq) fSeq

    /// [f;g] <%%%%> [a;b;c] = [f(a);g(a);f(b);g(b);f(c);g(c)]
    let inline (<%%%%>) fSeq xSeq =
        Seq.collect (fun x -> Seq.map (fun f -> f x) fSeq) xSeq
    
    /// [f;g] <%%%%%> [a;b;c] = [[f(a);g(a)];[f(b);g(b)];[f(c);g(c)]]
    let inline (<%%%%%>) fSeq xSeq =
        Seq.map (fun x -> Seq.map (fun f -> f x)) xSeq

    let map2 f u v =
        f <!> u <%> v
    let map3 f u v w=
        f <!> u <%> v <%> w
    let map4 f u v w x=
        f <!> u <%> v <%> w <%> x
    let map5 f u v w x y=
        f <!> u <%> v <%> w <%> x <%> y
    let map6 f u v w x y z=
        f <!> u <%> v <%> w <%> x <%> y <%> z

/// "Double" pipe-forward
let inline (||>) (x,y) f =
    f x y
/// "Triple" pipe-forward
let inline (|||>) (x,y,z) f =
    f x y z

module List =
    let inline sumProduct xs ys = List.map2 (*) xs ys |> List.sum


module Seq =
    /// Will throw exception if sequence length are not the same.
    let inline sumProduct xs ys = Seq.map2 (*) xs ys |> Seq.sum
    /// Will throw exception if sequence length are not the same.
    let inline sumProduct3 xs ys zs = Seq.map3 (fun x y z -> x * y * z) xs ys zs |> Seq.sum
    /// Will throw exception if sequence length are not the same.
    let inline sumBy2 f xs ys = Seq.map2 f xs ys |> Seq.sum
    /// Will throw exception if sequence length are not the same.
    let inline sumBy3 f xs ys zs = Seq.map3 f xs ys zs |> Seq.sum
    /// Will throw exception if sequence length are not the same.
    let inline sumBy4 f ws xs ys zs = General.map4 f ws xs ys zs |> Seq.sum
    /// Remove the first M elements, and last N elements from a seq.            
    let trim M N xSeq=
        let length = Seq.length xSeq
        xSeq    
        |> Seq.skip M 
        |> Seq.take (length - M - N)

    let takeLast N xSeq =
        let length = Seq.length xSeq
        xSeq |> Seq.skip (length - N)

module SeqSpecial =
    /// Estimates the unbiased population standard deviation from the provided samples. 
    ///
    /// On a dataset of size N will use an N-1 normalizer (Bessel's correction). Returns NaN if data has less than two entries or if any entry is NaN.
    let stdev (xSeq: seq<double>) =
        Statistics.StandardDeviation xSeq

        
module SeqSeq =
    let map f xSeqSeq =
        xSeqSeq
        |> Seq.map (fun xSeq ->
            xSeq 
            |> Seq.map f
        )


module RowSeqSeq =
    let toMatrix xSeqSeq =
        xSeqSeq
        |> Matrix.Build.DenseOfRows

module ColSeqSeq =
    let toMatrix xSeqSeq =
        xSeqSeq
        |> Matrix.Build.DenseOfColumns

module Matrix =
    let toRowSeqSeq xMatrix =
        xMatrix
        |> Matrix.toRowSeq
        |> Seq.map (Vector.toSeq)

    let toColSeqSeq xMatrix =
        xMatrix
        |> Matrix.toColSeq
        |> Seq.map (Vector.toSeq)

    let augmentMatrix xMatrix =
        let nRow = xMatrix |> Matrix.rowCount
        xMatrix
        |> toColSeqSeq 
        |> Seq.append [|Seq.replicate nRow 1.0|]
        |> ColSeqSeq.toMatrix
        
module Norm =
    let normalize01 (xList: List<float>) =
        let max = xList |> List.max
        let min = xList |> List.min
        xList
        |> List.map (fun x ->
            (x - min) / (max - min)
        )
        