module Common

open System
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics

// f(_,_,_) <!> [x1;x2] = [f(x1,_,_); f(x2,_,_)]
//
// [f(x1,_,_); f(x2,_,_)] <%> [y1;y2] = [f(x1,y1,_);f(x2,y2,_)]

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

let Map2 f u v =
    f <!> u <%> v
let Map3 f u v w=
    f <!> u <%> v <%> w
let Map4 f u v w x=
    f <!> u <%> v <%> w <%> x
let Map5 f u v w x y=
    f <!> u <%> v <%> w <%> x <%> y
let Map6 f u v w x y z=
    f <!> u <%> v <%> w <%> x <%> y <%> z

/// "Double" pipe-forward
let inline (||>) (x,y) f =
    f x y
/// "Triple" pipe-forward
let inline (|||>) (x,y,z) f =
    f x y z

/// Will throw exception if sequence length are not the same.
let inline SumProduct xs ys = Map2 (*) xs ys |> Seq.sum
/// Will throw exception if sequence length are not the same.
let inline SumProduct3 xs ys zs = Map3 (fun x y z -> x * y * z) xs ys zs |> Seq.sum
/// Will throw exception if sequence length are not the same.
let inline SumBy2 f xs ys = Map2 f xs ys |> Seq.sum
/// Will throw exception if sequence length are not the same.
let inline SumBy3 f xs ys zs = Map3 f xs ys zs |> Seq.sum
/// Will throw exception if sequence length are not the same.
let inline SumBy4 f ws xs ys zs = Map4 f ws xs ys zs |> Seq.sum



    
/// Remove the first M elements, and last N elements from a seq.            
let SeqTrim M N xSeq=
    let length = Seq.length xSeq
    xSeq    
    |> Seq.skip M 
    |> Seq.take (length - M - N)

let SeqTakeLast N xSeq =
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


let RowSeqSeqToMatrix xSeqSeq =
    xSeqSeq
    |> Matrix.Build.DenseOfRows

let ColSeqSeqToMatrix xSeqSeq =
    xSeqSeq
    |> Matrix.Build.DenseOfColumns

let MatrixToRowSeqSeq xMatrix =
    xMatrix
    |> Matrix.toRowSeq
    |> Seq.map (Vector.toSeq)

let MatrixToColSeqSeq xMatrix =
    xMatrix
    |> Matrix.toColSeq
    |> Seq.map (Vector.toSeq)