module NumericalSolver

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra

let NelderMeadFindMinimum f startList =
    let convertedFunction =
        new System.Func<Vector<float>,float>(Vector.toList >> f)
    let startVector =
        startList
        |> List.toSeq
        |> Vector.Build.DenseOfEnumerable
    MathNet.Numerics.FindMinimum.OfFunction(convertedFunction,startVector,1e-10,1000000)
    |> Vector.toSeq
    
let NelderMeadFindMinimumRefined f startList =
    let convertedFunction =
        new System.Func<Vector<float>,float>(Vector.toList >> f)
    let startVector =
        startList
        |> List.toSeq
        |> Vector.Build.DenseOfEnumerable
    MathNet.Numerics.FindMinimum.OfFunction(convertedFunction,startVector,1e-15,1000000)
    |> Vector.toSeq