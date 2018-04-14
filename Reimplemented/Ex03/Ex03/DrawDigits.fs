module DrawDigits

open System.Drawing
open System
open System.Windows.Forms

let intToHex (i:int) = 
    let pad = if (i < 16) then "0" else ""
    pad + String.Format("{00:X}",i)
    
let hexToInt xString = 
    Int32.Parse(xString,System.Globalization.NumberStyles.HexNumber)
    
let drawRect (brush:SolidBrush) (rect:Rectangle) (e:PaintEventArgs) =
    e.Graphics.FillRectangle(brush,rect)
    
let showDigit (rangeList: List<float>) =
    let size = new Size(250,250)
    let newForm = new Form(MaximizeBox = true, Text = "Exercise", Size = size)
    if (rangeList |> List.length ) <> 400 then failwith "Expected 20 x 20 pixels"
    rangeList
    |> List.chunkBySize 20
    |> List.mapi (fun i xList ->
        xList
        |> List.mapi (fun j value ->
            (i,j,value)
        )
    )
    |> List.concat
    |> List.iter (fun (i,j,value) ->
        let rgbValue = 
            value * 255.0
            |> int
        let color = Color.FromArgb(rgbValue,rgbValue,rgbValue)
        let brush = new SolidBrush(color)
        let rect = new Rectangle(i*10, j*10, 10, 10)
        newForm.Paint.Add(drawRect brush rect)
    )
    newForm.Show()
    ()
    