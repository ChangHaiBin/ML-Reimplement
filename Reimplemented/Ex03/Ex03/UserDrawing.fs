module UserDrawing


open System.Drawing
open System
open System.Windows.Forms
open Problem03

// Learn more about F# at http://fsharp.net 

//specifies the memory location of the class files 

//that will be needed in our application 

open System.Collections.Generic 
open System 
open System.Windows.Forms 
open System.ComponentModel 
open System.Drawing
open System.Drawing.Drawing2D 

// https://www.c-sharpcorner.com/UploadFile/f50501/paint-application-in-windows-forms-application-using-fsharp/

let output =
    let drawingform = 
        new Form(
            Text="Paint Application",
            AutoScaleDimensions=new System.Drawing.SizeF(60.0F, 13.0F),
            ClientSize=new System.Drawing.Size(400, 400),
            StartPosition=FormStartPosition.CenterScreen) 
    //creates our control 
    let erasebutton=new Button(Text="Erase", Location=new System.Drawing.Point(100, 350)) 
    let predictbutton = new Button(Text="Predict", Location = new System.Drawing.Point(200,350))
    
    let drawInitialRect (e:PaintEventArgs) =
        e.Graphics.DrawRectangle(new Pen(Color.Red),new Rectangle(100,100,200,200))
    drawingform.Paint.Add(drawInitialRect)
    
    drawingform.Controls.Add(erasebutton) 
    drawingform.Controls.Add(predictbutton) 
    
    let gr=drawingform.CreateGraphics() 
    gr.SmoothingMode<-SmoothingMode.HighQuality 
    //when the form is loaded, change its color to white 
    drawingform.Load.Add(fun background-> 
    drawingform.BackColor<-Color.White) 
    
    let mutable word = 
        "1"
        
    let easyDictionary =
        Array.replicate 20 (
            Array.replicate 20 (false)
        )
    
    let mutable mutableList =
        List.empty
    let mutable mutableSet = 
        Set.empty

    drawingform.MouseMove.Add(fun trail-> 
    //when the mouse button is moved and the left button is clicked 

    if (trail.Button=System.Windows.Forms.MouseButtons.Left) 
        && 100 < trail.X  && trail.X < 300
        && 100 < trail.Y  && trail.Y < 300 then 
        //draw the object assign the color seleted from the color dialog as a brush color 
        let x = (trail.X - 100) / 10
        let y = (trail.Y - 100) / 10
        let newX = x * 10 + 100
        let newY = y * 10 + 100

        gr.FillRectangle(new SolidBrush(Color.Black),new Rectangle(newX,newY,10,10))
        mutableList <- (x,y) :: mutableList
        mutableSet <- Set.add (x,y) mutableSet
        printfn "%i" (mutableSet |> Set.count)
        ()
    else
        ()
    )
        

    //when the erase button is clicked 
    //erase the object drawn in the form 
    erasebutton.Click.Add(fun erase->
        gr.Clear(Color.White)
        gr.DrawRectangle(new Pen(Color.Red),new Rectangle(100,100,200,200))
        [|0 .. 19|] |> Seq.iter (fun x ->
        [|0 .. 19|] |> Seq.iter (fun y ->
            easyDictionary.[x].[y] <- false
        )
        )
        mutableList <- List.empty
        mutableSet <- Set.empty
    )
    //let qqq = System.Random()
    predictbutton.Click.Add(fun predict ->
        //let scores =
        //    easyDictionary
        //    |> Array.concat
        //word <-
        //    scores
        //    |> Array.map (fun x -> x / 100.0)
        //    |> PredictByNeuralNet
        //    |> string
        //word <- qqq.Next(0,9) |> string
        let sampleDictionary = 
            Array.replicate 20 (
                Array.replicate 20 (
                    false
                )
            )
        mutableSet 
        |> Set.iter (fun (x,y) ->
            sampleDictionary.[x].[y] <- true
        )
        let word2 =
            mutableSet
            |> Set.count
            |> string
        printfn "%s" word2
        gr.FillRectangle(new SolidBrush(Color.White),new Rectangle(300,320,100,100))
        gr.DrawString(word2,new Font("Times New Roman",12.0f),new SolidBrush(Color.Black),new PointF(300.0 |> float32, 350.0 |> float32))
        ()
    )
    
    drawingform

//executes our application 
//Application.Run(drawingform) 
output.Show()