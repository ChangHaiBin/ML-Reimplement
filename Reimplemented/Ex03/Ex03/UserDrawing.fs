module UserDrawing


open System.Drawing
open System
open System.Windows.Forms

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
    let erasebutton=new Button(Text="Erase", Location=new System.Drawing.Point(170, 350)) 
    
    let drawInitialRect (e:PaintEventArgs) =
        e.Graphics.DrawRectangle(new Pen(Color.Red),new Rectangle(100,100,200,200))
    drawingform.Paint.Add(drawInitialRect)

    drawingform.Controls.Add(erasebutton) 
    
    let gr=drawingform.CreateGraphics() 
    gr.SmoothingMode<-SmoothingMode.HighQuality 
    //when the form is loaded, change its color to white 
    drawingform.Load.Add(fun background-> 
    drawingform.BackColor<-Color.White) 
    
    
    drawingform.MouseMove.Add(fun trail-> 
    //when the mouse button is moved and the left button is clicked 

    
    
    if (trail.Button=System.Windows.Forms.MouseButtons.Left) 
        && trail.X > 110 
        && trail.X < 290
        && trail.Y > 110
        && trail.Y < 290 then 
    //draw the object assign the color seleted from the color dialog as a brush color 
    gr.FillRectangle(new SolidBrush(Color.Black),new Rectangle(trail.X - 5,trail.Y - 5,10,10)))  

    //when the erase button is clicked 
    //erase the object drawn in the form 
    erasebutton.Click.Add(fun erase->
        gr.Clear(Color.White)
        gr.DrawRectangle(new Pen(Color.Red),new Rectangle(100,100,200,200))
    )  
    
    drawingform

//executes our application 
//Application.Run(drawingform) 
output.Show()