(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open System
open System.Text
open Gfx

type Cell = Wall | Walkable

type maze =
    val Width: int
    val Height: int
    val mutable Start_x: int
    val mutable Start_y: int
    val mutable End_x: int
    val mutable End_y: int

    val mutable Pixel_bg: pixel
    val mutable Pixel_wall: pixel
    val mutable Pixel_start: pixel
    val mutable Pixel_end: pixel
    
    val mutable Cells: Cell[,]

    new((w: int), (h: int), (generator: (maze -> unit))) as this =
        if (w < 3 || w % 2 = 0) then failwith "Maze: Width must be odd and bigger than 2"
        if (h < 3 || h % 2 = 0) then failwith "Maze: Heighh must be odd and bigger than 2"

        {  
            Width = w 
            Height = h
            Start_x = 1
            Start_y = 1
            End_x = w - 2
            End_y = h - 2
            Pixel_bg = pixel.filled Color.Red
            Pixel_wall = pixel.filled Color.White
            Pixel_start = pixel.filled Color.Green
            Pixel_end = pixel.filled Color.Yellow
            Cells = Array2D.create w h Walkable 
        }
        then
            generator(this)

    member this.is_start_pos (pos: int*int): bool =
        let (x, y) = pos
        (x = this.Start_x && y = this.Start_y)

    member this.is_end_pos (pos: int*int): bool =
        let (x, y) = pos
        (x = this.End_x && y = this.End_y)

    member this.to_image (): image =
        let i = new image (this.Width, this.Height)
        this.draw i
        i

    member this.draw (i: image): unit =
        for x in 0 .. this.Width-1  do
            for y in 0 .. this.Height-1  do
                i.plot(x, y, this.get_pixel_in_pos x y)

    member this.get_pixel_in_pos (x: int) (y: int): pixel =
        if this.is_start_pos (x, y) then
            this.Pixel_start
        else if this.is_end_pos (x, y) then
            this.Pixel_end
        else
            match this.Cells.[x, y] with
            | Cell.Walkable -> this.Pixel_bg
            | Cell.Wall -> this.Pixel_wall
