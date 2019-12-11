(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open System
open External
open System.Text

type Cell = Wall | Walkable

type maze =
    val W: int
    val H: int
    val WalkableW : int
    val WalkableH : int
    
    val mutable Cells: Cell[,] 

    new(w, h) =
        if (w < 3 || w % 2 = 0) then failwith "Maze: Width must be odd and bigger than 2"
        if (h < 3 || h % 2 = 0) then failwith "Maze: Heighh must be odd and bigger than 2"

        {  
            W = w 
            H = h
            WalkableW = w
            WalkableH = h
            Cells = Array2D.create w h Walkable 
        }

    member private __.generate = ()
     