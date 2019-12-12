(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open External
open Gfx

type CharInfo with
    /// Shortcut for creating a wall pixel.
    static member wall = pixel.create (Config.wall_pixel_char, Color.White)
    /// Shortcut for creating a path pixel.
    static member internal path = pixel.filled Color.Black
    /// Check whether this pixel is a wall.
    member this.isWall = this = pixel.wall


// TODO: implement the maze type, its generation (task 1) and its automatic resolution (task 2)
type maze (w, h) as this =

    // TODO: do not forget to call the generation function in your object initializer
    do this.generate

    // TODO: start with implementing the generation
    member private __.generate = ()
    