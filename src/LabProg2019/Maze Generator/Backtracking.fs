module LabProg2019.MazeGenerator

open Maze
open System

let rand = Random(DateTime.Now.Millisecond)
type AlgorithmCell = Unvisited | Visited

let private is_out_of_bound (m: maze) (x: int) (y: int): bool =
    (x >= m.W || y >= m.H || x < 0 || y < 0)

let backtracking (m: maze): maze = 
    let fill_with_wall (m: maze): maze =
        let rec aux (x: int) (y: int): maze =
            if (x % 2 = 0 || x = m.W - 1 || y % 2 = 0 || y = m.H - 1) then 
                m.Cells.[x, y] <- Cell.Wall
            if (x = m.W - 1 && y = m.H - 1) then
                m
            else
                if (y = m.H - 1) then
                    aux (x+1) 0
                else
                    aux x (y+1)

        aux 0 0

    let mutable cells = Array2D.create m.W m.H Unvisited 
    let aux (x: int) (y: int) = ()
     
               

    fill_with_wall m

