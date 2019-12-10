module LabProg2019.MazeGenerator

open Maze

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

    fill_with_wall m

