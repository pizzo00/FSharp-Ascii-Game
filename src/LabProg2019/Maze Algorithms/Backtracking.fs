module LabProg2019.Backtracking

open Maze
open MazeGeneratorUtility
open System
open System.Linq

let backtracking (m: maze) = 
    let mutable cells = Array2D.create m.Width m.Height Unvisited 

    let get_random_new_pos (m: maze) (cells: AlgorithmCell[,]) (pos: int*int): (int*int) option =
        let mutable possible_choices = []
        let (x, y) = pos
        if is_valid_pos m (x+2, y)   && cells.[x+2, y]   = Unvisited then possible_choices <- (x+2, y)  ::possible_choices
        if is_valid_pos m (x-2, y)   && cells.[x-2, y]   = Unvisited then possible_choices <- (x-2, y)  ::possible_choices
        if is_valid_pos m (x,   y+2) && cells.[x,   y+2] = Unvisited then possible_choices <- (x,   y+2)::possible_choices
        if is_valid_pos m (x,   y-2) && cells.[x,   y-2] = Unvisited then possible_choices <- (x,   y-2)::possible_choices
        if possible_choices.Count() = 0 || m.is_end_pos (x, y) then //if no possible way or reach the end (This prevent maze to put another dead end after end)
            None
        else
            Some (get_rand_element(possible_choices));

    let rec aux (m: maze)(x: int) (y: int) =
        let mutable newPos = get_random_new_pos m cells (x, y)
        while not newPos.IsNone do
            let (newX, newY) = newPos.Value
            set_walkable_between_coordinates m x y newX newY
            cells.[newX, newY] <- Visited
            aux m newX newY
            newPos <- get_random_new_pos m cells (x, y)              

    cells.[m.Start_x, m.Start_y] <- Visited
    m.Cells.[m.Start_x, m.Start_y] <- Walkable
    fill_with_wall m
    aux m m.Start_x m.Start_y