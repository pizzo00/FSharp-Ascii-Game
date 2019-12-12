module LabProg2019.MazeGenerator

open Maze
open System
open System.Linq

let rand = Random(DateTime.Now.Millisecond)
type AlgorithmCell = Unvisited | Visited

let private is_valid_pos (m: maze) (pos: int*int): bool =
    let (x, y) = pos
    (x < m.Width && y < m.Height && x >= 0 && y >= 0) && //No out of bound
    (x % 2 = 1 || y % 2 = 1) //No on wall grid

let private get_random_new_pos (m: maze) (cells: AlgorithmCell[,]) (pos: int*int): (int*int) option =
    let mutable possible_choice = []
    let (x, y) = pos
    if is_valid_pos m (x+2, y)   && cells.[x+2, y]   = Unvisited then possible_choice <- (x+2, y)  ::possible_choice
    if is_valid_pos m (x-2, y)   && cells.[x-2, y]   = Unvisited then possible_choice <- (x-2, y)  ::possible_choice
    if is_valid_pos m (x,   y+2) && cells.[x,   y+2] = Unvisited then possible_choice <- (x,   y+2)::possible_choice
    if is_valid_pos m (x,   y-2) && cells.[x,   y-2] = Unvisited then possible_choice <- (x,   y-2)::possible_choice
    let choice_count = possible_choice.Count()
    if choice_count = 0 || m.is_end_pos x y then //if no possible way or reach the end (This prevent maze to put another dead end after end)
        None
    else
        Some (possible_choice.[rand.Next(0, choice_count)]);

let private set_walkable_between (m: maze) (x0: int) (y0: int) (x1: int) (y1: int): unit =
    let mutable x = -1
    let mutable y = -1

    if (x0 = x1) then 
        x <- x0
        y <- y0 + (y1-y0)/2
    else
        if (y0 = y1) then
            x <- x0 + (x1-x0)/2
            y <- y0
    if (x <> -1 && y <> -1) then
        m.Cells.[x0, y0] <- Walkable
        m.Cells.[x1, y1] <- Walkable
        m.Cells.[x, y] <- Walkable


let backtracking (m: maze) = 
    let fill_with_wall (m: maze) =
        let rec aux (x: int) (y: int) =
            if (x % 2 = 0 || x = m.Width - 1 || y % 2 = 0 || y = m.Height - 1) then 
                m.Cells.[x, y] <- Cell.Wall
            if (x = m.Width - 1 && y = m.Height - 1) then
                ()
            else
                if (y = m.Height - 1) then
                    aux (x+1) 0
                else
                    aux x (y+1)

        aux 0 0

    let mutable cells = Array2D.create m.Width m.Height Unvisited 
    let rec aux (m: maze)(x: int) (y: int) =
        let mutable newPos = get_random_new_pos m cells (x, y)
        while not newPos.IsNone do
            let (newX, newY) = newPos.Value
            set_walkable_between m x y newX newY
            cells.[newX, newY] <- Visited
            aux m newX newY
            newPos <- get_random_new_pos m cells (x, y)
              
    cells.[m.Start_x, m.Start_y] <- Visited
    m.Cells.[m.Start_x, m.Start_y] <- Walkable
    fill_with_wall m
    aux m m.Start_x m.Start_y