module LabProg2019.MazeGeneratorUtility

open Maze 
open System
open System.Linq

let rand = Random(DateTime.Now.Millisecond)
type AlgorithmCell = Unvisited | Visited

/// <summary>Get a random boolean</summary>
let get_rand_boolean (): bool =
    rand.Next(0,2) = 1

/// <summary>Get a random element in a given list</summary>
let get_rand_element (l: 'a list): 'a =
    l.[rand.Next(0, l.Count())]

/// <summary>Is the given position inside the bound of maze</summary>
let is_valid_pos (m: maze) (pos: int*int): bool =
    let (x, y) = pos
    (x < m.Width && y < m.Height && x >= 0 && y >= 0) && //No out of bound
    (x % 2 = 1 || y % 2 = 1) //No on wall grid

/// <summary>Break a wall between two cells</summary>
let set_walkable_between_coordinates (m: maze) (x0: int) (y0: int) (x1: int) (y1: int): unit =
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

/// <summary>Break a wall between two cells</summary>
let set_walkable_between_pos (m: maze) (pos0: int*int) (pos1: int*int): unit =
    let (x0, y0) = pos0
    let (x1, y1) = pos1
    set_walkable_between_coordinates m x0 y0 x1 y1

/// <summary>Create all the possible wall in the maze</summary>
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

let flatten (l: 'a list list): 'a list =
    let rec aux (l: 'a list list) (acc: 'a list): 'a list =
        match l with
        | [] -> acc 
        | x::xs -> aux xs (x@acc)
    aux l [] 