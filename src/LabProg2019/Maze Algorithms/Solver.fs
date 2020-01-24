module LabProg2019.Solver

open Maze
open MazeGeneratorUtility
open System

let solver (m: maze): (int*int) list =
    let mutable cells = Array2D.create m.Width m.Height Unvisited 

    let rec get_possible_choices (choices: (int*int) list): (int*int) list =
        match choices with
        | [] -> []
        | pos::ls ->
            let (x, y) = pos
            if is_valid_pos m pos && m.Cells.[x, y] = Walkable && cells.[x, y] = Unvisited then
               pos::(get_possible_choices ls)
            else
               (get_possible_choices ls)

    let rec aux (pos: int*int): (int*int) list option =
        let (x, y) = pos
        cells.[x, y] <- Visited
        if m.is_end_pos pos then 
            Some ([])
        else
            let choices = [(x+1, y);
                           (x-1, y); 
                           (x,   y+1); 
                           (x,   y-1)]
            let possible_choices = get_possible_choices choices
            if possible_choices.Length = 0 then
                None
            else
                let res = try_choices possible_choices
                if res.IsSome then
                    Some(pos::res.Value)
                else   
                    None
 
    and try_choices (choices: (int*int) list): (int*int) list option =
        match choices with
        | [] -> None
        | x::xs -> 
            let res = aux x
            if res.IsSome then
                Some(res.Value)
            else
                try_choices xs

    (aux (m.Start_x, m.Start_y)).Value