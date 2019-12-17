module LabProg2019.Eller

open Maze
open MazeGeneratorUtility
open System
open System.Collections
open System.Linq

let eller (m: maze) = 
    let mutable cells_sets: (int*int) list list = []

    let rec set_contains_cell_with_y (set: (int*int) list) (y: int): bool = 
        match set with
        | [] -> false
        | i::is -> 
            let (ix, iy) = i
            (y = iy) || (set_contains_cell_with_y is y)

    let get_all_set_that_contains_cell_with_y (y: int): (int*int) list list =
        List.where (fun s -> set_contains_cell_with_y s y) cells_sets

    let get_set_of_cell (cell: int*int): (int*int) list =
        let res = (List.where (fun s -> List.contains cell s) cells_sets)
        if res.Count() = 0 then
            [cell]
        else
            res.Head

    let is_in_the_same_set (cell1: int*int) (cell2: int*int): bool =
        List.contains cell2 (get_set_of_cell cell1)

    let join_two_sets_and_append (set1: (int*int) list) (set2: (int*int) list): unit =
        let newSet = set1 @ set2
        cells_sets <- (List.where (fun x -> x <> set1 && x <> set2) cells_sets)//Remove the two set
        cells_sets <- newSet::cells_sets //add the joined set

    let join_two_cells (cell1: int*int) (cell2: int*int): bool =
        let choice = (not (is_in_the_same_set cell1 cell2))
        if choice then //if not in the same set randomly join
            join_two_sets_and_append (get_set_of_cell cell1) (get_set_of_cell cell2)// Join the two sets
            set_walkable_between_pos m cell1 cell2 // Break the wall in between 
        choice

    let randomly_join_two_cells (cell1: int*int) (cell2: int*int): bool =
        if get_rand_boolean () then //if not in the same set randomly join
            join_two_cells cell1 cell2 
        else
            false

    let add_cell_on_new_row_to_set (y: int): unit =
        let rec aux (x: int): unit =
            if x >= m.Width then //Stop one cell because I join with the next cell 
                () 
            else
                if not (List.contains (x, y) (flatten cells_sets)) then 
                    cells_sets <- [(x, y)]::cells_sets
                aux (x+2)        
        if (y >= m.Height) then failwith "Index out of bound"
        aux 1

    let create_random_connection_horizontal (y: int): unit =
        let rec aux (x: int): unit =
            if x >= m.Width - 2 then //Stop one cell before because I join with the next cell 
                () 
            else
                if y = m.Height - 2 then
                    join_two_cells (x, y) (x+2, y) |> ignore //If last row join every cell in different cells_set
                else
                    randomly_join_two_cells (x, y) (x+2, y) |> ignore
                aux (x+2)
        if (y >= m.Height) then failwith "Index out of bound"
        else aux 1
        
    let create_random_connection_vertical (y: int): unit =
        let create_random_connection_on_one_set (set: (int*int) list): unit =
            let cells_on_row = List.where (fun (x0, y0) -> y0 = y) set
            if cells_on_row.Count() = 0 then 
                failwith (String.Format("No cell with y: {0} on given set", y))
            else
                let mutable join_count = 0
                for cell in cells_on_row do
                    let (cell_x, cell_y) = cell 
                    let res = randomly_join_two_cells cell (cell_x, cell_y+2)
                    if res then join_count <- join_count + 1

                if join_count = 0 then // I need at least one connection per set
                    let cell = get_rand_element(cells_on_row)
                    let (cell_x, cell_y) = cell 
                    join_two_cells cell (cell_x, cell_y+2) |> ignore

        let rec aux (l: (int*int) list list): unit =
            match l with
            | [] -> ()
            | x::xs -> 
                create_random_connection_on_one_set x
                aux xs

        if (y >= m.Height) then 
            failwith "Index out of bound"
        else 
            aux (get_all_set_that_contains_cell_with_y y)
           
    let rec aux (y: int): unit =
        if y >= m.Height then
            ()
        else
            add_cell_on_new_row_to_set y
            create_random_connection_horizontal y
            if y <> m.Height - 2 then
                create_random_connection_vertical y
            aux (y+2)
            
    
    fill_with_wall m
    aux 1
        

    