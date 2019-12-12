module LabProg2019.MyMain

open System
open Engine
open Gfx
open Maze
    

[< NoEquality; NoComparison >]
type state = {
    player : sprite
    current_maze: maze
}

let W = 70
let H = 40

let maze_w = 15//61
let maze_h = 15//31
let maze_origin_x = 5
let maze_origin_y = 5

let main () =       
    let engine = new engine (W, H)

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0, -1
            | 's' -> 0, 1
            | 'a' -> -1, 0
            | 'd' -> 1, 0
            | _   -> 0, 0
        
        let next_x = st.player.x + dx
        let next_y = st.player.y + dy
        let maze_relative_next_x = next_x - maze_origin_x
        let maze_relative_next_y = next_y - maze_origin_y

        if(st.current_maze.Cells.[maze_relative_next_x, maze_relative_next_y] = Walkable) then
            st.player.move_by (dx, dy)
            st.player.pixels.[0].bg <- (st.current_maze.get_pixel_in_pos maze_relative_next_x maze_relative_next_y).fg //Update bg color of player

        if(st.current_maze.is_end_pos maze_relative_next_x maze_relative_next_y) then
            st.player.pixels.[0].fg <- Color.White

        let close = key.KeyChar = 'q' 
        (st, close)


    // create simple backgroud and player
    let m = maze(maze_w, maze_h)
    MazeGenerator.backtracking m
    ignore <| engine.create_and_register_sprite (m.to_image(), maze_origin_x, maze_origin_y, 0)
    
    let player = engine.create_and_register_sprite (image.single_char (pixel.create('\254', Color.Blue, m.Pixel_start.fg)), maze_origin_x + m.Start_x, maze_origin_y + m.Start_y, 1)

    // Initialize state
    let state = { 
        player = player
        current_maze = m
        }

    // Start engine
    engine.loop_on_key my_update state
