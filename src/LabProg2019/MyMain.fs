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

let maze_w = 61
let maze_h = 31
let maze_origin_x = 5
let maze_origin_y = 5
let maze_start_x = 1
let maze_start_y = 1

let main () =       
    let engine = new engine (W, H)

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0., -1.
            | 's' -> 0., 1.
            | 'a' -> -1., 0.
            | 'd' -> 1., 0.
            | _   -> 0., 0.
        
        let nextX = st.player.x + dx
        let nextY = st.player.y + dy

        if(st.current_maze.Cells.[(int)nextX - maze_origin_x, (int)nextY - maze_origin_y] = Walkable) then
            st.player.move_by (dx, dy)
        st, key.KeyChar = 'q'


    // create simple backgroud and player
    let m = maze(maze_w, maze_h)
    MazeGenerator.backtracking m maze_start_x maze_start_y
    let current_maze = engine.create_and_register_sprite ((image.maze m (pixel.filled Color.White) (pixel.filled Color.Red)), maze_origin_x, maze_origin_y, 0)
    
    let player = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Blue, pixel.filled Color.Blue), maze_origin_x + maze_start_x, maze_origin_y + maze_start_y, 1)

    // initialize state
    let st0 = { 
        player = player
        current_maze = m
        }
    // start engine
    engine.loop_on_key my_update st0
