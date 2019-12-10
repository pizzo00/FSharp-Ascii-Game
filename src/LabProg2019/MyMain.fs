module LabProg2019.MyMain

open System
open Engine
open Gfx
open Maze
    

[< NoEquality; NoComparison >]
type state = {
    player : sprite
}

let W = 70
let H = 40

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
        // TODO: check bounds
        st.player.move_by (dx, dy)
        st, key.KeyChar = 'q'


    // create simple backgroud and player
    let maze = MazeGenerator.backtracking  (maze(30, 30))
    
    
    ignore <| engine.create_and_register_sprite ((image.maze maze 5 5 (pixel.filled Color.White) (pixel.filled Color.Red)), 0, 0, 0)
    let player = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.White, pixel.filled Color.Red), W / 2, H / 2, 1)

    // initialize state
    let st0 = { 
        player = player
        }
    // start engine
    engine.loop_on_key my_update st0
