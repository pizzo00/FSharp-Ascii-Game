module LabProg2019.MyMain

open System
open Engine
open Gfx
open Maze
open Menu

//Maze Config
let maze_w = 61
let maze_h = 31
let maze_origin_x = 4
let maze_origin_y = 7

//Title Config
let title_x = 4
let title_y = 0
let title = ["""       _ _   _                 _                              """;
             """ /\ /\| | |_(_)_ __ ___   __ _| |_ ___    /\/\   __ _ _______ """;
             """/ / \ \ | __| | '_ ` _ \ / _` | __/ _ \  /    \ / _` |_  / _ \""";
             """\ \_/ / | |_| | | | | | | (_| | ||  __/ / /\/\ \ (_| |/ /  __/""";
             """ \___/|_|\__|_|_| |_| |_|\__,_|\__\___| \/    \/\__,_/___\___|"""]

//Engine Config

    

[< NoEquality; NoComparison >]
type start_menu_state = {
    menu: menu
}
[< NoEquality; NoComparison >]
type game_state = {
    player: sprite
    current_maze: maze
}

/// <summary>Display a menu</summary>
/// <param name="menu">The menu to be displaied</param>
/// <param name="exit_only_in">The exit choice</param>
let menu_display (menu: menu, exit_only_in: Option<int>): menu =
    let start_menu_update(key : ConsoleKeyInfo) (screen : wronly_raster) (st : start_menu_state) =
        let mutable close = false

        match key.Key with 
        | ConsoleKey.W -> st.menu.select_up()
        | ConsoleKey.S -> st.menu.select_down()
        | ConsoleKey.A -> st.menu.select_left()
        | ConsoleKey.D -> st.menu.select_right()
        | ConsoleKey.Enter -> close <- (exit_only_in.IsNone || exit_only_in.Value = st.menu.Selected)
        | _   -> ()
        (st, close)

    let st = {
        menu = menu
    }
    engine.loop_on_key start_menu_update st
    st.menu.clear()
    st.menu

/// <summary>Display the maze</summary>
/// <param name="algorithm_selector">Algorith to use</param>
/// <param name="player_selector">Player selector</param>
let play(algorithm_selector: int, player_selector: int) =
    let game_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : game_state) =
        // Move player
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

        if key.KeyChar = 'r' then
            let a = Solver.solver st.current_maze
            for (x, y) in a do
                ignore <| engine.create_and_register_sprite (image.single_char (pixel.create('\254', Color.Black, Color.Black)), maze_origin_x + x, maze_origin_y + y, 1)
            
        //Check Bounds
        if st.current_maze.Cells.[maze_relative_next_x, maze_relative_next_y] = Walkable then
            st.player.move_by (dx, dy)
            st.player.pixels.[0].bg <- (st.current_maze.get_pixel_in_pos maze_relative_next_x maze_relative_next_y).fg //Update bg color of player

        //End
        if st.current_maze.is_end_pos (maze_relative_next_x, maze_relative_next_y) then
            st.player.pixels.[0].fg <- Color.White

        let close = key.KeyChar = 'q' 
        (st, close)

    let algorithm =
        match algorithm_selector with
        | 0 -> Backtracking.backtracking
        | 1 -> Eller.eller
        | _ -> failwith "Algorithm not present"

    let m = maze(maze_w, maze_h, algorithm)
    engine.create_and_register_sprite (m.to_image(), maze_origin_x, maze_origin_y, 0) |> ignore
    
    let player = engine.create_and_register_sprite (image.single_char (pixel.create('\254', Color.Blue, m.Pixel_start.fg)), maze_origin_x + m.Start_x, maze_origin_y + m.Start_y, 1)

    // Initialize state
    let game_state = { 
        player = player
        current_maze = m
        }

    // Start engine
    engine.loop_on_key game_update game_state
    ()

/// <summary>Display play settings</summary>
let play_settings () =
    let game_choice_menu_opt1 = new menu_item(["Backtrack"; "Eller"], 6, 8)
    let game_choice_menu_opt2 = new menu_item(["Utente"; "PC"], 6, 10)
    let game_choice_menu_opt3 = new menu_item("Inizia", 6, 12)
    let game_choice_menu = new menu([game_choice_menu_opt1; game_choice_menu_opt2; game_choice_menu_opt3])
    let game_choice_menu = menu_display(game_choice_menu, Some(2))
    let opt_1 = snd(game_choice_menu.Items.[0]).Selected
    let opt_2 = snd(game_choice_menu.Items.[1]).Selected
    play(opt_1, opt_2)

/// <summary>Display the start menu</summary>
let start_menu () =
    let start_menu_opt1 = new menu_item("Inizia Gioco", 6, 8)
    let start_menu_opt2 = new menu_item("Esci", 6, 10)
    let start_menu = new menu([start_menu_opt1; start_menu_opt2])
    let start_menu_index = menu_display(start_menu, None).Selected
    match start_menu_index with 
    | 0 -> play_settings()        
    | _ -> ()

/// <summary>Display title and start hte program</summary>
let main () =
    engine.show_fps <- false;
    let spr = new text(title, Color.White)
    engine.create_and_register_sprite(spr.to_image(), title_x, title_y, 1) |> ignore
    
    start_menu()