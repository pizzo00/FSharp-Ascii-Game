module LabProg2019.MyMain

open System
open System.Timers
open Engine
open Gfx
open Maze
open Menu
open Scoreboard

//Maze Config
let maze_w = 61
let maze_h = 31
let maze_origin_x = 4
let maze_origin_y = 9

//Title Config
let title_x = 4
let title_y = 0
let title = ["""       _ _   _                 _                              """;
             """ /\ /\| | |_(_)_ __ ___   __ _| |_ ___    /\/\   __ _ _______ """;
             """/ / \ \ | __| | '_ ` _ \ / _` | __/ _ \  /    \ / _` |_  / _ \""";
             """\ \_/ / | |_| | | | | | | (_| | ||  __/ / /\/\ \ (_| |/ /  __/""";
             """ \___/|_|\__|_|_| |_| |_|\__,_|\__\___| \/    \/\__,_/___\___|"""]
    
//Score Config - play
let mutable score = 999
let lose_a_point_every: float = 1000.0
let score_x = 6
let score_y = 7

//Score Add Config
let score_insert_name_x = 6
let score_insert_name_y = 7

[< NoEquality; NoComparison >]
type start_menu_state = {
    menu: menu
}
[< NoEquality; NoComparison >]
type game_state_player = {
    player: sprite
    current_maze: maze
}
[< NoEquality; NoComparison >]
type game_state_autoplay = {
    player: sprite
    current_maze: maze
    mutable solution: (int*int) list
}
[< NoEquality; NoComparison >]
type score_insert_state = {
    mutable name: string
    mutable spr_str: sprite list
    mutable line: sprite
}

/// <summary>Display a menu</summary>
/// <param name="menu">The menu to be displaied</param>
/// <param name="exit_only_in">The exit choice</param>
let menu_display (menu: menu, exit_only_in: Option<int>): menu =
    let start_menu_update(key : ConsoleKeyInfo) (screen : wronly_raster) (st : start_menu_state) =
        let mutable close = false

        match key.Key with 
        | ConsoleKey.W | ConsoleKey.UpArrow -> st.menu.select_up()
        | ConsoleKey.S | ConsoleKey.DownArrow -> st.menu.select_down()
        | ConsoleKey.A | ConsoleKey.LeftArrow -> st.menu.select_left()
        | ConsoleKey.D | ConsoleKey.RightArrow -> st.menu.select_right()
        | ConsoleKey.Enter -> close <- (exit_only_in.IsNone || exit_only_in.Value = st.menu.Selected)
        | _   -> ()
        (st, close)

    let st = {
        menu = menu
    }
    engine.loop_on_key start_menu_update st
    st.menu.clear()
    st.menu
    
/// <summary>Prints the score list</summary>
let score_show() =
    let update(key : ConsoleKeyInfo) (screen : wronly_raster) (st) =
        (st, (key.Key = ConsoleKey.Q))

    let rec aux (l: score list) (max: int): string list =
        if max = 0 then []
        else 
            match l with
            | [] -> []
            | x::xs -> (x.Name + "   " + x.Points.ToString())::""::aux xs (max-1)//Leave also a void line
    
    let l = get_all_score()
    let s = ("NAME SCORE")::(" ")::aux l 20
    let t = new text(s, Color.White)
    engine.create_and_register_sprite(t.to_image(), 12, 6, 4) |> ignore
    let t2 = new text("Press Q to quit...", Color.White)
    engine.create_and_register_sprite(t2.to_image(), 70 - t.Width - 10, 40 - 3, 4) |> ignore
    engine.loop_on_key update 0
            
    
/// <summary>Permit to seva a score</summary>
let score_add(score: int) =
    let update(key : ConsoleKeyInfo) (screen : wronly_raster) (st: score_insert_state) =
        if (key.KeyChar >= 'a' && key.KeyChar >= 'z') || (key.KeyChar >= 'A' && key.KeyChar >= 'Z') then
            let c = Char.ToUpper(key.KeyChar)
            st.name <- st.name + c.ToString()
            let (x, y) =
                match st.name.Length with
                | 1 ->  (score_insert_name_x, score_insert_name_y)
                | 2 ->  (score_insert_name_x + 1, score_insert_name_y)
                | 3 ->  (score_insert_name_x + 2, score_insert_name_y)
                | _ -> failwith "Lenght not supported"

            st.spr_str <- engine.create_and_register_sprite(image.single_char(pixel.create(c, Color.White)), x, y, 4)::st.spr_str

            if (st.name.Length < 3) then
                st.line.move_by(1, 0)                
                (st, false)
            else
                for i in 0..st.spr_str.Length-1 do
                    st.spr_str.[i].clear
                st.line.clear
                (st, true)
                
        else
            (st, false)

    let st = { 
        name = ""
        spr_str = []
        line = engine.create_and_register_sprite(image.single_char(pixel.create('_', Color.White)), score_insert_name_x, score_insert_name_y, 4)
    }
    
    let desc_txt = new text("NAME SCORE", Color.White)
    let score_txt = new text(score.ToString(), Color.White)
    let desc_spr = engine.create_and_register_sprite(desc_txt.to_image(), score_insert_name_x, score_insert_name_y-1, 4)
    let score_spr = engine.create_and_register_sprite(score_txt.to_image(), score_insert_name_x+6, score_insert_name_y, 4)
    engine.loop_on_key update st
    insert_score(new score(st.name, score))
    desc_spr.clear
    score_spr.clear
    ()

/// <summary>Display the maze</summary>
/// <param name="algorithm_selector">Algorith to use</param>
/// <param name="player_selector">Player selector</param>
let play(algorithm_selector: int, player_selector: int) =
    let game_update_player (key : ConsoleKeyInfo) (screen : wronly_raster) (st : game_state_player) =
        // Move player
        let dx, dy =
            match key.Key with 
            | ConsoleKey.W | ConsoleKey.UpArrow -> 0, -1
            | ConsoleKey.A | ConsoleKey.DownArrow -> 0, 1
            | ConsoleKey.S | ConsoleKey.LeftArrow -> -1, 0
            | ConsoleKey.D | ConsoleKey.RightArrow -> 1, 0
            | _   -> 0, 0

        let next_x = st.player.x + dx
        let next_y = st.player.y + dy
        let maze_relative_next_x = next_x - maze_origin_x
        let maze_relative_next_y = next_y - maze_origin_y
            
        //Check Bounds
        if st.current_maze.Cells.[maze_relative_next_x, maze_relative_next_y] = Walkable then
            st.player.move_by (dx, dy)
            st.player.pixels.[0].bg <- (st.current_maze.get_pixel_in_pos maze_relative_next_x maze_relative_next_y).fg //Update bg color of player

        
        let is_end = st.current_maze.is_end_pos (maze_relative_next_x, maze_relative_next_y)
        (st, is_end)

    let game_update_autoplay (key : ConsoleKeyInfo) (screen : wronly_raster) (st : game_state_autoplay) =
        match st.solution with
        | [] -> (st, true)
        | p::ps ->
            let (x, y) = p
            let abs_x = maze_origin_x + x
            let abs_y = maze_origin_y + y
            let maze_relative_next_x = abs_x - maze_origin_x
            let maze_relative_next_y = abs_y - maze_origin_y
            st.player.x <- abs_x
            st.player.y <- abs_y            
            st.player.pixels.[0].bg <- (st.current_maze.get_pixel_in_pos maze_relative_next_x maze_relative_next_y).fg //Update bg color of player
            engine.create_and_register_sprite (image.single_char (pixel.create('\254', Color.Black, Color.Black)), abs_x, abs_y, 1) |> ignore
            st.solution <- ps
            (st, false)

    let algorithm =
        match algorithm_selector with
        | 0 -> Backtracking.backtracking
        | 1 -> Eller.eller
        | _ -> failwith "Algorithm not present"

    let m = maze(maze_w, maze_h, algorithm)
    let maze_spr = engine.create_and_register_sprite (m.to_image(), maze_origin_x, maze_origin_y, 0)
    
    let player = engine.create_and_register_sprite (image.single_char (pixel.create('\254', Color.Blue, m.Pixel_start.fg)), maze_origin_x + m.Start_x, maze_origin_y + m.Start_y, 1)
    
    // Start game
    if player_selector = 0 then
        let timer = new Timer();        
        let mutable score_spr = engine.create_and_register_sprite((new text("Punteggio: " + score.ToString(), Color.White)).to_image(), score_x, score_y, 1) 

        let timer_action(*(object: obj)*) (args: EventArgs):unit =
            score <- score - 1
            if score < 0 then
                timer.Stop()
            else
                score_spr.clear
                score_spr <- engine.create_and_register_sprite((new text("Punteggio: " + score.ToString(), Color.White)).to_image(), score_x, score_y, 1)
        
        timer.Interval <- lose_a_point_every
        timer.Elapsed.Add(timer_action)
        timer.Start()

        // Initialize state
        let game_state = { 
            player = player
            current_maze = m
            }
        engine.loop_on_key game_update_player game_state
        timer.Stop()
        score_spr.clear
    else
        let game_state = { 
            player = player
            current_maze = m
            solution = (Solver.solver m).Tail //Remove the first element (the start)
            }
        engine.loop_on_key game_update_autoplay game_state
    maze_spr.clear
    player.clear

    if player_selector = 0 then
        score_add(score)    
    score_show()

/// <summary>Display play settings</summary>
let play_settings () =
    let game_choice_menu_opt1 = new menu_item(["Backtrack"; "Eller"], 6, 8)
    let game_choice_menu_opt2 = new menu_item(["Utente"; "Auto Play"], 6, 10)
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