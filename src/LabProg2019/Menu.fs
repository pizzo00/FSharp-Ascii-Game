﻿module LabProg2019.Menu

open Gfx
open Engine
                   
/// <summary>
/// Provide a class to print text sprite
/// </summary>
type text =
    val Lines: string list
    val Width: int
    val Height: int
    val Fg_color: Color
    val Bg_color: Color    
    

    /// <param name="lines">A list of string, every element is a line of text</param>
    /// <param name="fg_color">Foreground color of text</param>
    /// <param name="bg_color">Background color of text</param>
    new (lines: string list, fg_color: Color, ?bg_color: Color) =
        {
            Lines = lines
            Width = List.max (List.map (fun (x: string) -> x.Length) lines)
            Height = lines.Length
            Fg_color = fg_color
            Bg_color = defaultArg bg_color Color.Black
        }

    /// <param name="lines">A single line of text</param>
    /// <param name="fg_color">Foreground color of text</param>
    /// <param name="bg_color">Background color of text</param>
    new (line: string, fg_color: Color, ?bg_color: Color) = text([line], fg_color, defaultArg bg_color Color.Black)

    /// <summary>Create a new image with the text</summary>
    /// <returns>The newly created image</returns>
    member this.to_image (): image =
        let i = new image (this.Width, this.Height)
        this.draw i
        i
    
    /// <summary>Insert the text in a newly created image</summary> 
    /// <param name="img">Foreground color of text</param>
    member private this.draw (img: image): unit =
        let rec draw_line (x: int) (y: int) (l: char list): unit =
            match l with
            | [] -> ()
            | c::cs ->
                let pixel = pixel.create(c, this.Fg_color, this.Bg_color)
                img.plot(x, y, pixel)
                draw_line (x+1) y cs

        for y in 0 .. this.Height-1  do
            let chars = Array.toList(this.Lines.[y].ToCharArray())
            draw_line 0 y chars

/// <summary>
/// Provide a class to print menu item
/// </summary>
type menu_item =
    val MultiChoice: bool
    val mutable Choices: text list
    val MaxWidth: int
    val mutable Selected: int
    val mutable CurrentSprite: Option<sprite>
    val mutable X: int
    val mutable Y: int
    val mutable Arrow_left: Option<sprite>
    val mutable Arrow_right: Option<sprite>

    /// <param name="choices">A list of string, every element is a possible choice</param>
    /// <param name="x">X position of menu item</param>
    /// <param name="y">Y position of menu item</param>
    /// <param name="selected">Initial index of choiches</param>
    /// <param name="fg_color">Foreground color of text</param>
    /// <param name="bg_color">Background color of text</param>
    new(choices: string list, x: int, y: int, ?selected: int, ?fg_color: Color, ?bg_color: Color) as this = 
        {
            MultiChoice = true
            Choices = []
            MaxWidth = List.max (List.map (fun (x: string) -> x.Length) choices)
            Selected = defaultArg selected 0
            X = x
            Y = y
            CurrentSprite = None
            Arrow_left = None
            Arrow_right = None
        }
        then
            this.Choices <- List.map ( fun (x: string) -> new text(x, (defaultArg fg_color Color.White), (defaultArg bg_color Color.Black))) choices
            this.CurrentSprite <- Some (this.display_Choiche(this.Selected))
            
            //Display Arrow
            let arrow_left = image.single_char(pixel.create('<', (defaultArg fg_color Color.White), (defaultArg bg_color Color.Black)))
            let arrow_right = image.single_char(pixel.create('>', (defaultArg fg_color Color.White), (defaultArg bg_color Color.Black)))
            this.Arrow_left  <- Some (engine.create_and_register_sprite(arrow_left, this.X, this.Y, 1))
            this.Arrow_right <- Some (engine.create_and_register_sprite(arrow_right, this.X+this.MaxWidth+4, this.Y, 1))

    /// <param name="choice">The string to be printed</param>
    /// <param name="x">X position of menu item</param>
    /// <param name="y">Y position of menu item</param>
    /// <param name="fg_color">Foreground color of text</param>
    /// <param name="bg_color">Background color of text</param>
    new(choice: string, x: int, y: int, ?fg_color: Color, ?bg_color: Color) as this = 
        {
            MultiChoice = false
            Choices = [new text(choice, (defaultArg fg_color Color.White), (defaultArg bg_color Color.Black))]
            MaxWidth = choice.Length
            Selected = 0
            X = x
            Y = y
            CurrentSprite = None
            Arrow_left = None
            Arrow_right = None
        }
        then            
            this.CurrentSprite <- Some (this.display_Choiche(this.Selected))
               
    /// <summary>Display a Choice</summary>
    /// <param name="index">The index of choice to display</param>
    /// <returns>The newly created sprite</returns>
    member private this.display_Choiche (index: int): sprite =
        engine.create_and_register_sprite(this.Choices.[index].to_image(), this.X+2, this.Y, 1)

    /// <summary>Change diplayed choiche</summary>
    /// <param name="index">The index of choice to display</param>
    member this.select (index: int): unit =
        this.CurrentSprite.Value.clear
        this.CurrentSprite <- Some(this.display_Choiche(index))

    /// <summary>Change diplayed choiche (go right)</summary>
    member this.select_right (): unit =
        this.Selected <- this.Selected+1
        if this.Selected >= this.Choices.Length then this.Selected <- 0
        this.select(this.Selected)
    
    /// <summary>Change diplayed choiche (go left)</summary>
    member this.select_left (): unit =
        this.Selected <- this.Selected-1
        if this.Selected < 0 then this.Selected <- this.Choices.Length-1
        this.select(this.Selected)
        
    /// <summary>Erase the registered sprite</summary>
    member this.clear(): unit =
        if this.Arrow_left.IsSome then
            this.Arrow_left.Value.clear
        if this.Arrow_right.IsSome then   
            this.Arrow_right.Value.clear
        this.CurrentSprite.Value.clear

/// <summary>
/// Provide a class to print a collection of menu item
/// </summary>
type menu = 
    val mutable Items: (sprite*menu_item) list
    val mutable Selected: int
    val private Fg_color: Color
    val private Bg_color: Color

    /// <param name="items">A list of menu_items</param>
    /// <param name="selected">Initial position of selector</param>
    /// <param name="fg_color">Foreground color of text</param>
    /// <param name="bg_color">Background color of text</param>
    new(items: (menu_item) list, ?selected: int, ?fg_color: Color, ?bg_color: Color) as this =
        let rec create_selectors (l: (menu_item) list) (selected: int): (sprite*menu_item) list = 
            match l with
            | [] -> []
            | c::cs ->
                let selector = 
                    if selected = 0 then
                        image.single_char(pixel.create('\175', this.Fg_color, this.Bg_color))
                    else
                        image.single_char(pixel.create('\175', this.Bg_color, this.Bg_color))
                let selector_spr = engine.create_and_register_sprite(selector, c.X-2, c.Y, 1)
                (selector_spr, c)::create_selectors cs (selected-1)
        {
            Items = []
            Selected = defaultArg selected 0
            Fg_color = defaultArg fg_color Color.White
            Bg_color = defaultArg bg_color Color.Black
        }
        then
            this.Items <- create_selectors items (defaultArg selected 0)
        
    /// <summary>Change selector position</summary>
    /// <param name="index">The selector position</param>
    member this.select (index: int): unit =
        let rec aux (l: (sprite*menu_item) list) (i: int): unit =
            match l with
            | [] -> ()
            | x::xs ->
                let (s, _) = x
                s.pixels.[0].fg <-
                    if i = 0 then 
                        this.Fg_color
                    else
                        this.Bg_color
                aux xs (i-1)
        aux this.Items index

    /// <summary>Change selector position (go down)</summary>
    member this.select_down (): unit =
        this.Selected <- this.Selected+1
        if this.Selected >= this.Items.Length then this.Selected <- 0
        this.select(this.Selected)
    
    /// <summary>Change selector position (go up)</summary>
    member this.select_up (): unit =
        this.Selected <- this.Selected-1
        if this.Selected < 0 then this.Selected <- this.Items.Length-1
        this.select(this.Selected)

    /// <summary>Change diplayed choiche (go left)</summary>
    member this.select_left (): unit =
        snd(this.Items.[this.Selected]).select_left()

    /// <summary>Change diplayed choiche (go right)</summary>
    member this.select_right (): unit =
        snd(this.Items.[this.Selected]).select_right()

    /// <summary>Erase the registered sprite</summary>
    member this.clear (): unit =
        let rec aux (l: (sprite*menu_item) list) =
            match l with
            | [] -> ()
            | x::xs ->
                let (a, b) = x
                a.clear
                b.clear()
                aux xs
        aux this.Items
