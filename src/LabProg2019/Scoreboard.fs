module Scoreboard

open System
open System.Data.SQLite

let db_pos = @"..\..\..\db.sqlite3"
let connection_string = sprintf "Data Source=%s;Version=3;" db_pos

type score =
    val Name: string
    val Points: int 

    new(name, points) =
        {
            Name = name
            Points = points
        }


///<summary>
///Get all the score order desc
///</summary>
///<returns>A Score List</returns>
let get_all_score(): score list =
    let rec aux (reader: SQLiteDataReader): score list =
        if reader.Read() then
            let name = reader.["name"].ToString()
            let score: int = Convert.ToInt32(reader.["score"])
            new score(name, score)::(aux reader)
        else
            []

    let conn = new SQLiteConnection(connection_string)
    conn.Open() 
    let sql = "SELECT name, score FROM scores ORDER BY score DESC"
    let command = new SQLiteCommand(sql, conn)
    let reader = command.ExecuteReader()
    let res = aux reader
    conn.Close()
    res

let insert_score(s: score): unit =
    let conn = new SQLiteConnection(connection_string)
    conn.Open() 
    let sql = "INSERT INTO scores (name, score) VALUES (@name, @score)"
    let command = new SQLiteCommand(sql, conn)
    command.Parameters.AddWithValue("@name", s.Name) |> ignore
    command.Parameters.AddWithValue("@score", s.Points) |> ignore
    command.ExecuteNonQuery() |> ignore
    conn.Close()