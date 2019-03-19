module Oware

open System.Drawing

type StartingPosition =
    | South
    | North

type GameState =
| SouthTurn
| NorthTurn
| GameEndedDraw
| SouthWon
| NorthWon

let printState = function
| SouthTurn-> "South’s turn"
| NorthTurn -> "North's turn"
| GameEndedDraw -> "Game ended in a draw"
| SouthWon -> "South won"
| NorthWon -> "North won"
| _ -> failwith "Exception: Invalid Game State!"

type Player = {
    Houses : int * int * int * int * int * int
    Name : StartingPosition
    CapturedSeeds : int
}

type Board = {
    P1 : Player
    P2 : Player
    State : GameState
}

let getSeeds n board = failwith "Not implemented"

let useHouse n board = failwith "Not implemented"

let start position = failwith "Not implemented"

let score board = failwith "Not implemented"

let gameState board = failwith "Not implemented"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
