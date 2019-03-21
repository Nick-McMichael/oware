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
| SouthTurn-> "South's turn"
| NorthTurn -> "North's turn"
| GameEndedDraw -> "Game ended in a draw"
| SouthWon -> "South won"
| NorthWon -> "North won"

type Player = {
    Name : StartingPosition
    CapturedSeeds : int
}

type Board = {
    Houses : int * int * int * int * int * int * int * int * int * int * int * int
    State : GameState
    P1 : Player
    P2 : Player
}

let setSeeds n board v =
    match board.Houses with
    | (a,b,c,d,e,f,g,h,i,j,k,l) -> 
        match n with
        | 1 -> {board with Houses = v,b,c,d,e,f,g,h,i,j,k,l}
        | 2 -> {board with Houses = a,v,c,d,e,f,g,h,i,j,k,l}
        | 3 -> {board with Houses = a,b,v,d,e,f,g,h,i,j,k,l}
        | 4 -> {board with Houses = a,b,c,v,e,f,g,h,i,j,k,l}
        | 5 -> {board with Houses = a,b,c,d,v,f,g,h,i,j,k,l}
        | 6 -> {board with Houses = a,b,c,d,e,v,g,h,i,j,k,l}
        | 7 -> {board with Houses = a,b,c,d,e,f,v,h,i,j,k,l}
        | 8 -> {board with Houses = a,b,c,d,e,f,g,v,i,j,k,l}
        | 9 -> {board with Houses = a,b,c,d,e,f,g,h,v,j,k,l}
        | 10 -> {board with Houses = a,b,c,d,e,f,g,h,i,v,k,l}
        | 11 -> {board with Houses = a,b,c,d,e,f,g,h,i,j,v,l}
        | 12 -> {board with Houses = a,b,c,d,e,f,g,h,i,j,k,v}
        | _ -> failwith "Exception: Invalid house number"

let getSeeds n board = 
    match board.Houses with
    | (a,b,c,d,e,f,g,h,i,j,k,l) -> 
        match n with 
        | 1 -> a | 2 -> b | 3 -> c | 4 -> d  | 5 -> e  | 6 -> f
        | 7 -> g | 8 -> h | 9 -> i | 10 -> j | 11 -> k | 12 -> l
        | _ -> failwith "Exception: Invalid house number"
   
let collectAndSow n board =
    let rec distributeSeeds seeds houseNo boardn =
        match seeds = 0 with 
        |true -> boardn
        | _ -> 
            match houseNo = 12 with 
            |true -> distributeSeeds (seeds-1) (1) (setSeeds houseNo boardn ((getSeeds houseNo boardn)+1))
            |_ -> 
                match houseNo = 13 with
                |true -> distributeSeeds (seeds-1) (2) (setSeeds (houseNo-12) boardn ((getSeeds (houseNo-12) boardn)+1))
                |_ -> distributeSeeds (seeds-1) (houseNo+1) (setSeeds houseNo boardn ((getSeeds houseNo boardn)+1))
    match board.State with
    | SouthTurn -> 
        match n >=1 && n <= 6 with
        | true -> 
            let seeds = getSeeds n board
            match seeds >= 1 with 
            | true -> 
                let newBoard = setSeeds n board 0 //Collect seeds and set house seeds to 0
                distributeSeeds seeds (n+1) {newBoard with State = NorthTurn}
            | _ -> board
        | _ -> board
    | NorthTurn -> 
        match n >= 7 && n <= 12 with
        | true -> 
            let seeds = getSeeds n board
            match seeds >=1 with
            | true -> 
                let newBoard = setSeeds n board 0 //Collect seeds and set house seeds to 0
                distributeSeeds seeds (n+1) {newBoard with State = SouthTurn}
            | _ -> board
        | _ -> board
 
let captureSeeds n board = failwith "not implemented"

let useHouse n board = 
    collectAndSow n board

let start position = 
    let p1 = {Name = South ; CapturedSeeds = 0}
    let p2 = {Name = North ; CapturedSeeds = 0}
    match position with 
    | South -> {Houses = 4,4,4,4,4,4,4,4,4,4,4,4 ; P1 = p1 ; P2 = p2 ; State = SouthTurn}
    | North -> {Houses = 4,4,4,4,4,4,4,4,4,4,4,4 ; P1 = p1 ; P2 = p2 ; State = NorthTurn}

let score board = failwith "Not implemented"

let gameState board = 
    printState board.State

[<EntryPoint>]
let main _ =
    let b = useHouse 1 (start South) |> useHouse 7 |> useHouse 2 |> useHouse 9 |> useHouse 3 |> useHouse 10 |> useHouse 1 |> useHouse 11 |> useHouse 2 |> useHouse 9 |> useHouse 4 |> useHouse 7 |> useHouse 5 |> useHouse 12 |> useHouse 3 |> useHouse 11 |> useHouse 6
    printfn "Hello from F#!"
    0 // return an integer exit code
