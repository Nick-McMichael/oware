module Oware


open System.Drawing

//Used to define player one and two board rows
type StartingPosition =
    | South
    | North

//Used to control the flow of the game
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

// A player owns their captured seeds and have a starting 
// position defining their side of the board
type Player = {
    Name : StartingPosition
    CapturedSeeds : int
}

// A 12-tuple to store the amount of seeds from 1-12 (left to right) where
// the first 6 elements belong to South and the last 6 to North 
// NOTE: element 1 is South's leftmost house and 12 is North's left most house
// N  12  11  10  9   8   7
// S  1   2   3   4   5   6
type Board = {
    Houses : int * int * int * int * int * int * int * int * int * int * int * int
    State : GameState
    P1 : Player
    P2 : Player
}

// Notice the argument value which is used to set the n-th element
// to value
let setSeeds n board value =
    match board.Houses with
    | (a,b,c,d,e,f,g,h,i,j,k,l) -> 
        match n with
        | 1 -> {board with Houses = value,b,c,d,e,f,g,h,i,j,k,l}
        | 2 -> {board with Houses = a,value,c,d,e,f,g,h,i,j,k,l}
        | 3 -> {board with Houses = a,b,value,d,e,f,g,h,i,j,k,l}
        | 4 -> {board with Houses = a,b,c,value,e,f,g,h,i,j,k,l}
        | 5 -> {board with Houses = a,b,c,d,value,f,g,h,i,j,k,l}
        | 6 -> {board with Houses = a,b,c,d,e,value,g,h,i,j,k,l}
        | 7 -> {board with Houses = a,b,c,d,e,f,value,h,i,j,k,l}
        | 8 -> {board with Houses = a,b,c,d,e,f,g,value,i,j,k,l}
        | 9 -> {board with Houses = a,b,c,d,e,f,g,h,value,j,k,l}
        | 10 -> {board with Houses = a,b,c,d,e,f,g,h,i,value,k,l}
        | 11 -> {board with Houses = a,b,c,d,e,f,g,h,i,j,value,l}
        | 12 -> {board with Houses = a,b,c,d,e,f,g,h,i,j,k,value}
        | _ -> failwith "Exception: Invalid house number"

// Used to return the number of seeds in a specified house on the board
let getSeeds n board = 
    match board.Houses with
    | (a,b,c,d,e,f,g,h,i,j,k,l) -> 
        match n with 
        | 1 -> a | 2 -> b | 3 -> c | 4 -> d  | 5 -> e  | 6 -> f
        | 7 -> g | 8 -> h | 9 -> i | 10 -> j | 11 -> k | 12 -> l
        | _ -> failwith "Exception: Invalid house number"

let getSeeds2 n board =
    match n-1 with
    |0 -> getSeeds 12 board
    |_ -> getSeeds (n - 1) board
    
let getHouse n =
    match n-1 with
    |0 -> 12
    |_ -> n-1
   

    ///////////////////////////////////////////////////////////////////////////////
let rec captureSeeds n board = 
    //match baord>state 
    match (n>6,board.State) with
    |(true, NorthTurn) -> 

        let newBoard = {board with P1 = { board.P1 with CapturedSeeds = (board.P1.CapturedSeeds + getSeeds n board)}}
        let newnewBoard = setSeeds n newBoard 0
             
        match (n-1 > 6) && (getSeeds2 (n) newnewBoard) = 2 || (getSeeds2 (n) newnewBoard) = 3 with
        |true -> captureSeeds (getHouse n) newnewBoard
        |false -> 
            match newnewBoard.P1.CapturedSeeds = 24 && newnewBoard.P2.CapturedSeeds = 24 with
            |true -> {newnewBoard with State = GameEndedDraw}
            |false ->
                match newnewBoard.P1.CapturedSeeds > 24 with
                |true -> {newnewBoard with State = SouthWon}
                |false -> 
                    match newnewBoard.Houses with // this match statment now checks that the new board has left a move open for the next player
                    |_,_,_,_,_,_,0,0,0,0,0,0 -> board //North has no move, thus this move was ilegal and no change is made
                    |_,_,_,_,_,_,_,_,_,_,_,_ -> newnewBoard //North has a move, the move is legal  



    |(false, SouthTurn) -> 

              let newBoard = {board with P2 = { board.P2 with CapturedSeeds = (board.P2.CapturedSeeds + getSeeds n board)}}
              let newnewBoard = setSeeds n newBoard 0

              match (n-1 > 0) && (getSeeds2 (n) newnewBoard) = 2 || (getSeeds2 (n) newnewBoard) = 3 with
              |true -> captureSeeds (getHouse n) newnewBoard
              |false ->
                    match newnewBoard.P1.CapturedSeeds = 24 && newnewBoard.P2.CapturedSeeds = 24 with
                    |true -> {newnewBoard with State = GameEndedDraw}
                    |false ->
                        match newnewBoard.P2.CapturedSeeds > 24 with
                        |true -> {newnewBoard with State = NorthWon}
                        |false -> 
                            match newnewBoard.Houses with // this match statment now checks that the new board has left a move open for the next player
                            |0,0,0,0,0,0,_,_,_,_,_,_ -> board //North has no move, thus this move was ilegal and no change is made
                            |_,_,_,_,_,_,_,_,_,_,_,_ -> newnewBoard //North has a move, the move is legal
                

           

     |_,_ -> board
 ///////////////////////////////////////////////////////////////////////////////
// Makes use of getSeeds and setSeeds
// Contains recursive function distributeSeeds to 'sow' collected seeds anticlockwise
// around the board.
// NOTE: The recursive funcion is triggered from the match statement below it.


let collectAndSow n board =
    let collectionHouse = n //Keeping track of the initial house chosen to collect seeds from
    
    let rec distributeSeeds seeds houseNo boardn = 
        match seeds = 0 with 
        |true -> 
            match getSeeds2 (houseNo) boardn with
            |2 -> captureSeeds (getHouse houseNo) boardn
            |3 -> captureSeeds (getHouse houseNo) boardn
            |_ -> boardn
           // boardn
             // BASE CASE
        | _ -> //RECURSIVE CASE----------------------------------------------------------------------------------------------------------
            match houseNo = collectionHouse with //Skipping the initial house as seeds can't be sown there.
            | true -> 
                match houseNo = 12 with  //Edge case to deal with looping back to the start of the board (house 12 to house 1)
                |true -> distributeSeeds (seeds-1) (houseNo-12+2) (setSeeds (houseNo+1) boardn ((getSeeds (houseNo+1) boardn)+1))
                |_ -> 
                match houseNo = 13 with //Edge case to deal with looping back to the start of the board (house 12 to house 1)
                |true -> distributeSeeds (seeds-1) (houseNo-12+2) (setSeeds (houseNo-12+1) boardn ((getSeeds (houseNo-12+1) boardn)+1))
                |_ -> distributeSeeds (seeds-1) (houseNo+2) (setSeeds (houseNo+1) boardn ((getSeeds (houseNo+1) boardn)+1))
            | false -> // When the initial house does not need to be skipped
                match houseNo = 12 with  //Edge case to deal with looping back to the start of the board (house 12 to house 1)
                |true -> distributeSeeds (seeds-1) (houseNo-12+1) (setSeeds houseNo boardn ((getSeeds houseNo boardn)+1))
                |_ -> 
                    match houseNo = 13 with //Edge case to deal with looping back to the start of the board (house 12 to house 1)
                    |true -> distributeSeeds (seeds-1) (houseNo-12+1) (setSeeds (houseNo-12) boardn ((getSeeds (houseNo-12) boardn)+1))
                    |_ -> distributeSeeds (seeds-1) (houseNo+1) (setSeeds houseNo boardn ((getSeeds houseNo boardn)+1))
        //---------------------------------------------------------------------------------------------------------------------------------
     // ************************ This function starts here and calls the above recursive function ************************       
    match board.State with
    | SouthTurn -> // South's Move
        match n >=1 && n <= 6 with // Checking for valid entry of chosen house for respective turn
        | true -> 
            let seeds = getSeeds n board // Retrieve the amount of seeds collected from chosen house
            match seeds >= 1 with // Can only collect from house with one or more seeds
            | true -> 
                let temp = setSeeds n board 0 // setting chosen house amount of seeds to 0 as they have been collected
                //let FinalHousePlacedIn = n + seeds
                let newBoard = distributeSeeds seeds (n+1) {temp with State = NorthTurn} // sowing collected seeds recursively starting from next house

                match newBoard.Houses with // this match statment now checks that the new board has left a move open for the next player
                |_,_,_,_,_,_,0,0,0,0,0,0 -> newBoard //North has no move, thus this move was ilegal and no change is made
                |_,_,_,_,_,_,_,_,_,_,_,_ -> newBoard //North has a move, the move is legal
            | _ -> board // invalid house was chosen, board unchanged and try again
        | _ -> board // invalid house was chosen, board unchanged and try again
    | NorthTurn -> // North's turn
        match n >= 7 && n <= 12 with // Checking for valid entry of chosen house for respective turn
        | true -> 
            let seeds = getSeeds n board 
            match seeds >=1 with
            | true -> 
                let temp = setSeeds n board 0 //Collect seeds and set house seeds to 0
                let newBoard = distributeSeeds seeds (n+1) {temp with State = SouthTurn} //Same as before, note State alternates after succesful turn
                match newBoard.Houses with // this match statment now checks that the new board has left a move open for the next player
                |0,0,0,0,0,0,_,_,_,_,_,_ -> newBoard //South has no move, thus this move was ilegal and no change is made
                |_,_,_,_,_,_,_,_,_,_,_,_ -> newBoard //South has a move, the move is legal
            | _ -> board // invalid house was chosen, board unchanged and try again
        | _ -> board // invalid house was chosen, board unchanged and try again
    |_ -> failwith "78ju8ji8ji8ji"


let useHouse n board = // TODO ........ Sowing passes tests so far..
    collectAndSow n board

// Choosing the starting position for players and initialising the game
let start position = 
    let p1 = {Name = South ; CapturedSeeds = 0}
    let p2 = {Name = North ; CapturedSeeds = 0}
    match position with 
    | South -> {Houses = 4,4,4,4,4,4,4,4,4,4,4,4 ; P1 = p1 ; P2 = p2 ; State = SouthTurn}
    | North -> {Houses = 4,4,4,4,4,4,4,4,4,4,4,4 ; P1 = p1 ; P2 = p2 ; State = NorthTurn}

let playGame numbers =
    let rec play xs game =
        match xs with
        | [] -> game
        | x::xs -> play xs (useHouse x game)
    play numbers (start South)


let score board = 
    (board.P1.CapturedSeeds,board.P2.CapturedSeeds)

let gameState board = 
    printState board.State



[<EntryPoint>]
let main _ =
    //let ``Seeds are captured when there are 2 or 3 of them`` () =
    let ``kl`` () =
         playGame [6; 8; 5; 9; 4; 12; 3; 10; 1; 11; 2; 12; 5; 7; 5; 11; 6; 8; 1; 12; 4; 10; 5; 9;
            2; 11; 3; 12; 6; 9; 5; 10; 2; 11; 1; 12; 4; 7; 6; 7; 3; 8; 5; 9; 6; 10; 1; 11;
            2; 12; 3; 8; 4; 9; 1; 10; 6; 7; 2; 8; 3; 9; 4; 10; 5; 11; 6; 12]
    kl ()
    
    
    
    0
     // return an integer exit code
