namespace Jiri.FSharp.TennisGame
open System.Collections.Generic

module Seq =
    let asMap (sequence:seq<'a * 'b>) =
        sequence |> Map.ofSeq

type Player = Player1 | Player2
    
type Score = Zero | Fifteen | Thirty | Forty | Deuce | Advantage

type Set = { Games: Map<Player,int> }
    with member this.Winner =
            let Player1 = Seq.head this.Games
            let Player2 = Seq.head (Seq.tail this.Games)
        
            match Player1.Value,Player2.Value with
            | a,b when a >= 6 && a-b >= 2 -> Some(Player1)
            | a,b when b >= 6 && b-a >= 2 -> Some(Player2)
            // TODO tie-breaks?
            // | 7,6 -> Some(Player1) 
            // | 6,7 -> Some(Player2)
            | _,_ -> None
    
type GameState = { Sets: Set list; CurrentGame: Map<Player,Score> }

type TennisGame() = 

    let startNewGame =
        Seq.asMap [(Player1, Zero); (Player2, Zero)]    

    let startNewSet =
        { Games = Seq.asMap [(Player1, 0);(Player2, 0)] }

    let getOtherPlayer (player:Player) =        
        match player with
        | Player1 -> Player2
        | Player2 -> Player1

    let getCurrentSet gameState = 
        gameState.Sets.Head

    let getPreviousSets gameState =
        gameState.Sets.Tail

    let getGamesWonInSet player set =
        set.Games.[player]        

    let incrementGamesWonInSet player set =
        let otherPlayer              = getOtherPlayer player
        let playerGamesWonInSet      = getGamesWonInSet player set
        let otherPlayerGamesWonInSet = getGamesWonInSet otherPlayer set
        { Games = Seq.asMap [(player,playerGamesWonInSet+1);(otherPlayer,otherPlayerGamesWonInSet)] }
        
    let winGame player gameState =
        let updatedSet = 
            getCurrentSet gameState
            |> incrementGamesWonInSet player

        let previousSets = getPreviousSets gameState
        match updatedSet.Winner with
        | Some(player) -> { Sets = [startNewSet;updatedSet] @ previousSets; CurrentGame = startNewGame }
        | None         -> { Sets = [updatedSet]             @ previousSets; CurrentGame = startNewGame }  

    let incrementScoreInNormalGame player gameState =
        let pointsInCurrentGame = gameState.CurrentGame
        let otherPlayer         = getOtherPlayer player
        let scoreForPlayer      = pointsInCurrentGame.[player]
        let scoreForOtherPlayer = pointsInCurrentGame.[otherPlayer]

        let (scorePlayer,scoreOtherPlayer,gameWinner) =
            match scoreForPlayer,scoreForOtherPlayer with
                | Zero,otherScore    -> Fifteen,otherScore,None
                | Fifteen,otherScore -> Thirty,otherScore,None
                | Thirty,otherScore  -> 
                    match otherScore with
                        | Zero
                        | Fifteen
                        | Thirty -> Forty,otherScore,None
                        | Forty  -> Deuce,Deuce,None
                        | _      -> invalidOp "Unexpected game state"
                | Forty,otherScore ->
                    match otherScore with
                        | Zero
                        | Fifteen
                        | Thirty    -> Forty,otherScore,Some(player)
                        | Advantage -> Deuce,Deuce,None
                        | _         -> invalidOp "Unexpected game state"
                | Deuce,Deuce     -> Advantage,Forty,None
                | Deuce,_         -> invalidOp "Unexpected game state"
                | Advantage,Forty -> Advantage,Forty,Some(player)
                | _,_             -> invalidOp "Unexpected game state"

        match gameWinner with
        | Some(winner) -> 
            winGame winner gameState            
        | None ->
            { Sets = gameState.Sets; CurrentGame = (Seq.asMap [(player,scorePlayer);(otherPlayer,scoreOtherPlayer)]) }

    let winGame player =
        player |> ignore

    let scorePointFor player gameState =
        incrementScoreInNormalGame player gameState

    let mutable gameState = { Sets = [startNewSet]; CurrentGame = startNewGame }

    member this.ScorePointFor player =
        gameState <-
            scorePointFor player gameState            

    member this.CurrentScore =
        gameState