namespace Jiri.FSharp.TennisGame
open System.Collections.Generic

module Seq =
    let asMap (sequence:seq<'a * 'b>) =
        sequence |> Map.ofSeq

type Player(name) = 
    member this.Name = name

    override this.Equals(otherObj) =
        match otherObj with
        | :? Player as y -> (this.Name = y.Name)
        | _ -> false
 
    override this.GetHashCode() = hash this.Name
    interface System.IComparable with
        member this.CompareTo otherObj =
            match otherObj with
            | :? Player as y -> compare this.Name y.Name
            | _ -> invalidArg "yobj" "cannot compare values of different types"
    
type Score = Zero | Fifteen | Thirty | Forty | Duece | Advantage

type Set = { Games: Map<Player,int> }
    with member this.Winner =
            let player1 = Seq.head this.Games
            let player2 = Seq.head (Seq.tail this.Games)
        
            match player1.Value,player2.Value with
            | a,b when a >= 6 && a-b >= 2 -> Some(player1)
            | a,b when b >= 6 && b-a >= 2 -> Some(player2)
            // TODO tie-breaks?
            // | 7,6 -> Some(player1) 
            // | 6,7 -> Some(player2)
            | _,_ -> None
    
type GameState = { Sets: Set list; CurrentGame: Map<Player,Score> }

type TennisGame(player1:Player, player2:Player) = 

    let startNewGame =
        Seq.asMap [(player1, Zero); (player2, Zero)]    

    let startNewSet =
        { Games = Seq.asMap [(player1,0);(player2,0)] }

    let getOtherPlayer (player:Player) =        
        match player with
        | _ when player1.Name = player.Name -> player2
        | _                                 -> player1 

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
                        | Forty  -> Duece,Duece,None
                        | _      -> invalidOp "Unexpected game state"
                | Forty,otherScore ->
                    match otherScore with
                        | Zero
                        | Fifteen
                        | Thirty    -> Forty,otherScore,Some(player)
                        | Advantage -> Duece,Duece,None
                        | _         -> invalidOp "Unexpected game state"
                | Duece,Duece     -> Advantage,Forty,None
                | Duece,_         -> invalidOp "Unexpected game state"
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