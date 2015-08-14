namespace Jiri.FSharp.TennisGame

type Player = Player1 | Player2
    with override this.ToString() =
            sprintf "%A" this
    
type ScoreInGame = ZeroZero | FifteenZero | ThirtyZero | FortyZero | ZeroFifteen | FifteenFifteen | ThirtyFifteen | FortyFifteen | ZeroThirty | FifteenThirty | ThirtyThirty | FortyThirty | ZeroForty | FifteenForty | ThirtyForty | Deuce | AdvantagePlayer1 | AdvantagePlayer2
    with override this.ToString() =
            sprintf "%A" this

type Set = { GamesWonPlayer1:int; GamesWonPlayer2:int }
    with member this.Winner =
            match this.GamesWonPlayer1,this.GamesWonPlayer2 with
            | a,b when a >= 6 && a-b >= 2 -> Some(Player1)
            | a,b when b >= 6 && b-a >= 2 -> Some(Player2)
            // TODO tie-breaks?
            // | 7,6 -> Some(Player1) 
            // | 6,7 -> Some(Player2)
            | _,_ -> None
    
type GameState = { Sets: Set list; CurrentGame: ScoreInGame }

module TennisGame = 

    let private startNewSet =
        { GamesWonPlayer1 = 0; GamesWonPlayer2 = 0 }

    let private getCurrentSet gameState = 
        gameState.Sets.Head

    let private getPreviousSets gameState =
        gameState.Sets.Tail

    let private incrementGamesWonInSet player set =
        match player with
            | Player1 -> { GamesWonPlayer1 = set.GamesWonPlayer1 + 1; GamesWonPlayer2 = set.GamesWonPlayer2     }
            | Player2 -> { GamesWonPlayer1 = set.GamesWonPlayer1;     GamesWonPlayer2 = set.GamesWonPlayer2 + 1 }        
        
    let private winGame player gameState =
        let updatedSet = 
            gameState
            |> getCurrentSet 
            |> incrementGamesWonInSet player

        let previousSets = getPreviousSets gameState
        match updatedSet.Winner with
            | Some(player) -> { Sets = [startNewSet;updatedSet] @ previousSets; CurrentGame = ZeroZero }
            | None         -> { Sets = [updatedSet]             @ previousSets; CurrentGame = ZeroZero }  

    let scorePointFor player gameState =
        let scoreInCurrentGame = gameState.CurrentGame

        let (updatedScoreInCurrentGame,gameWinner) =
            match player with
                | Player1 ->
                    match scoreInCurrentGame with
                        | FortyZero        
                        | FortyFifteen     
                        | FortyThirty    
                        | AdvantagePlayer1 -> ZeroZero,Some(Player1)

                        | ThirtyForty     
                        | AdvantagePlayer2 -> Deuce,None

                        | ZeroZero         -> FifteenZero,None
                        | FifteenZero      -> ThirtyZero,None
                        | ThirtyZero       -> FortyZero,None
                        | ZeroFifteen      -> FifteenFifteen,None
                        | FifteenFifteen   -> ThirtyFifteen,None
                        | ThirtyFifteen    -> FortyFifteen,None                        
                        | ZeroThirty       -> FifteenThirty,None
                        | FifteenThirty    -> ThirtyThirty,None
                        | ThirtyThirty     -> FortyThirty,None                          
                        | ZeroForty        -> FifteenForty,None
                        | FifteenForty     -> ThirtyForty,None
                        | Deuce            -> AdvantagePlayer1,None
                | Player2 ->
                    match scoreInCurrentGame with
                        | ZeroForty        
                        | FifteenForty     
                        | ThirtyForty    
                        | AdvantagePlayer2 -> ZeroZero,Some(Player2)

                        | FortyThirty     
                        | AdvantagePlayer1 -> Deuce,None

                        | ZeroZero         -> ZeroFifteen,None
                        | FifteenZero      -> FifteenFifteen,None
                        | ThirtyZero       -> ThirtyFifteen,None
                        | ZeroFifteen      -> ZeroThirty,None
                        | FifteenFifteen   -> FifteenThirty,None
                        | ThirtyFifteen    -> ThirtyThirty,None                        
                        | ZeroThirty       -> ZeroForty,None
                        | FifteenThirty    -> FifteenForty,None
                        | ThirtyThirty     -> ThirtyForty,None                          
                        | FortyZero        -> FortyFifteen,None
                        | FortyFifteen     -> FortyThirty,None
                        | Deuce            -> AdvantagePlayer2,None    
                       

        match gameWinner with
            | Some(winner) -> winGame winner gameState            
            | None         -> { Sets = gameState.Sets; CurrentGame = updatedScoreInCurrentGame }

    let startNewTennisGame =
        { Sets = [startNewSet]; CurrentGame = ZeroZero }      