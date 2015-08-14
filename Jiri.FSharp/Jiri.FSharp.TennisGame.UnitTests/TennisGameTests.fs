namespace Jiri.FSharp.TennisGame.UnitTests
open System.Collections.Generic
open NUnit.Framework
open FsUnit
open Jiri.FSharp.TennisGame

[<TestFixture>]
type TennnisGameTests() = 

    
    let startTennisGame() =
        new TennisGame()

    let scoreInCurrentGameFor player (tennisGame:TennisGame) =
        tennisGame.GameState.CurrentGame.[player]

    let gamesWonInSet player (tennisGame:TennisGame) =
        tennisGame.GameState.Sets.Head.Games.[player]

    let setsWon player (tennisGame:TennisGame) =
        tennisGame.GameState.Sets
        |> Seq.filter (fun (set) -> match set.Winner with | None -> false | Some(p) -> p.Key = player)
        |> Seq.length
       
    let scorePointsFor player points (tennisGame:TennisGame) =
        for i = 1 to points do
            tennisGame.ScorePointFor player       

    let winGame player tennisGame =
        scorePointsFor player 4 tennisGame

    let winGames player games tennisGame =
        for i = 1 to games do
            winGame player tennisGame

    let getDeuceGame() =
        let tennisGame = startTennisGame()
        tennisGame |> scorePointsFor Player1 3
        tennisGame |> scorePointsFor Player2 3        
        tennisGame   


    [<Test>]
    member this.InNewGameBothPlayersHaveNoPointsInGame () =
        let tennisGame = startTennisGame()
        
        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Zero
        
        tennisGame
        |> scoreInCurrentGameFor Player2
        |> should equal Zero

    [<Test>]
    member this.FirstPointscoreInGameOneShouldGiveScore15 () =
        let tennisGame = startTennisGame()
        
        tennisGame |> scorePointsFor Player1 1 |> ignore
        
        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Fifteen

    [<Test>]
    member this.TwoPointsscoreInGameOneShouldGiveScore30 () =
        let tennisGame = startTennisGame()
        
        tennisGame |> scorePointsFor Player1 2

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Thirty

    [<Test>]
    member this.ThreePointsscoreInGameOneShouldGiveScore40 () =
        let tennisGame = startTennisGame()
        
        tennisGame |> scorePointsFor Player1 3

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Forty

    [<Test>]
    member this.FourPointsscoreInGameOneShouldStartsNewGame () =
        let tennisGame = startTennisGame()
        
        tennisGame |> scorePointsFor Player1 4        

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Zero

        tennisGame
        |> scoreInCurrentGameFor Player2
        |> should equal Zero

    [<Test>]
    member this.TwoPointsForPlayer1And1PointForPlayer2ShouldGiveScoreThirtyFifteen () =
        let tennisGame = startTennisGame()
        
        tennisGame |> scorePointsFor Player1 2        
        tennisGame |> scorePointsFor Player2 1        

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Thirty

        tennisGame
        |> scoreInCurrentGameFor Player2
        |> should equal Fifteen

    [<Test>]
    member this.ThreePointsForBothPlayersShouldGiveScoreDuece () =
        let tennisGame = startTennisGame()
        
        tennisGame |> scorePointsFor Player1 3        
        tennisGame |> scorePointsFor Player2 3        

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Deuce

        tennisGame
        |> scoreInCurrentGameFor Player2
        |> should equal Deuce

    [<Test>]
    member this.ScoreInGameOneAtDueceGivesAdvantage () =
        let tennisGame = getDeuceGame()

        tennisGame |> scorePointsFor Player1 1

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Advantage

        tennisGame
        |> scoreInCurrentGameFor Player2
        |> should equal Forty

    [<Test>]
    member this.ScoreInGameTwoAtDueceGivesAdvantage () =
        let tennisGame = getDeuceGame()

        tennisGame |> scorePointsFor Player2 1

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Forty

        tennisGame
        |> scoreInCurrentGameFor Player2
        |> should equal Advantage

    [<Test>]
    member this.ScoreInGameOneWhenPlayerTwoHasAdvantageGivesDuece () =
        let tennisGame = getDeuceGame()
        
        tennisGame |> scorePointsFor Player2 1
        tennisGame |> scorePointsFor Player1 1

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Deuce

        tennisGame
        |> scoreInCurrentGameFor Player2
        |> should equal Deuce

    [<Test>]
    member this.ScoreInGameOneAtAdvantageStartsNewGame () =
        let tennisGame = getDeuceGame()
        
        tennisGame |> scorePointsFor Player1 1
        tennisGame |> scorePointsFor Player1 1

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Zero

        tennisGame
        |> scoreInCurrentGameFor Player2
        |> should equal Zero

    [<Test>]
    member this.InNewGameBothPlayersHaveNogamesWonInSet () =
        let tennisGame = startTennisGame()
        
        tennisGame
        |> gamesWonInSet Player1
        |> should equal 0
        
        tennisGame
        |> gamesWonInSet Player2
        |> should equal 0


    [<Test>]
    member this.ScoreFourPointsInGameScoresGameInSet () =
        let tennisGame = startTennisGame()
        
        tennisGame |> scorePointsFor Player1 4

        tennisGame
        |> gamesWonInSet Player1
        |> should equal 1

        tennisGame
        |> gamesWonInSet Player2
        |> should equal 0

    [<Test>]
    member this.WinTwoGamesScoresTwogamesWonInSet () =
        let tennisGame = startTennisGame()
        
        tennisGame |> winGames Player1 1
        tennisGame |> winGames Player1 1

        tennisGame
        |> gamesWonInSet Player1
        |> should equal 2

    [<Test>]
    member this.WinSixGamesScoresSetWon () =
        let tennisGame = startTennisGame()
        
        tennisGame |> winGames Player1 6

        tennisGame
        |> setsWon Player1
        |> should equal 1

    [<Test>]
    member this.WinSixthGameWhenOpponentHasFiveGamesDoesNotEndSet () =
        let tennisGame = startTennisGame()
        
        tennisGame |> winGames Player2 5
        tennisGame |> winGames Player1 6

        tennisGame
        |> setsWon Player1
        |> should equal 0

    [<Test>]
    member this.WinSeventhGameWhenOpponentHasFiveGamesWinsSet () =
        let tennisGame = startTennisGame()
        
        tennisGame |> winGames Player2 5
        tennisGame |> winGames Player1 7

        tennisGame
        |> setsWon Player1
        |> should equal 1

    [<Test>]
    member this.SetContinuesWhileDifferenceInGamesIsNot2 () =
        let tennisGame = startTennisGame()
        
        tennisGame |> winGames Player2 5
        tennisGame |> winGames Player1 5

        for i = 1 to 10 do
            tennisGame |> winGames Player1 1
            tennisGame |> winGames Player2 1

        tennisGame
        |> setsWon Player1
        |> should equal 0        