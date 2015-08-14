namespace Jiri.FSharp.TennisGame.UnitTests
open System.Collections.Generic
open NUnit.Framework
open FsUnit
open Jiri.FSharp.TennisGame

[<TestFixture>]
type TennnisGameTests() = 
    
    let startTennisGame() =
        TennisGame.startNewTennisGame

    let scoreInCurrentGameFor player (tennisGame:GameState) =
        tennisGame.CurrentGame.[player]

    let gamesWonInSet player (tennisGame:GameState) =
        tennisGame.Sets.Head.Games.[player]

    let setsWon player (tennisGame:GameState) =
        tennisGame.Sets
        |> Seq.filter (fun (set) -> match set.Winner with | None -> false | Some(p) -> p.Key = player)
        |> Seq.length
       
    let rec scorePointsFor player points (tennisGame:GameState) =
        if points = 0 then tennisGame
        else 
            tennisGame
            |> TennisGame.scorePointFor player 
            |> scorePointsFor player (points - 1)

    let winGame player tennisGame =
        tennisGame
        |> scorePointsFor player 4 

    let rec winGames player games tennisGame =
        if games = 0 then tennisGame
        else 
            tennisGame
            |> winGame player 
            |> winGames player (games - 1)        

    let getDeuceGame() =
        startTennisGame() 
        |> scorePointsFor Player1 3
        |> scorePointsFor Player2 3                

    let rec winGamesForBothPlayers games (tennisGame:GameState) =
        if games = 0 then tennisGame
        else
            tennisGame
            |> winGame Player1
            |> winGame Player2
            |> winGamesForBothPlayers (games - 1)

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
        let tennisGame = 
            startTennisGame()
            |> scorePointsFor Player1 1 

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Fifteen

    [<Test>]
    member this.TwoPointsscoreInGameOneShouldGiveScore30 () =
        let tennisGame = 
            startTennisGame() 
            |> scorePointsFor Player1 2

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Thirty

    [<Test>]
    member this.ThreePointsscoreInGameOneShouldGiveScore40 () =
        let tennisGame = 
            startTennisGame() 
            |> scorePointsFor Player1 3

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Forty

    [<Test>]
    member this.FourPointsscoreInGameOneShouldStartsNewGame () =
        let tennisGame = 
            startTennisGame() 
            |> scorePointsFor Player1 4        

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Zero

        tennisGame
        |> scoreInCurrentGameFor Player2
        |> should equal Zero

    [<Test>]
    member this.TwoPointsForPlayer1And1PointForPlayer2ShouldGiveScoreThirtyFifteen () =
        let tennisGame = 
            startTennisGame() 
            |> scorePointsFor Player1 2        
            |> scorePointsFor Player2 1        

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Thirty

        tennisGame
        |> scoreInCurrentGameFor Player2
        |> should equal Fifteen

    [<Test>]
    member this.ThreePointsForBothPlayersShouldGiveScoreDuece () =
        let tennisGame = 
            startTennisGame()        
            |> scorePointsFor Player1 3        
            |> scorePointsFor Player2 3        

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Deuce

        tennisGame
        |> scoreInCurrentGameFor Player2
        |> should equal Deuce

    [<Test>]
    member this.ScoreInGameOneAtDueceGivesAdvantage () =
        let tennisGame = 
            getDeuceGame()
            |> scorePointsFor Player1 1

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Advantage

        tennisGame
        |> scoreInCurrentGameFor Player2
        |> should equal Forty

    [<Test>]
    member this.ScoreInGameTwoAtDueceGivesAdvantage () =
        let tennisGame = 
            getDeuceGame()
            |> scorePointsFor Player2 1

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Forty

        tennisGame
        |> scoreInCurrentGameFor Player2
        |> should equal Advantage

    [<Test>]
    member this.ScoreInGameOneWhenPlayerTwoHasAdvantageGivesDuece () =
        let tennisGame = 
            getDeuceGame()        
            |> scorePointsFor Player2 1
            |> scorePointsFor Player1 1

        tennisGame
        |> scoreInCurrentGameFor Player1
        |> should equal Deuce

        tennisGame
        |> scoreInCurrentGameFor Player2
        |> should equal Deuce

    [<Test>]
    member this.ScoreInGameOneAtAdvantageStartsNewGame () =
        let tennisGame = 
            getDeuceGame()        
            |> scorePointsFor Player1 1
            |> scorePointsFor Player1 1

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
        let tennisGame = 
            startTennisGame()
            |> scorePointsFor Player1 4

        tennisGame
        |> gamesWonInSet Player1
        |> should equal 1

        tennisGame
        |> gamesWonInSet Player2
        |> should equal 0

    [<Test>]
    member this.WinTwoGamesScoresTwogamesWonInSet () =
        let tennisGame =         
            startTennisGame() 
            |> winGames Player1 1
            |> winGames Player1 1

        tennisGame
        |> gamesWonInSet Player1
        |> should equal 2

    [<Test>]
    member this.WinSixGamesScoresSetWon () =
        let tennisGame =         
            startTennisGame() 
            |> winGames Player1 6

        tennisGame
        |> setsWon Player1
        |> should equal 1

    [<Test>]
    member this.WinSixthGameWhenOpponentHasFiveGamesDoesNotEndSet () =
        let tennisGame = 
            startTennisGame()        
            |> winGames Player2 5
            |> winGames Player1 6

        tennisGame
        |> setsWon Player1
        |> should equal 0

    [<Test>]
    member this.WinSeventhGameWhenOpponentHasFiveGamesWinsSet () =
        let tennisGame = 
            startTennisGame()
            |> winGames Player2 5
            |> winGames Player1 7

        tennisGame
        |> setsWon Player1
        |> should equal 1

    [<Test>]
    member this.SetContinuesWhileDifferenceInGamesIsNot2 () =
        let tennisGame = 
            startTennisGame()        
            |> winGames Player2 5
            |> winGames Player1 5
            |> winGamesForBothPlayers 10

        tennisGame
        |> setsWon Player1
        |> should equal 0        