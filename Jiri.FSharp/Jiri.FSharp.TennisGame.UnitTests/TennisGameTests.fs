namespace Jiri.FSharp.TennisGame.UnitTests
open System.Collections.Generic
open NUnit.Framework
open FsUnit
open Jiri.FSharp.TennisGame

[<TestFixture>]
type TennnisGameTests() = 
    
    let startTennisGame() =
        TennisGame.startNewTennisGame

    let scoreInCurrentGame (tennisGame:GameState) =
        tennisGame.CurrentGame

    let gamesWonInSet player (tennisGame:GameState) =
        match player with
        | Player1 -> tennisGame.Sets.Head.GamesWonPlayer1
        | Player2 -> tennisGame.Sets.Head.GamesWonPlayer2

    let setsWon player (tennisGame:GameState) =
        tennisGame.Sets
        |> Seq.filter (fun (set) -> match set.Winner with | None -> false | Some(p) -> p = player)
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
        |> scoreInCurrentGame
        |> should equal ZeroZero

    [<Test>]
    member this.FirstPointscoreInGameOneShouldGiveScore15 () =
        let tennisGame = 
            startTennisGame()
            |> scorePointsFor Player1 1 

        tennisGame
        |> scoreInCurrentGame
        |> should equal FifteenZero

    [<Test>]
    member this.TwoPointsscoreInGameOneShouldGiveScore30 () =
        let tennisGame = 
            startTennisGame() 
            |> scorePointsFor Player1 2

        tennisGame
        |> scoreInCurrentGame
        |> should equal ThirtyZero

    [<Test>]
    member this.ThreePointsscoreInGameOneShouldGiveScore40 () =
        let tennisGame = 
            startTennisGame() 
            |> scorePointsFor Player1 3

        tennisGame
        |> scoreInCurrentGame
        |> should equal FortyZero

    [<Test>]
    member this.FourPointsscoreInGameOneShouldStartsNewGame () =
        let tennisGame = 
            startTennisGame() 
            |> scorePointsFor Player1 4        

        tennisGame
        |> scoreInCurrentGame
        |> should equal ZeroZero

    [<Test>]
    member this.TwoPointsForPlayer1And1PointForPlayer2ShouldGiveScoreThirtyFifteen () =
        let tennisGame = 
            startTennisGame() 
            |> scorePointsFor Player1 2        
            |> scorePointsFor Player2 1        

        tennisGame
        |> scoreInCurrentGame
        |> should equal ThirtyFifteen

    [<Test>]
    member this.ThreePointsForBothPlayersShouldGiveScoreDuece () =
        let tennisGame = 
            startTennisGame()        
            |> scorePointsFor Player1 3        
            |> scorePointsFor Player2 3        

        tennisGame
        |> scoreInCurrentGame
        |> should equal Deuce        

    [<Test>]
    member this.ScoreInGameOneAtDueceGivesAdvantage () =
        let tennisGame = 
            getDeuceGame()
            |> scorePointsFor Player1 1

        tennisGame
        |> scoreInCurrentGame
        |> should equal AdvantagePlayer1


    [<Test>]
    member this.ScoreInGameTwoAtDueceGivesAdvantage () =
        let tennisGame = 
            getDeuceGame()
            |> scorePointsFor Player2 1

        tennisGame
        |> scoreInCurrentGame
        |> should equal AdvantagePlayer2

    [<Test>]
    member this.ScoreInGameOneWhenPlayerTwoHasAdvantageGivesDuece () =
        let tennisGame = 
            getDeuceGame()        
            |> scorePointsFor Player2 1
            |> scorePointsFor Player1 1

        tennisGame
        |> scoreInCurrentGame
        |> should equal Deuce

    [<Test>]
    member this.ScoreInGameOneAtAdvantageStartsNewGame () =
        let tennisGame = 
            getDeuceGame()        
            |> scorePointsFor Player1 1
            |> scorePointsFor Player1 1

        tennisGame
        |> scoreInCurrentGame
        |> should equal ZeroZero


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