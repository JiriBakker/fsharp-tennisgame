namespace Jiri.FSharp.TennisGame.UnitTests
open System.Collections.Generic
open NUnit.Framework
open FsUnit
open Jiri.FSharp.TennisGame

[<TestFixture>]
type TennnisGameTests() = 

    let player1 = new Player("Player 1")
    let player2 = new Player("Player 2")
    
    let startTennisGame() =
        new TennisGame(player1, player2)

    let scoreInGame player gameState =
        gameState.CurrentGame.[player]

    let gamesWonInSet player gameState =
        gameState.Sets.Head.Games.[player]

    let setsWon player gameState =
        gameState.Sets
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

    let getDueceGame() =
        let tennisGame = startTennisGame()
        tennisGame |> scorePointsFor player1 3
        tennisGame |> scorePointsFor player2 3        
        tennisGame

    


    [<Test>]
    member this.InNewGameBothPlayersHaveNoPointsInGame () =
        let tennisGame = startTennisGame()
        
        tennisGame.CurrentScore
        |> scoreInGame player1
        |> should equal Zero
        
        tennisGame.CurrentScore
        |> scoreInGame player2
        |> should equal Zero

    [<Test>]
    member this.FirstPointscoreInGameOneShouldGiveScore15 () =
        let tennisGame = startTennisGame()
        
        tennisGame |> scorePointsFor player1 1 |> ignore
        
        tennisGame.CurrentScore
        |> scoreInGame player1
        |> should equal Fifteen

    [<Test>]
    member this.TwoPointsscoreInGameOneShouldGiveScore30 () =
        let tennisGame = startTennisGame()
        
        tennisGame |> scorePointsFor player1 2

        tennisGame.CurrentScore
        |> scoreInGame player1
        |> should equal Thirty

    [<Test>]
    member this.ThreePointsscoreInGameOneShouldGiveScore40 () =
        let tennisGame = startTennisGame()
        
        tennisGame |> scorePointsFor player1 3

        tennisGame.CurrentScore
        |> scoreInGame player1
        |> should equal Forty

    [<Test>]
    member this.FourPointsscoreInGameOneShouldStartsNewGame () =
        let tennisGame = startTennisGame()
        
        tennisGame |> scorePointsFor player1 4        

        tennisGame.CurrentScore
        |> scoreInGame player1
        |> should equal Zero

        tennisGame.CurrentScore
        |> scoreInGame player2
        |> should equal Zero

    [<Test>]
    member this.ThreePointsForBothPlayersShouldGiveScoreDuece () =
        let tennisGame = startTennisGame()
        
        tennisGame |> scorePointsFor player1 3        
        tennisGame |> scorePointsFor player2 3        

        tennisGame.CurrentScore
        |> scoreInGame player1
        |> should equal Deuce

        tennisGame.CurrentScore
        |> scoreInGame player2
        |> should equal Deuce

    [<Test>]
    member this.ScoreInGameOneAtDueceGivesAdvantage () =
        let tennisGame = getDueceGame()

        tennisGame |> scorePointsFor player1 1

        tennisGame.CurrentScore
        |> scoreInGame player1
        |> should equal Advantage

        tennisGame.CurrentScore
        |> scoreInGame player2
        |> should equal Forty

    [<Test>]
    member this.ScoreInGameTwoAtDueceGivesAdvantage () =
        let tennisGame = getDueceGame()

        tennisGame |> scorePointsFor player2 1

        tennisGame.CurrentScore
        |> scoreInGame player1
        |> should equal Forty

        tennisGame.CurrentScore
        |> scoreInGame player2
        |> should equal Advantage

    [<Test>]
    member this.ScoreInGameOneWhenPlayerTwoHasAdvantageGivesDuece () =
        let tennisGame = getDueceGame()
        
        tennisGame |> scorePointsFor player2 1
        tennisGame |> scorePointsFor player1 1

        tennisGame.CurrentScore
        |> scoreInGame player1
        |> should equal Deuce

        tennisGame.CurrentScore
        |> scoreInGame player2
        |> should equal Deuce

    [<Test>]
    member this.ScoreInGameOneAtAdvantageStartsNewGame () =
        let tennisGame = getDueceGame()
        
        tennisGame |> scorePointsFor player1 1
        tennisGame |> scorePointsFor player1 1

        tennisGame.CurrentScore
        |> scoreInGame player1
        |> should equal Zero

        tennisGame.CurrentScore
        |> scoreInGame player2
        |> should equal Zero

    [<Test>]
    member this.InNewGameBothPlayersHaveNogamesWonInSet () =
        let tennisGame = startTennisGame()
        
        tennisGame.CurrentScore
        |> gamesWonInSet player1
        |> should equal 0
        
        tennisGame.CurrentScore
        |> gamesWonInSet player2
        |> should equal 0


    [<Test>]
    member this.ScoreFourPointsInGameScoresGameInSet () =
        let tennisGame = startTennisGame()
        
        tennisGame |> scorePointsFor player1 4

        tennisGame.CurrentScore
        |> gamesWonInSet player1
        |> should equal 1

        tennisGame.CurrentScore
        |> gamesWonInSet player2
        |> should equal 0

    [<Test>]
    member this.WinTwoGamesScoresTwogamesWonInSet () =
        let tennisGame = startTennisGame()
        
        tennisGame |> winGames player1 1
        tennisGame |> winGames player1 1

        tennisGame.CurrentScore
        |> gamesWonInSet player1
        |> should equal 2

    [<Test>]
    member this.WinSixGamesScoresSetWon () =
        let tennisGame = startTennisGame()
        
        tennisGame |> winGames player1 6

        tennisGame.CurrentScore
        |> setsWon player1
        |> should equal 1

    [<Test>]
    member this.WinSixthGameWhenOpponentHasFiveGamesDoesNotEndSet () =
        let tennisGame = startTennisGame()
        
        tennisGame |> winGames player2 5
        tennisGame |> winGames player1 6

        tennisGame.CurrentScore
        |> setsWon player1
        |> should equal 0

    [<Test>]
    member this.WinSeventhGameWhenOpponentHasFiveGamesWinsSet () =
        let tennisGame = startTennisGame()
        
        tennisGame |> winGames player2 5
        tennisGame |> winGames player1 7

        tennisGame.CurrentScore
        |> setsWon player1
        |> should equal 1

    [<Test>]
    member this.SetContinuesWhileDifferenceInGamesIsNot2 () =
        let tennisGame = startTennisGame()
        
        tennisGame |> winGames player2 5
        tennisGame |> winGames player1 5

        for i = 1 to 10 do
            tennisGame |> winGames player1 1
            tennisGame |> winGames player2 1

        tennisGame.CurrentScore
        |> setsWon player1
        |> should equal 0        