import scala.io.StdIn.readLine

/**
 * provides values and variables needed for the game
 */
package object tictactoe {
  val playerXsymbol = 'X'
  val playerOsymbol = 'O'
  val playerXName = readLine(s"You will be player X. What is your name? ")
  val playerOName = readLine(s"You will be player O. What is your name? ")
  val winningCombo = Array((0,1,2), (3,4,5), (6,7,8), //Horizontal
    (0,3,6), (1,4,7), (2,5,8), //Vertical
    (0,4,8), (2,4,6)) //Diagonal
  //val initialBoardNumbers = Array('1', '2', '3', '4', '5', '6', '7', '8','9')
  var boardNumbers = Array('1', '2', '3', '4', '5', '6', '7', '8','9')
  var response = "yes"
}

package tictactoe {

  import tictactoe.TicTac.mainPlay

  class GameBoard(classBoard:Array[Char] = boardNumbers) {

    /**
     * Printing array in a form of 3x3 game board
     */
    def showBoard {
      println("\t\t\t " + classBoard(0) + " | " + classBoard(1) + " | " + classBoard(2))
      println("\t\t\t " + classBoard(3) + " | " + classBoard(4) + " | " + classBoard(5))
      println("\t\t\t " + classBoard(6) + " | " + classBoard(7) + " | " + classBoard(8))
    }

    //the basis for this part of our code has been found here:
    //https://titanwolf.org/Network/Articles/Article?AID=bf42c070-bf2c-4848-af8d-f2cddf310e28
    /**
     * Updating board (array) by replacing player X chosen number index with symbol X
     */
    def playerXplays(move: Char) = new GameBoard(classBoard.updated(classBoard.indexOf(move), playerXsymbol))
    /**
     * Updating board (array) by replacing player Y chosen number index with symbol X
     */
    def playerOplays(move: Char) = new GameBoard(classBoard.updated(classBoard.indexOf(move), playerOsymbol)) // replaced the computer move with 2nd player move
    /**
     * Checking if any of the winning combinations exists on the current board
     */
    def winner(winner: Char) =
      winningCombo.exists{case (i,j,k) => classBoard(i) == winner && classBoard(j) == winner && classBoard(k) == winner}

    /**
     * Checking is there is a tie (if all the board (array) characters have been updated and replaced with symbols, but nobody has won yet)
     */
    def isTie:Boolean = {
      classBoard(0) !='1' &&
        classBoard(1) !='2' &&
        classBoard(2) !='3' &&
        classBoard(3) !='4' &&
        classBoard(4) !='5' &&
        classBoard(5) !='6' &&
        classBoard(6) !='7' &&
        classBoard(7) !='8' &&
        classBoard(8) !='9'}

    /**
     * Checking which numbers are still available (not replaced with symbols) to be picked by players
     * @return a new array consisting of available elements
     */
    def movesLeft: Array[Char] = classBoard.filter(n => n != playerXsymbol && n != playerOsymbol)

    /**
     * Checks if game is over. It happens when any of the players wins or there is a tie
     */
    def isGameOver: Boolean = winner(playerOsymbol) || winner(playerXsymbol) || isTie

    /**
     * Congratulating the winning player or prints out a line that there's no winner
     */
    def gameOverPrint {
      if (winner(playerXsymbol)) println(s"\tCongratulations, $playerXName, you won!")
      else if (winner(playerOsymbol)) println(s"\tCongratulations, $playerOName, you won!")
      else if (isTie) println("\tTie - there is no winner this time.")
    }

    /**
     * Resetting the game board and game to its initial state, player X having the first turn
     */
    def resetBoard:Unit = {
      mainPlay(new GameBoard(),playerXsymbol)
    }

    /**
     * Offers to play again and reads the player's input
     */
    def playAgain(): Boolean = {
      response = readLine("DO YOU WANT TO PLAY AGAIN? TYPE Y OR N -> ")
      if (response == "Y" || response == "y") {
        println("\t\tGreat! Let's start again!")
        return true
      }
      else if (response == "N" || response == "n") {
        println("  Thanks for playing. See you next time!")
        //break
        return false
      }
      else {
        println("\tPlease choose between 'Y' or 'N' ")
        playAgain()
      }
    }
  }

  object TicTac extends App {
    import scala.io.StdIn.readChar

    println("\t\tLET'S PLAY TIC-TAC-TOE!")

    /**
     * a single match of tic tac toe
     */
    def mainPlay(board: GameBoard, turn: Char) {

      /**
       * Offers a player to pick a number, checks if it's available on the board,
       * if not - re-offers to pick a number
       */
      def clampMove(): Char = {
        print("Choose a number from the board above -> ")
        val validNumbers = board.movesLeft
        val move = readChar
        if (validNumbers.contains(move)) {
          move
        } else {
          println(s"Wrong number! Choose another one from available options")
          clampMove()
        }
      }

      /**
       * @gameStat variable we need in order to break the loop from playAgain function (when player answers N)
       * @playAg another variable we created
       * @return the game board is reset and printed out
       */
      var gameStat = false
      def restartingGame(): Unit = {
        if (board.isGameOver) {
          board.gameOverPrint
          var playAg = board.playAgain()
          if (playAg == true) {
            board.resetBoard
            //board.showBoard
            gameStat = true
          } else if (playAg == false) {gameStat = true}
        }
      }

      println
      board.showBoard
      restartingGame()


      if (turn == playerXsymbol && gameStat == false) {
        println("\t  ***  PLAYER X TURN  ***")
        val updatedBoard = board.playerXplays(clampMove)
        mainPlay(updatedBoard, playerOsymbol)
      } else if (turn == playerOsymbol && gameStat == false) {
        println("\t  ***  PLAYER O TURN  ***")
        val updatedBoard = board.playerOplays(clampMove)
        mainPlay(updatedBoard, playerXsymbol)
      }

    }
    /**
     * Calling the function, player X having the first turn
     */
    mainPlay(new GameBoard(),playerXsymbol)
  }
}