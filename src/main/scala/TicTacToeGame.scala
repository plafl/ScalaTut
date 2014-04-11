package TicTacToe

import java.net.{Socket, ServerSocket}
import java.io._
import scala.actors.Actor
import scala.actors.Actor._


/***
 * Communicates socket with game
 */
class PlayerInterface(val id:Int, val game: TicTacToeGame, val conn: Socket) extends Actor {      
  
  val inp = new BufferedReader(new InputStreamReader(conn.getInputStream))
  val out = new PrintWriter(conn.getOutputStream, true)
  
  out.println("Connected as player " + id)
  
  def move () {
	var readMove = true
	while (readMove) {
	    out.println("Your move: ")
	    val move = inp.readLine().split("\\s")
	    if (move.length == 2) { 	        	  	          	          
	      try {	          
	    	  val row = move(0).toInt
	    	  val col = move(1).toInt
	    	  
	    	  if (row >=1 && row <= 3 && col >=1 && col <=3) {
	    	    readMove = false
	    	    game ! (id, row - 1, col - 1)
	    	  }
	        }
	        catch {
	          case _ => {} // do nothing	            	
	        }
	        if (readMove) {
	          out.println("Could not understand your movement. Try again.")
	        }
	      }
	  }
  }
  
  def act() {      
    var play = true
    while (play) {      
      receive {
        case (board: TicTacToeBoard, "WIN") => {
          out.println(board.toString)
          out.println("You are the best!")
          play = false
        }
        case (board: TicTacToeBoard, "TIE") => {
          out.println(board.toString)
          out.println("Game is a tie")
          play = false
        }
        case (board: TicTacToeBoard, "LOOSE") => {
          out.println(board.toString)
          out.println("Sucker")
          play = false
        }
        case "ERROR" => {          
          out.println("Unexpected error. Exit 1.")
          play = false
        }
        
        case (board: TicTacToeBoard, "WAIT") => {
          out.println(board.toString)
          out.println("Waiting other player...")
        }
        case (board: TicTacToeBoard, "MOVE") => {
          out.println(board.toString)          
          move()
        }
        case (board: TicTacToeBoard, "INCORRECT") => {
          out.println("Incorrect movement")
        }
        
        case other => {
          out.println("Unexpected error. Exit 2.")
          play = false
        }
      }      
    }
  }
}


class TicTacToeGame(val port:Int = 5000) extends Actor {
  import CellState._
  import BoardState._
  
  val board = new TicTacToeBoard  
  val connection = new ServerSocket(port)

  
  println("Waiting player 1...")
  val p1 = new PlayerInterface(1, this, connection.accept())
  println("Waiting player 2...")
  val p2 = new PlayerInterface(2, this, connection.accept())
         
  def act() {
    println("Game started!")
    p1.start
    p2.start
        
    var next = p1 // next player
    var wait = p2 // waiting player
    
    var play = true // play while true  
    while (play) {
      wait ! (board.clone, "WAIT")
      
      // repeat until the player makes a correct movement
      var move = true
      while (move) {
        next ! (board.clone, "MOVE")
        receive {
          case (id: Int, row: Int, col: Int) => {
            if (next.id == id) { // ignore other player
              try {
                board(row, col) = if (next == p1) Tic else Tac
                move = false
              } catch {
                case _ => next ! (board.clone, "INCORRECT")
              }
            }
          }
        }
      }
          
      // check for end of game
      board.state match {
        case TicWins => {
          p1 ! (board.clone, "WIN") 
          p2 ! (board.clone, "LOOSE")
          play = false
        }
        case TacWins => {
          p2 ! (board.clone, "WIN") 
          p1 ! (board.clone, "LOOSE")
          play = false
        }
        case Tie     => {
          p1 ! (board.clone, "TIE") 
          p2 ! (board.clone, "TIE")
          play = false
        }            
        case Invalid => {
          p1 ! (board.clone, "ERROR")
          p2 ! (board.clone, "ERROR")
          play = false
        }
        case NoWinner => {} // just execute next iteration, play = true
      }      
      // switch players
      next = if (next == p1) p2 else p1
      wait = if (next == p1) p2 else p1
    }
    println("Gameover")
  }
}

object Server extends App {  
    val game = new TicTacToeGame
    game.start      
}

