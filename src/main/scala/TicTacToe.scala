package TicTacToe


import org.junit.Test
import org.junit.Assert._

/** Each cell in a board can be marked with X (Tic), O (Tac) or be unmarked (Empty)*/
object CellState extends Enumeration {
  type CellState = Value
  val Empty, Tic, Tac = Value
}

/** The possible state of a board
 *  1. TicWins: a line of 3 Xs, witho	ut a row of 3 Os 
 *  {{{
 *     XXX
 *     .OO
 *     O..
 *  }}}
 *  2. TacWins: a line 3 Os, without a row of 3 Xs
 *  {{{
 *     OOO
 *     .XX
 *     X..
 *  }}}
 *  3. NoWinner: the game has not finished yet
 *  {{{
 *     XOO
 *     X..
 *     ...
 *  }}}
 *  4. Tie: the game is finished without a winner
 *  {{{
 *     XOX
 *     XXO
 *     OXO
 *  }}}
 *  5. Invalid: a line of 3 Xs and 3 Os
 *  {{{
 *     XXX
 *     OOO
 *     ...
 *  }}}
 */
object BoardState extends Enumeration {
  type BoardState = Value
  val TicWins, TacWins, NoWinner, Tie, Invalid = Value
}

class TicTacToeBoard {
	 	
	import CellState._	
	import BoardState._
	
	/**
	 *  The board is represented by a 3x3 array
	 */
	var cells = Array.fill[CellState](3,3){Empty}
			
	def apply(i: Int, j:Int) = cells(i)(j)
	def update(i: Int, j:Int, v: CellState) {
	  if (cells(i)(j) == Empty) cells(i)(j) = v else throw new Exception
	}

	/**
	 * Construct a board with an initial state
	 */
	def this(cells: Array[Array[CellState.Value]]) {
		this
		this.cells = cells.clone
	}
	
	override def clone() = new TicTacToeBoard(this.cells)
	
		
	/**
	 *  There are 8 possible lines inside a board:
	 *  {{{
	 *    000 ... ...
	 *    ... 111 ...
	 *    ... ... 222
	 *    
	 *    3.. .4. ..5
	 *    3.. .4. ..5
	 *    3.. .4. ..5
	 *    
	 *    6.. ..7
	 *    .6. .7.
	 *    ..6 7..
	 *  }}}    
	 */
	def line(i: Int): (CellState, CellState, CellState) = {
		i match {
		  case 0 => (this(0,0), this(0,1), this(0,2))
		  case 1 => (this(1,0), this(1,1), this(1,2))
		  case 2 => (this(2,0), this(2,1), this(2,2))
		  case 3 => (this(0,0), this(1,0), this(2,0))
		  case 4 => (this(0,1), this(1,1), this(2,1))
		  case 5 => (this(0,2), this(1,2), this(2,2))
		  case 6 => (this(0,0), this(1,1), this(2,2))
		  case 7 => (this(2,0), this(1,1), this(0,2))			  
		}
	}	
	
	/**
	 * Checks if anyone has won
	 */
	def state: BoardState = {
	  var tic = false // Any winning row for X?
	  var tac = false // Any winning row for O?
	  for (i <- 0 to 7) {
		  if (line(i) == (Tic, Tic, Tic)) {
		    tic = true
		  }
		  if (line(i) == (Tac, Tac, Tac)) {
		    tac = true
		  }
	  }
	  if (tic && !tac) TicWins else
	  if (tac && !tic) TacWins else 
	  if (tic &&  tac) Invalid else { // !tic and !tac
		  var isBoardFull = true
		  for (i <- 0 until 3; 
		       j <- 0 until 3) {
		    if (this(i, j) == Empty) isBoardFull = false
		  }
		  if (isBoardFull) Tie else NoWinner 
	  }	    	  
	}
	
	/**
	 * Prints something as:
	 * {{{
	 * +---+
	 * |X.O|
	 * |OOX|
	 * |X.X|
	 * +---+	 
	 * }}}
	 */
	override def toString: String = {
	  var s = "+---+\n"	  
	  for (i <- 0 until 3) {
	    s += "|"
	    for (j <- 0 until 3) {	      
	      s += (this(i,j) match {
	        case Tic   => "X"
	        case Tac   => "O"
	        case Empty => "."
	        // case _     => throw new Exception
	      })	      
	    }	    
	    s += "|\n"
	  }
	  s += "+---+\n"
	  s
	}
}
  

class TicTacToeBoardTests {
  @Test 
  def test1 {
	var board = new TicTacToeBoard();
	  
	board(0, 0) = CellState.Tic
	board(1, 1) = CellState.Tic
	board(2, 2) = CellState.Tic
	assertTrue(board.state == BoardState.TicWins)	  
  }

  @Test
  def test2 {
    var board = new TicTacToeBoard();

    board(0, 0) = CellState.Tac
    board(1, 1) = CellState.Tac
    board(2, 2) = CellState.Tac
    assertTrue(board.state == BoardState.TacWins)    
  }

  @Test
  def test3 {
    var board = new TicTacToeBoard();
    assertTrue(board.state == BoardState.NoWinner)    
  }
  
  @Test
  def test4 {
    var board = new TicTacToeBoard();
    board(0, 0) = CellState.Tic
    board(0, 1) = CellState.Tac
    board(0, 2) = CellState.Tic
    board(1, 0) = CellState.Tic
    board(1, 1) = CellState.Tic
    board(1, 2) = CellState.Tac
    board(2, 0) = CellState.Tac
    board(2, 1) = CellState.Tic
    board(2, 2) = CellState.Tac

    assertTrue(board.state == BoardState.Tie)    
  }

}
