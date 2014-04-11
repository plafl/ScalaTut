To compile and run:

    sbt compile
    sbt run

That will start the server, which will display:
Waiting for player1...

From other terminal type:

    nc localhost 5000

And from other terminal type:
    
    nc localhost 5000

To move enter row and column separated by space:

    > nc localhost 5000
    Connected as player 1
    +---+
    |...|
    |...|
    |...|
    +---+
    
    Your move: 
    2 2
    +---+
    |...|
    |.X.|
    |...|
    +---+
    
    Waiting other player...
    