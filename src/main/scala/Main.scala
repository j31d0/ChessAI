package mychessAI

import scala.io.StdIn

object Main:
    def main(args: Array[String]): Unit = {

    // UCI protocol initialization
    println("id name MyChessAI")
    println("id author JiheePark")
    println("uci")

    var position: BoardState = BoardState.initial

    while (true) {
      val input = StdIn.readLine()
      val command = input.split(" ").head

      command match {
        case "uci" =>
          println("uciok")

        case "isready" =>
          println("readyok")

        case "position" =>
          position = BoardState.parsePosition(input)
          // Update the internal game position

        case "go" =>
          val depth = 3
          val (cp, bestMove) = ChessAI.findBestMove(position, depth)
          println(s"info depth $depth score cp $cp pv ${bestMove.map(_.toLAN).mkString(" ")}")

          println(s"bestmove ${bestMove.head.toLAN}")

        case "quit" =>
          System.exit(0)

        case _ =>
          // Handle unknown command or ignore
      }
    }
  }

  /* def main(args: Array[String]): Unit =
    println("Hello world!")
    println(msg)
    var i = BoardState.initial

    while (i.generateAllMoves.nonEmpty) {
      println(i.visualize(true))
      println(i.toFEN)
      if (i.turn == BlackSide)
        val k = ChessAI.findBestMove(i, 3)
        println(BoardState.moveToSANS(i, k))
        i = i.applyMove(k.head)
      else
        val (mvs, mvsSAN) = i.generateAllMovesSAN.unzip
        println(mvsSAN)
        val idx = mvsSAN.indexOf(StdIn.readLine.strip)
        if (idx != -1) i = i.applyMove(mvs(idx))
    }

  def msg = "I was compiled by Scala 3. :)"
  */
end Main
