package mychessAI

object ChessAI {

  def pieceValue(piece: Piece): Int = piece match {
    case _: Pawn   => 100
    case _: Knight => 300
    case _: Bishop => 300
    case _: Rook   => 500
    case _: Queen  => 900
    case _: King   => 10000
  }

  def evaluate(board: BoardState): Int = {
    if (board.isCheckmated)
      return if (board.turn == WhiteSide) Int.MinValue + 1 else Int.MaxValue - 1
    else if (board.isGameOver && board.isDrawn) return 0
    var score = 0
    board.pieceAndPos(WhiteSide).foreach { case (piece, pos) =>
      score += pieceValue(piece)
    }
    board.pieceAndPos(BlackSide).foreach { case (piece, pos) =>
      score -= pieceValue(piece)
    }
    score += board
      .getAttackingBitBoard(WhiteSide)
      .data
      .map(_.count(identity))
      .sum
    score -= board
      .getAttackingBitBoard(BlackSide)
      .data
      .map(_.count(identity))
      .sum

    score
  }

  def minimax(
      board: BoardState,
      depth: Int,
      alpha: Int,
      beta: Int,
      maximizingPlayer: Boolean,
      ev: Int
  ): (Int, List[Move]) = {
    if (depth == 0 || board.isGameOver) (ev, Nil)
    else if (maximizingPlayer) {
      var maxEval = Int.MinValue
      var newAlpha = alpha
      var bestMove: List[Move] = Nil
      var result: (Int, List[Move]) =
        (maxEval, bestMove)
      val moves =
        board.generateAllMoves
          .map((m) => {
            val newBoard = board.applyMove(m)
            (m, newBoard, evaluate(newBoard))
          })
          .sortBy(_._3)
          .reverse
      var i = 0
      while (i < moves.size && maxEval < beta) {
        val (move, newBoard, newEv) = moves(i)
        val (eval, nmoves) =
          minimax(newBoard, depth - 1, newAlpha, beta, false, newEv)
        if (eval > maxEval) {
          maxEval = eval
          bestMove = move +: nmoves
        }
        newAlpha = math.max(newAlpha, eval)
        result = (maxEval, bestMove)
        i += 1
      }
      result
    } else {
      var minEval = Int.MaxValue
      var newBeta = beta
      var bestMove: List[Move] = Nil
      var result: (Int, List[Move]) =
        (minEval, bestMove)
      val moves =
        board.generateAllMoves
          .map((m) => {
            val newBoard = board.applyMove(m)
            (m, newBoard, evaluate(newBoard))
          })
          .sortBy(_._3)
      var i = 0
      while (i < moves.size && minEval > alpha) {
        val (move, newBoard, newEv) = moves(i)
        val (eval, nmoves) =
          minimax(newBoard, depth - 1, alpha, newBeta, true, newEv)
        if (eval < minEval) {
          minEval = eval
          bestMove = move +: nmoves
        }
        newBeta = math.min(newBeta, eval)
        result = (minEval, bestMove)
        i += 1
      }
      result
    }
  }

  def findBestMove(board: BoardState, depth: Int): (Int, List[Move]) = {
    val (v, bestMove) =
      minimax(
        board,
        depth,
        Int.MinValue,
        Int.MaxValue,
        board.turn == WhiteSide,
        evaluate(board)
      )
    (if (board.turn == WhiteSide) v else -v, bestMove)
  }

}
