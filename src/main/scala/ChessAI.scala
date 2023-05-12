package mychessAI

object ChessAI {

  def pieceValue(piece: Piece): Int = piece match {
    case _: Pawn   => 1
    case _: Knight => 3
    case _: Bishop => 3
    case _: Rook   => 5
    case _: Queen  => 9
    case _: King   => 100
  }

  def evaluate(board: BoardState): Int = {
    if (board.isCheckmated) return if (board.turn == WhiteSide) -1000 else 1000
    else if (board.isGameOver && board.isDrawn) return 0
    var score = 0
    board.pieceAndPos(WhiteSide).foreach { case (piece, pos) =>
      score += pieceValue(piece)
    }
    board.pieceAndPos(BlackSide).foreach { case (piece, pos) =>
      score -= pieceValue(piece)
    }
    score
  }

  def minimax(
      board: BoardState,
      depth: Int,
      alpha: Int,
      beta: Int,
      maximizingPlayer: Boolean
  ): (Int, List[Move]) = {
    if (depth == 0 || board.isGameOver) (evaluate(board), Nil)
    else if (maximizingPlayer) {
      var maxEval = -1001
      var newAlpha = alpha
      var bestMove: List[Move] = Nil
      var result: (Int, List[Move]) =
        (maxEval, bestMove)
      val moves =
        board.generateAllMoves
          .sortBy((m) => evaluate(board.applyMove(m)))
          .reverse
      var i = 0
      while (i < moves.size && maxEval < beta) {
        val move = moves(i)
        val newBoard = board.applyMove(move)
        val (eval, nmoves) =
          minimax(newBoard, depth - 1, newAlpha, beta, false)
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
      var minEval = 1001
      var newBeta = beta
      var bestMove: List[Move] = Nil
      var result: (Int, List[Move]) =
        (minEval, bestMove)
      val moves =
        board.generateAllMoves.sortBy((m) => evaluate(board.applyMove(m)))
      var i = 0
      while (i < moves.size && minEval > alpha) {
        val move = moves(i)
        val newBoard = board.applyMove(move)
        val (eval, nmoves) =
          minimax(newBoard, depth - 1, alpha, newBeta, true)
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
      minimax(board, depth, -1001, 1001, board.turn == WhiteSide)
    (v * 100, bestMove)
  }

}
