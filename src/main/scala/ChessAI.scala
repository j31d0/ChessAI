package mychessAI
import scala.collection.mutable
object ChessAI {

  def pieceValue(piece: Piece): Int = piece match {
    case _: Pawn   => 100
    case _: Knight => 300
    case _: Bishop => 300
    case _: Rook   => 500
    case _: Queen  => 900
    case _: King   => 10000
  }

  def safetyValue(piece: Piece): Int = piece match {
    case _: Pawn   => 0
    case _: Knight => 1
    case _: Bishop => 2
    case _: Rook   => 5
    case _: Queen  => 10
    case _: King   => 10
  }

  def activePieceValue(piece: Piece): Int = piece match {
    case _: Pawn   => 1
    case _: Knight => 6
    case _: Bishop => 6
    case _: Rook   => 4
    case _: Queen  => 3
    case _: King   => 1
  }

  def piecePosValue(
      piece: Piece,
      pos: Position,
      opponentAttackingVal: Int
  ): Int =
    val dangerness = if (piece.side == WhiteSide) pos.row else 7 - pos.row
    val middleness = math.abs(pos.col - 3.5) / 2
    -(dangerness * middleness * safetyValue(
      piece
    ) * opponentAttackingVal / 100.0).toInt

  def evaluate(board: BoardState): (Int, Int) = {
    if (board.isCheckmated)
      return if (board.turn == WhiteSide) (Int.MinValue + 1, 0)
      else (Int.MaxValue - 1, 0)
    else if (board.isGameOver && board.isDrawn) return (0, 0)
    var score = 0
    var threat = -200
    val whiteAttackingVal =
      board
        .pieceAndPos(WhiteSide)
        .map { case (piece, pos) =>
          piece
            .generateAttackingPositions(pos, board.bb)
            .filter(pos =>
              board.get(pos).map(_.side == BlackSide).getOrElse(true)
            )
            .length * activePieceValue(piece)
        }
        .sum
    val blackAttackingVal =
      board
        .pieceAndPos(BlackSide)
        .map { case (piece, pos) =>
          piece
            .generateAttackingPositions(pos, board.bb)
            .filter(pos =>
              board.get(pos).map(_.side == WhiteSide).getOrElse(true)
            )
            .length * activePieceValue(piece)
        }
        .sum
    score += whiteAttackingVal - blackAttackingVal
    board.pieceAndPos(WhiteSide).foreach { case (piece, pos) =>
      score += pieceValue(piece) + piecePosValue(
        piece,
        pos,
        blackAttackingVal
      )
    }
    board.pieceAndPos(BlackSide).foreach { case (piece, pos) =>
      score -= pieceValue(piece) + piecePosValue(
        piece,
        pos,
        whiteAttackingVal
      )
    }
    (score, threat)
  }

  // Entry of lower bound
  case class TableEntry(
      score: Int,
      threat: Int,
      depth: Int,
      mvs: List[Move]
  )
  class TranspositionTable {
    val table: mutable.Map[Long, TableEntry] =
      mutable.Map()

    def store(
        hash: Long,
        score: Int,
        threat: Int,
        depth: Int,
        mvs: List[Move]
    ): Unit = {
      table.update(hash, TableEntry(score, threat, depth, mvs))
    }

    def lookup(hash: Long): Option[TableEntry] = {
      table.get(hash)
    }

    def clear(): Unit = {
      table.clear()
    }
  }

  val transpositionTable: TranspositionTable =
    new TranspositionTable()

  def search(
      board: BoardState,
      depth: Int
  ): (Int, List[Move]) = {
    val iv = evaluate(board)
    val r = pvSearch(
      board,
      depth,
      -Int.MaxValue,
      Int.MaxValue,
      board.turn == WhiteSide,
      iv._1 * (if (board.turn == WhiteSide) 1 else -1),
      iv._2,
      false
    )
    (r._1, r._3)
  }

  private def pvSearch(
      board: BoardState,
      depth: Int,
      alpha: Int,
      beta: Int,
      isMaximizing: Boolean,
      ev: Int,
      th: Int,
      toRefute: Boolean
  ): (Int, Int, List[Move]) = {
    val hash = board.hashKey
    // println(board.visualize(false))
    if (depth == 0 || board.isGameOver) {
      return transpositionTable
        .lookup(hash)
        .map { case TableEntry(v, t, d, mvs) =>
          (v, t, mvs)
        }
        .getOrElse(ev, th, Nil)
    }

    /*
    val ndepth =
      if ((ev + th < alpha || ev - th >= beta))
        math.min(1, depth - 1)
      else depth - 1
     */
    val ndepth = depth - 1

    transpositionTable.lookup(hash) match
      case Some(v, t, d, mvs) if (d >= depth && v > beta) =>
        return (v, t, mvs)
      case _ => ()

    val moves = board.generateAllMoves
    val n = moves.length
    if (n == 0) {
      return transpositionTable
        .lookup(hash)
        .map { case TableEntry(v, t, d, mvs) =>
          if (v > beta) (v, t, mvs) else (ev, th, Nil)
        }
        .getOrElse(ev, th, Nil)
    }

    val orderedMoves = orderMoves(board, moves, isMaximizing)

    val (move, newBoard, nEv, nTh) = orderedMoves.head
    val (npvScore, pvThreat, pvMvs) = pvSearch(
      newBoard,
      depth - 1,
      -beta,
      -alpha,
      !isMaximizing,
      -nEv,
      nTh,
      toRefute
    )
    val pvScore = -npvScore
    val minorVariation = orderedMoves.tail
    var refuted = false
    var bestScore = pvScore
    var bestThreat = pvThreat
    var alphaCopy = if (pvScore > alpha) pvScore else alpha
    var betaCopy = beta
    var pvMove: List[Move] = move :: pvMvs

    var i = 0
    // null window search for minor variations to find refutation
    while (i < minorVariation.length && !refuted) {
      val (move, newBoard, nEv, nTh) = minorVariation(i)

      val (nscore, nth, nmvs) =
        pvSearch(
          newBoard,
          ndepth,
          -alphaCopy - 1,
          -alphaCopy,
          !isMaximizing,
          -nEv,
          nTh,
          true
        )
      val score = -nscore

      if (score > alphaCopy) refuted = true
      i += 1
    }

    if (refuted) {
      i = 0

      while (i < minorVariation.length) {
        val (move, newBoard, nEv, nTh) = minorVariation(i)

        // full search
        val (nscore, nth, nmvs) =
          pvSearch(
            newBoard,
            ndepth,
            -betaCopy,
            -alphaCopy,
            !isMaximizing,
            -nEv,
            nTh,
            toRefute
          )

        val score = -nscore

        if (score > bestScore) {
          bestScore = score
          bestThreat = nTh
          pvMove = move :: nmvs
          if (score >= alphaCopy) {
            alphaCopy = score
          }
          if (score > betaCopy) {
            if (
              transpositionTable.lookup(hash) match
                case Some(TableEntry(s, t, d, _))
                    if (d <= depth || (d == depth && s <= bestScore)) =>
                  true
                case None => true
                case _    => false
            )
              transpositionTable.store(
                hash,
                bestScore,
                bestThreat,
                depth,
                pvMove
              )
            return (betaCopy, bestThreat, pvMove)
          }
        }
        i += 1
      }
    }
    if (bestScore == -Int.MaxValue) {
      bestScore = ev
    }

    transpositionTable.lookup(hash) match
      case Some(TableEntry(e, t, d, _))
          if (d <= depth || (d == depth && e <= bestScore)) =>
        transpositionTable.store(
          hash,
          bestScore,
          bestThreat,
          depth,
          pvMove
        )
      case None =>
        transpositionTable.store(
          hash,
          bestScore,
          bestThreat,
          depth,
          pvMove
        )
      case _ => ()

    (bestScore, bestThreat, pvMove)
  }

  private def orderMoves(
      board: BoardState,
      moves: List[(Move, PseudoBoardState)],
      isMaximizing: Boolean
  ): List[(Move, BoardState, Int, Int)] = {
    moves
      .map((m) =>
        val newBoard = m._2.toBoardState
        val nEvTh =
          (transpositionTable.lookup(newBoard.hashKey) match
            case Some(TableEntry(score, threat, depth, _)) => (score, threat)
            case _ => {
              evaluate(newBoard) match {
                case (ev, t) => (ev * (if (isMaximizing) 1 else -1), t)
              }
            }
          )
        (
          m._1,
          newBoard,
          nEvTh._1,
          nEvTh._2
        )
      )
      .sortBy(_._3 * -1)
  }

  def findBestMove(board: BoardState, depth: Int): (Int, List[Move]) = {
    search(board, depth)
  }

}
