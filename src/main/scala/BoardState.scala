package mychessAI

import scala.collection.mutable.ListBuffer

case class EtcData(
    whiteKingMoved: Boolean = false,
    blackKingMoved: Boolean = false,
    whiteRookAMoved: Boolean = false,
    whiteRookHMoved: Boolean = false,
    blackRookAMoved: Boolean = false,
    blackRookHMoved: Boolean = false,
    enPassant: Option[Position] = None
)

class PseudoBoardState(
    val board: Array[Array[Option[Piece]]],
    val turn: Side,
    val etc: EtcData
) {
  val bb = BitBoard(data = board.map(_.map(_.isDefined)))
  def get(pos: Position): Option[Piece] = board(pos.row)(pos.col)

  val pieceAndPos: Map[Side, IndexedSeq[(Piece, Position)]] =
    val whiteBuilder = ListBuffer.empty[(Piece, Position)]
    val blackBuilder = ListBuffer.empty[(Piece, Position)]
    for {
      row <- board.indices
      col <- board(row).indices
      pieceOption = board(row)(col)
      if pieceOption.isDefined
    } {
      val piece = pieceOption.get
      if (piece.side == WhiteSide) whiteBuilder += ((piece, Position(row, col)))
      else blackBuilder += ((piece, Position(row, col)))
    }
    Map(
      WhiteSide -> whiteBuilder.toIndexedSeq,
      BlackSide -> blackBuilder.toIndexedSeq
    )

  val getAttackingBitBoard: Map[Side, BitBoard] =
    List(WhiteSide, BlackSide).map { case side =>
      val arr = Array.fill(8, 8)(false)
      pieceAndPos(side).foreach((piece, pos) => {
        piece
          .generateAttackingPositions(pos, bb)
          .foreach((p) => arr(p.row)(p.col) = true)
      })
      (side, BitBoard(arr))
    }.toMap

  val isChecked: Map[Side, Boolean] = List(WhiteSide, BlackSide).map {
    case side =>
      val kingPosition = pieceAndPos(side)
        .find(_._1 == (if (side == WhiteSide) WhiteKing else BlackKing))
        .get
        ._2
      val opponentTurn = if (side == WhiteSide) BlackSide else WhiteSide
      val opponentMoves = getAttackingBitBoard(opponentTurn)
      (side, opponentMoves.data(kingPosition.row)(kingPosition.col))
  }.toMap

  def toBoardState: BoardState = BoardState(board, turn, etc)
}

case class BoardState(
    override val board: Array[Array[Option[Piece]]],
    override val turn: Side,
    override val etc: EtcData
) extends PseudoBoardState(board, turn, etc) {

  def visualize(useUnicode: Boolean): String = {
    val sb = StringBuilder.newBuilder
    sb.append("  +-----------------+\n")
    for (row <- board.indices.reverse) {
      sb.append(s"${row + 1} | ")
      for (col <- board(row).indices) {
        // sb.append(board(row)(col).map(_.toChar).getOrElse(' '))
        if (useUnicode) {
          sb.append(board(row)(col).map(_.toUnicode).getOrElse(" "))
        } else {
          sb.append(board(row)(col).map(_.toChar).getOrElse(' '))
        }
        sb.append(' ')
      }
      sb.append("|\n")
    }
    sb.append("  +-----------------+\n")
    sb.append("    a b c d e f g h\n")
    sb.toString
  }
  def toFEN: String = {
    val fenBoard = board.map(_.map(_.map(_.toChar).getOrElse(' ')))
    def reduceSingleLine(l: Array[Char]): String = {
      val sb = StringBuilder.newBuilder
      var count = 0
      for (c <- l) {
        if (c == ' ') count += 1
        else {
          if (count > 0) sb.append(count)
          sb.append(c)
          count = 0
        }
      }
      if (count > 0) sb.append(count)
      sb.toString
    }
    val fenBoardString = fenBoard
      .map(reduceSingleLine)
      .reverse
      .mkString("/")
    val fenTurn = if (turn == WhiteSide) "w" else "b"
    val fenCastling = {
      val whiteKingMoved = etc.whiteKingMoved
      val blackKingMoved = etc.blackKingMoved
      val whiteRookAMoved = etc.whiteRookAMoved
      val whiteRookHMoved = etc.whiteRookHMoved
      val blackRookAMoved = etc.blackRookAMoved
      val blackRookHMoved = etc.blackRookHMoved
      val whiteKingSideCastling = !whiteKingMoved && !whiteRookHMoved
      val whiteQueenSideCastling = !whiteKingMoved && !whiteRookAMoved
      val blackKingSideCastling = !blackKingMoved && !blackRookHMoved
      val blackQueenSideCastling = !blackKingMoved && !blackRookAMoved
      val whiteCastling =
        if (whiteKingSideCastling && whiteQueenSideCastling) "KQ"
        else if (whiteKingSideCastling) "K"
        else if (whiteQueenSideCastling) "Q"
        else "-"
      val blackCastling =
        if (blackKingSideCastling && blackQueenSideCastling) "kq"
        else if (blackKingSideCastling) "k"
        else if (blackQueenSideCastling) "q"
        else "-"
      s"$whiteCastling$blackCastling"
    }
    val fenEnPassant = etc.enPassant.map(_.toLAN).getOrElse("-")
    val fenHalfMoveClock = 0
    val fenFullMoveNumber = 1
    s"$fenBoardString $fenTurn $fenCastling $fenEnPassant $fenHalfMoveClock $fenFullMoveNumber"
  }

  val generatePseudoMoves: List[Move] = {
    val moves = ListBuffer.empty[Move]
    val mypieceAndPos: IndexedSeq[(Piece, Position)] = pieceAndPos(turn)
    val opponentPieceAndPos: IndexedSeq[(Piece, Position)] = pieceAndPos(
      if (turn == WhiteSide) BlackSide else WhiteSide
    )
    val attackedBitBoard = {
      val arr = Array.fill(8, 8)(false)
      opponentPieceAndPos.foreach((piece, pos) => {
        piece
          .generateAttackingPositions(pos, bb)
          .foreach((p) => arr(p.row)(p.col) = true)
      })
      BitBoard(arr)
    }
    val noncapturingMoves = mypieceAndPos
      .map((piece, pos) =>
        piece
          .generateNonAttackingPositions(pos, bb)
          .filter((p) => get(p).isEmpty)
          .flatMap(to =>
            piece match
              case WhitePawn => {
                if (to.row == 7)
                  List(
                    PromotionNonCapturingMove(pos, to, WhiteQueen),
                    PromotionNonCapturingMove(pos, to, WhiteRook),
                    PromotionNonCapturingMove(pos, to, WhiteBishop),
                    PromotionNonCapturingMove(pos, to, WhiteKnight)
                  )
                else
                  List(
                    NormalNonCapturingMove(pos, to)
                  )
              }
              case BlackPawn => {
                if (to.row == 0)
                  List(
                    PromotionNonCapturingMove(pos, to, BlackQueen),
                    PromotionNonCapturingMove(pos, to, BlackRook),
                    PromotionNonCapturingMove(pos, to, BlackBishop),
                    PromotionNonCapturingMove(pos, to, BlackKnight)
                  )
                else
                  List(
                    NormalNonCapturingMove(pos, to)
                  )
              }
              case _ => List(NormalNonCapturingMove(pos, to))
          )
      )
      .flatten
    val capturingMoves = mypieceAndPos
      .map((piece, pos) =>
        piece
          .generateAttackingPositions(pos, bb)
          .filter((p) => get(p).isDefined && get(p).get.side != turn)
          .flatMap(to =>
            piece match
              case WhitePawn => {
                val nonEnpassan =
                  if (to.row == 7)
                    List(
                      PromotionCapturingMove(pos, to, WhiteQueen),
                      PromotionCapturingMove(pos, to, WhiteRook),
                      PromotionCapturingMove(pos, to, WhiteBishop),
                      PromotionCapturingMove(pos, to, WhiteKnight)
                    )
                  else
                    List(
                      NormalCapturingMove(pos, to)
                    )
                val enpassan =
                  etc.enPassant match
                    case Some(enpassanPos) if enpassanPos == to =>
                      List(EnPassant(pos, to))
                    case _ => List.empty
                nonEnpassan ++ enpassan
              }
              case BlackPawn => {
                val nonEnpassan =
                  if (to.row == 0)
                    List(
                      PromotionCapturingMove(pos, to, BlackQueen),
                      PromotionCapturingMove(pos, to, BlackRook),
                      PromotionCapturingMove(pos, to, BlackBishop),
                      PromotionCapturingMove(pos, to, BlackKnight)
                    )
                  else
                    List(
                      NormalCapturingMove(pos, to)
                    )
                val enpassan =
                  etc.enPassant match
                    case Some(enpassanPos) if enpassanPos == to =>
                      List(EnPassant(pos, to))
                    case _ => List.empty
                nonEnpassan ++ enpassan

              }
              case _ => List(NormalCapturingMove(pos, to))
          )
      )
      .flatten
    val castlingMoves = turn match
      case WhiteSide => {
        val kingMoved = etc.whiteKingMoved
        val rookAMoved = etc.whiteRookAMoved
        val rookHMoved = etc.whiteRookHMoved
        val kingSideEmpty =
          (1 to 2).forall((i) => board(0)(4 + i).isEmpty)
        val kingSideNotAttacked =
          (0 to 2).forall((i) => !attackedBitBoard.get(Position(0, 4 + i)))
        val queenSideEmpty =
          (1 to 3).forall((i) => board(0)(4 - i).isEmpty)
        val queenSideNotAttacked =
          (0 to 3).forall((i) => !attackedBitBoard.get(Position(0, 4 - i)))
        val kingSideCastling =
          !kingMoved && !rookHMoved && kingSideEmpty && kingSideNotAttacked
        val queenSideCastling =
          !kingMoved && !rookAMoved && queenSideEmpty && queenSideNotAttacked
        val m = ListBuffer[CastlingMove]()
        if (kingSideCastling) m += CastlingMove(WhiteSide, true)
        if (queenSideCastling) m += CastlingMove(WhiteSide, false)
        m.toList
      }
      case BlackSide => {
        val kingMoved = etc.blackKingMoved
        val rookAMoved = etc.blackRookAMoved
        val rookHMoved = etc.blackRookHMoved
        val kingSideEmpty =
          (1 to 2).forall((i) => board(7)(4 + i).isEmpty)
        val kingSideNotAttacked =
          (0 to 2).forall((i) => !attackedBitBoard.get(Position(7, 4 + i)))
        val queenSideEmpty =
          (1 to 3).forall((i) => board(7)(4 - i).isEmpty)
        val queenSideNotAttacked =
          (0 to 3).forall((i) => !attackedBitBoard.get(Position(7, 4 - i)))
        val kingSideCastling =
          !kingMoved && !rookHMoved && kingSideEmpty && kingSideNotAttacked
        val queenSideCastling =
          !kingMoved && !rookAMoved && queenSideEmpty && queenSideNotAttacked
        val m = ListBuffer[CastlingMove]()
        if (kingSideCastling) m += CastlingMove(BlackSide, true)
        if (queenSideCastling) m += CastlingMove(BlackSide, false)
        m.toList
      }
    (noncapturingMoves.toList ++ capturingMoves.toList ++ castlingMoves).distinct
  }

  val generateAllMoves: List[Move] =
    generatePseudoMoves.filter(!applyPseudoMove(_).isChecked(turn))

  val isCheckmated: Boolean =
    isChecked(turn) && generateAllMoves.isEmpty

  val isStalemated: Boolean =
    !isChecked(turn) && generateAllMoves.isEmpty

  // calculate insufficient material (King vs King + Knight or Bishop)
  val insufficient: Boolean =
    val w1 = pieceAndPos(WhiteSide)
    val b1 = pieceAndPos(BlackSide)
    w1.length == 1 && b1.length == 1 ||
    w1.length == 1 && b1.length == 2 && b1.exists(_._1.isInstanceOf[Knight]) ||
    b1.length == 1 && w1.length == 2 && w1.exists(_._1.isInstanceOf[Knight]) ||
    w1.length == 1 && b1.length == 2 && b1.exists(_._1.isInstanceOf[Bishop]) ||
    b1.length == 1 && w1.length == 2 && w1.exists(_._1.isInstanceOf[Bishop])

  val isDrawn: Boolean = isStalemated || insufficient
  val isGameOver: Boolean = isCheckmated || isDrawn
  val getWinner: Option[Side] =
    if (isCheckmated) Some(if (turn == WhiteSide) BlackSide else WhiteSide)
    else None

  def applyPseudoMove(move: Move): PseudoBoardState = {
    val boardCopy =
      board.map(_.clone()) // Create a deep copy of the board

    val newEtc = move match
      case m: SingleMove => {
        val originalPiece = boardCopy(m.from.row)(m.from.col).getOrElse(
          throw new IllegalArgumentException(
            "No piece found at the source square"
          )
        )
        val newPiece = m match
          case PromotionCapturingMove(from, to, promotion)    => promotion
          case PromotionNonCapturingMove(from, to, promotion) => promotion
          case _                                              => originalPiece

        move match
          case m: CapturingMove =>
            boardCopy(m.capturedPosition.row)(m.capturedPosition.col) = None
          case _ => ()
        // Move the piece to the destination square
        boardCopy(m.to.row)(m.to.col) = Some(newPiece)
        boardCopy(m.from.row)(m.from.col) = None
        etc.copy(
          whiteKingMoved = etc.whiteKingMoved || originalPiece == WhiteKing,
          blackKingMoved = etc.blackKingMoved || originalPiece == BlackKing,
          whiteRookAMoved =
            etc.whiteRookAMoved || originalPiece == WhiteRook && m.from == Position(
              0,
              0
            ),
          whiteRookHMoved =
            etc.whiteRookHMoved || originalPiece == WhiteRook && m.from == Position(
              0,
              7
            ),
          blackRookAMoved =
            etc.blackRookAMoved || originalPiece == BlackRook && m.from == Position(
              7,
              0
            ),
          blackRookHMoved =
            etc.blackRookHMoved || originalPiece == BlackRook && m.from == Position(
              7,
              7
            ),
          enPassant =
            if (originalPiece == WhitePawn && m.from.row == 1 && m.to.row == 3)
              Some(Position(2, m.from.col))
            else if (
              originalPiece == BlackPawn && m.from.row == 6 && m.to.row == 4
            ) Some(Position(5, m.from.col))
            else None
        )
      }
      case CastlingMove(side, isKingSide) => {
        val row = if (side == WhiteSide) 0 else 7
        val kingCol = 4
        val rookCol = if (isKingSide) 7 else 0
        val king = boardCopy(row)(kingCol).getOrElse(
          throw new IllegalArgumentException("No king found")
        )
        val rook = boardCopy(row)(rookCol).getOrElse(
          throw new IllegalArgumentException("No rook found")
        )
        boardCopy(row)(kingCol) = None
        boardCopy(row)(rookCol) = None
        boardCopy(row)(if (isKingSide) 6 else 2) = Some(king)
        boardCopy(row)(if (isKingSide) 5 else 3) = Some(rook)
        etc.copy(
          whiteKingMoved = etc.whiteKingMoved || side == WhiteSide,
          blackKingMoved = etc.blackKingMoved || side == BlackSide,
          whiteRookAMoved =
            etc.whiteRookAMoved || (side == WhiteSide && !isKingSide),
          whiteRookHMoved =
            etc.whiteRookHMoved || (side == WhiteSide && isKingSide),
          blackRookAMoved =
            etc.blackRookAMoved || (side == BlackSide && !isKingSide),
          blackRookHMoved =
            etc.blackRookHMoved || (side == BlackSide && isKingSide),
          enPassant = None
        )
      }

    val nextTurn =
      if (turn == WhiteSide) BlackSide else WhiteSide // Switch the turn

    PseudoBoardState(boardCopy, nextTurn, newEtc)
  }

  def applyMove(m: Move): BoardState = applyPseudoMove(m).toBoardState

  def generateAllMovesSAN: List[(Move, String)] = {
    val pmoves = generatePseudoMoves.map((m) =>
      val nstate = applyMove(m)
      (
        m,
        nstate.isChecked(turn),
        nstate.isChecked(nstate.turn),
        nstate.isCheckmated
      )
    )
    val moves = pmoves.filter((p) => !p._2)
    val castles = moves.collect { case (m: CastlingMove, i, j, k) => (m, j, k) }
    val singles = moves.collect { case (s: SingleMove, i, j, k) => (s, j, k) }
    val singledatas = singles.map { case (s, i, j) =>
      val piece = get(s.from).get
      val pieceChar =
        if (piece.isInstanceOf[Pawn]) "" else piece.toChar.toUpper.toString
      val isCapture = s.isInstanceOf[CapturingMove]
      val from = s.from
      val to = s.to
      val strPromotion = s match
        case PromotionCapturingMove(_, _, promotion) =>
          s"=${promotion.toChar.toUpper}"
        case PromotionNonCapturingMove(_, _, promotion) =>
          s"=${promotion.toChar.toUpper}"
        case _ => ""
      (
        s,
        pieceChar,
        isCapture,
        from,
        to,
        strPromotion + (if (j) "#" else if (i) "+" else "")
      )
    }
    (singledatas.groupBy((x) => (x._2, x._5)).toList.flatMap { case (_, l) =>
      l.map { case (s, pieceChar, isCapture, from, to, etc) =>
        val (s1, ngroup) =
          if (l.map(_._4.col).distinct.length == 1) ("", l)
          else
            (('a' + from.col).toChar.toString, l.filter(_._4.col == from.col))
        val s2 =
          if (ngroup.map(_._4.row).distinct.length == 1) ""
          else ('1' + from.row).toChar.toString

        val firstPart = pieceChar + s1 + s2
        val secondPart = (if (isCapture) "x" else "") + to.toLAN + etc
        (
          s,
          (if (firstPart.length == 0 && isCapture)
             ('a' + from.col).toChar.toString
           else firstPart) + secondPart
        )
      }
    } ++
      castles.map { case (m, i, j) =>
        (
          m,
          (if (m.isKingSide) "O-O" else "O-O-O") + (if (j) "#"
                                                    else if (i) "+"
                                                    else "")
        )
      }).sortBy(_._2)
  }

  def moveToSAN(m: Move): String = {
    val moves = generateAllMovesSAN
    val idx = moves.map(_._1).indexOf(m)
    if (idx == -1) throw new IllegalArgumentException("Invalid move")
    moves(idx)._2
  }
}

object BoardState {
  def initial: BoardState = {
    val board = Array.fill[Option[Piece]](8, 8)(None)
    // Place the initial pieces on the board

    // Place pawns
    for (col <- 0 until 8) {
      board(1)(col) = Some(WhitePawn)
      board(6)(col) = Some(BlackPawn)
    }

    // Place rooks
    board(0)(0) = Some(WhiteRook)
    board(0)(7) = Some(WhiteRook)
    board(7)(0) = Some(BlackRook)
    board(7)(7) = Some(BlackRook)

    // Place knights
    board(0)(1) = Some(WhiteKnight)
    board(0)(6) = Some(WhiteKnight)
    board(7)(1) = Some(BlackKnight)
    board(7)(6) = Some(BlackKnight)

    // Place bishops
    board(0)(2) = Some(WhiteBishop)
    board(0)(5) = Some(WhiteBishop)
    board(7)(2) = Some(BlackBishop)
    board(7)(5) = Some(BlackBishop)

    // Place queens
    board(0)(3) = Some(WhiteQueen)
    board(7)(3) = Some(BlackQueen)

    // Place kings
    board(0)(4) = Some(WhiteKing)
    board(7)(4) = Some(BlackKing)

    BoardState(board, WhiteSide, EtcData())
  }

  def moveToSANS(bs: BoardState, m: List[Move]): List[String] = {
    var i = bs
    m.map((m) => {
      val san = i.moveToSAN(m)
      i = i.applyMove(m)
      san
    })
  }

  /* input example:
    position fen rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2
    position startpos moves e2e4
   */
  def parsePosition(input: String): BoardState =
    val parts = input.split(" ")
    val board = Array.fill[Option[Piece]](8, 8)(None)
    var i = 1
    val b1 = if (parts(i) == "fen")
      val fen = parts(i + 1)
      val turn = if (parts(i + 2) == "w") WhiteSide else BlackSide
      val castling = parts(i + 3)
      val enpassant = parts(i + 4)
      val rows = fen.split("/")
      for (row <- 0 until 8) {
        var col = 0
        for (c <- rows(7 - row)) {
          if (c.isDigit) col += c.asDigit
          else {
            val piece = c match
              case 'p' => BlackPawn
              case 'n' => BlackKnight
              case 'b' => BlackBishop
              case 'r' => BlackRook
              case 'q' => BlackQueen
              case 'k' => BlackKing
              case 'P' => WhitePawn
              case 'N' => WhiteKnight
              case 'B' => WhiteBishop
              case 'R' => WhiteRook
              case 'Q' => WhiteQueen
              case 'K' => WhiteKing
            board(row)(col) = Some(piece)
            col += 1
          }
        }
      }
      val whiteKingMoved =
        castling.indexOf('K') == -1 || castling.indexOf('Q') == -1
      val whiteRookAMoved = castling.indexOf('Q') == -1
      val whiteRookHMoved = castling.indexOf('K') == -1
      val blackKingMoved =
        castling.indexOf('k') == -1 || castling.indexOf('q') == -1
      val blackRookAMoved = castling.indexOf('q') == -1
      val blackRookHMoved = castling.indexOf('k') == -1
      val enPassantPosition =
        if (enpassant == "-") None
        else Some(Position(enpassant(0) - 'a', enpassant(1) - '1'))
      i += 7
      BoardState(
        board,
        turn,
        EtcData(
          whiteKingMoved,
          blackKingMoved,
          whiteRookAMoved,
          whiteRookHMoved,
          blackRookAMoved,
          blackRookHMoved,
          enPassantPosition
        )
      )
    else
      i += 1
      BoardState.initial
    if (parts.length <= i) b1
    else
      assert(parts(i) == "moves")
      parts
        .drop(i + 1)
        .foldLeft(b1)((b, s) =>
          b.generateAllMoves.map((m) => (m, m.toLAN)).find(_._2 == s) match
            case Some((m, _)) => b.applyMove(m)
            case None => throw new IllegalArgumentException("Invalid move")
        )
}
