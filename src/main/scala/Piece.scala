package mychessAI

sealed trait Piece {
  def toChar: Char // Returns the character representation of the piece
  def toUnicode: String // Returns the unicode representation of the piece
  def side: Side // Returns the side (color) of the piece
  def generateAttackingPositions(pos: Position, bb: BitBoard): List[Position]
  // Generates positions that the piece can attack from the given position
  def generateNonAttackingPositions(
      pos: Position,
      bb: BitBoard
  ): List[Position]
  // Generates positions that the piece can move to without attacking from the given position
}

sealed trait SimplePiece extends Piece {
  val direction: List[
    (Int, Int)
  ] // The directions in which the piece can move
  val repeatable: Boolean // Indicates if the piece can make multiple moves in a direction

  /** Generates the positions that the piece can move to in a specific direction
    * from a given position.
    * @param pos
    *   The starting position of the piece.
    * @param bb
    *   The BitBoard representing the game state.
    * @param dir
    *   The direction in which to generate positions.
    * @return
    *   A list of positions that the piece can move to in the given direction.
    */
  def generatePositionsForDirection(
      pos: Position,
      bb: BitBoard,
      dir: (Int, Int)
  ): List[Position] =
    val newpos = Position(pos.row + dir._1, pos.col + dir._2)
    if (newpos.isValid)
      if (!repeatable || bb.get(newpos)) List(newpos)
      else generatePositionsForDirection(newpos, bb, dir) :+ newpos
    else List.empty

  /** Generates all possible positions that the piece can move to from a given
    * position.
    * @param pos
    *   The starting position of the piece.
    * @param bb
    *   The BitBoard representing the game state.
    * @return
    *   A list of positions that the piece can move to.
    */
  def generatePositions(pos: Position, bb: BitBoard): List[Position] =
    direction.map(generatePositionsForDirection(pos, bb, _)).flatten

  override def generateAttackingPositions(
      pos: Position,
      bb: BitBoard
  ): List[Position] = generatePositions(pos, bb)
  override def generateNonAttackingPositions(
      pos: Position,
      bb: BitBoard
  ): List[Position] = generatePositions(pos, bb)
}

sealed trait Pawn extends Piece {
  val attackdirection: List[(Int, Int)]
  val nonattackdirection: List[(Int, Int)]

  def generateAttackingPositions(
      pos: Position,
      bb: BitBoard
  ): List[Position] = {
    val attackingPositions = for {
      direction <- attackdirection
      newPos = Position(pos.row + direction._1, pos.col + direction._2)
      if newPos.isValid
    } yield newPos

    attackingPositions
  }

  def generateNonAttackingPositions(
      pos: Position,
      bb: BitBoard
  ): List[Position] = {
    val nonAttackingPositions = for {
      direction <- nonattackdirection
      newPos = Position(pos.row + direction._1, pos.col + direction._2)
      if newPos.isValid
    } yield newPos
    val firstMovePositions = for {
      direction <- nonattackdirection
      newPos = Position(pos.row + direction._1 * 2, pos.col + direction._2 * 2)
      if newPos.isValid && ((side == WhiteSide && pos.row == 1) || (side == BlackSide && pos.row == 6)) && !bb
        .get(Position(pos.row + direction._1, pos.col + direction._2))
    } yield newPos
    nonAttackingPositions ++ firstMovePositions
  }
}

sealed trait Knight extends SimplePiece {
  val direction: List[(Int, Int)] = List(
    (1, 2),
    (1, -2),
    (-1, 2),
    (-1, -2),
    (2, 1),
    (2, -1),
    (-2, 1),
    (-2, -1)
  )
  val repeatable: Boolean = false
}

sealed trait Bishop extends SimplePiece {
  val direction: List[(Int, Int)] = List(
    (1, 1),
    (1, -1),
    (-1, 1),
    (-1, -1)
  )
  val repeatable: Boolean = true
}

sealed trait Rook extends SimplePiece {
  val direction: List[(Int, Int)] = List(
    (1, 0),
    (0, 1),
    (-1, 0),
    (0, -1)
  )
  val repeatable: Boolean = true
}

sealed trait Queen extends SimplePiece {
  val direction: List[(Int, Int)] = List(
    (1, 0),
    (0, 1),
    (-1, 0),
    (0, -1), // Horizontal and vertical directions
    (1, 1),
    (1, -1),
    (-1, 1),
    (-1, -1) // Diagonal directions
  )
  val repeatable: Boolean = true
}

sealed trait King extends SimplePiece {
  val direction: List[(Int, Int)] = List(
    (1, 0),
    (0, 1),
    (-1, 0),
    (0, -1), // Horizontal and vertical directions
    (1, 1),
    (1, -1),
    (-1, 1),
    (-1, -1) // Diagonal directions
  )
  val repeatable: Boolean = false
}

case object WhitePawn extends Pawn {
  val attackdirection: List[(Int, Int)] = List((1, 1), (1, -1))
  val nonattackdirection: List[(Int, Int)] = List((1, 0))
  def toChar: Char = 'P'
  def toUnicode: String = "♙"
  def side: Side = WhiteSide
}

case object BlackPawn extends Pawn {
  val attackdirection: List[(Int, Int)] = List((-1, 1), (-1, -1))
  val nonattackdirection: List[(Int, Int)] = List((-1, 0))
  def toChar: Char = 'p'
  def toUnicode: String = "♟"
  def side: Side = BlackSide
}

case object WhiteKnight extends Knight {
  def toChar: Char = 'N'
  def toUnicode: String = "♘"
  def side: Side = WhiteSide
}
case object BlackKnight extends Knight {
  def toChar: Char = 'n'
  def toUnicode: String = "♞"
  def side: Side = BlackSide
}
case object WhiteBishop extends Bishop {
  def toChar: Char = 'B'
  def toUnicode: String = "♗"
  def side: Side = WhiteSide
}

case object BlackBishop extends Bishop {
  def toChar: Char = 'b'
  def toUnicode: String = "♝"
  def side: Side = BlackSide
}

case object WhiteRook extends Rook {
  def toChar: Char = 'R'
  def toUnicode: String = "♖"
  def side: Side = WhiteSide
}

case object BlackRook extends Rook {
  def toChar: Char = 'r'
  def toUnicode: String = "♜"
  def side: Side = BlackSide
}

case object WhiteQueen extends Queen {
  def toChar: Char = 'Q'
  def toUnicode: String = "♕"
  def side: Side = WhiteSide
}

case object BlackQueen extends Queen {
  def toChar: Char = 'q'
  def toUnicode: String = "♛"
  def side: Side = BlackSide
}

case object WhiteKing extends King {
  def toChar: Char = 'K'
  def toUnicode: String = "♔"
  def side: Side = WhiteSide
}

case object BlackKing extends King {
  def toChar: Char = 'k'
  def toUnicode: String = "♚"
  def side: Side = BlackSide
}
