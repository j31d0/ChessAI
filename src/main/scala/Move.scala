package mychessAI


sealed trait Move {
  def toLAN: String // LAN without a piece name
}

sealed trait SingleMove extends Move {
  val from: Position
  val to: Position
  def toLAN: String = s"${from.toLAN}${to.toLAN}"
}
sealed trait NonSingleMove extends Move

sealed trait CapturingMove extends SingleMove {
  val capturedPosition: Position
}
sealed trait NonCapturingSingleMove extends SingleMove

case class NormalCapturingMove(from: Position, to: Position)
    extends CapturingMove {
  val capturedPosition: Position = to
}
case class NormalNonCapturingMove(from: Position, to: Position)
    extends NonCapturingSingleMove

case class PromotionCapturingMove(
    from: Position,
    to: Position,
    promotion: Piece
) extends CapturingMove {
  val capturedPosition: Position = to
  override def toLAN: String =
    s"${from.toLAN}${to.toLAN}${promotion.toChar.toLower}"
}
case class PromotionNonCapturingMove(
    from: Position,
    to: Position,
    promotion: Piece
) extends NonCapturingSingleMove {
  override def toLAN: String =
    s"${from.toLAN}${to.toLAN}${promotion.toChar.toLower}"
}

case class EnPassant(from: Position, to: Position) extends CapturingMove {
  val capturedPosition: Position = Position(from.row, to.col)
}
case class CastlingMove(side: Side, isKingSide: Boolean) extends NonSingleMove {
  def toLAN: String = (side, isKingSide) match
    case (WhiteSide, true)  => "e1g1"
    case (WhiteSide, false) => "e1c1"
    case (BlackSide, true)  => "e8g8"
    case (BlackSide, false) => "e8c8"
}
