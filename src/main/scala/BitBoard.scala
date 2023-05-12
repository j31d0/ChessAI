package mychessAI

case class BitBoard(data: Array[Array[Boolean]]) {
  // Retrieves the value of the specified position on the board
  def get(pos: Position): Boolean = if (pos.isValid) data(pos.row)(pos.col)
  else throw new IllegalArgumentException("Invalid position")
}