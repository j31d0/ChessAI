package mychessAI

case class Position(row: Int, col: Int) {

  /** Checks if the position is within the bounds of the chessboard (0-7 for
    * both row and column).
    * @return
    *   True if the position is valid, false otherwise.
    */
  def isValid: Boolean = 0 <= row && row <= 7 && 0 <= col && col <= 7
  // Returns the position in LAN format
  def toLAN: String =
    val rowChar = ('1' + row).toChar
    val colChar = ('a' + col).toChar
    s"$colChar$rowChar"
}
