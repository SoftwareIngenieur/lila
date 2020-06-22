package lila.game

import chess.{Bishop, Color, King, Knight, Pawn, Piece, Pos, Queen, Rook, UniquePiece}
import chess.variant.crazy._
object KagHelper {

  def valForLastThreeMoves(numMovesAgo: Int, numMovesEachSide: Int, index: Int,lastThreeEachSide: IndexedSeq[Option[Pos]]): Option[Pos] = {
    if(numMovesAgo < numMovesEachSide  ){
     lastThreeEachSide(index)
    }else{
      None
    }
  }


  def fromlistOfTurnsAndUniquPiecesMovedStr(str: String): LastThreeMoves = {
    val lastThreeEachSide = str.toSeq.map{
      case "~" => None
      case otherChar => Pos.piotr(otherChar)
    }
    LastThreeMoves(
      lastThreeEachSide(0),  lastThreeEachSide(1),  lastThreeEachSide(2),
      lastThreeEachSide(3),  lastThreeEachSide(4),  lastThreeEachSide(5)),


  }

  import chess.variant.crazy._

  def tolistOfTurnsAndUniquPiecesMovedStr(lastThree: LastThreeMoves):String =
    lastThree match {
      case LastThreeMoves(b1, b2, b3, w1, w2, w3) =>

          b1.map(_.piotr.toString).getOrElse("~") +
          b2.map(_.piotr.toString).getOrElse("~") +
          b3.map(_.piotr.toString).getOrElse("~")+
            w1.map(_.piotr.toString).getOrElse("~") +
            w2.map(_.piotr.toString).getOrElse("~") +
            w3.map(_.piotr.toString).getOrElse("~")
    }




  def fromPieceStr(str: String): UniquePiece = {
 val splitted = str.split(",")
    if(splitted.length == 2) {
      val Seq(id: String, sym: String) = Seq(splitted(0), splitted(1))
      val piece = KagHelper.pieceFromUnicode(sym)
      val uniquePiece = UniquePiece(Integer.parseInt(id), piece)
      uniquePiece
    }
    else {
      println(s"ERROR fromPieceStr $str")
      UniquePiece(10001,pieceFromUnicode("♘"))
    }
  }

  def toUPieceStr(piece: UniquePiece): String = {
    piece.id.toString + "," + piece.genericPiece.unicode
  }

  def pieceFromUnicode(unicode: String): Piece = {
unicode match {
          case "♔" => Piece(Color.White, King) 
          case "♕" => Piece(Color.White, Queen) 
          case "♖" => Piece(Color.White, Rook) 
          case "♗" => Piece(Color.White, Bishop) 
          case "♘" => Piece(Color.White, Knight) 
          case "♙" => Piece(Color.White, Pawn) 

          case "♚" => Piece(Color.Black, King)
          case "♛" => Piece(Color.Black, Queen)
          case "♜" => Piece(Color.Black, Rook)
          case "♝" => Piece(Color.Black, Bishop)
          case "♞" => Piece(Color.Black, Knight)
          case "♟︎" =>Piece(Color.Black, Pawn)

          case huh => {
            println(s"ERROR in pieceFromUnicode. argument was $huh")
            Piece(Color.black, Pawn)
          }
}
      }
  def toPieceMappingStr(tuple: (UniquePiece, Pos)):String = {
    val (piece, position) = tuple
    piece.id.toString + "," + piece.genericPiece.unicode + "," + position.piotr
  }
  def toPieceMappings(str: String): (UniquePiece, Pos) = {
    val Seq(id:String,sym:String,pos:String) = str.split(",").toSeq
    val piece = KagHelper.pieceFromUnicode(sym)
    val uniquePiece = UniquePiece(Integer.parseInt(id).toInt , piece)
    val piotr = pos.toCharArray.toSeq(0)
    val posFromPiotr = Pos.piotr(piotr).getOrElse(Pos.B8)
    (uniquePiece,posFromPiotr )
  }

}
