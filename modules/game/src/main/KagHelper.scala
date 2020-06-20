package lila.game

import chess.{Bishop, Color, King, Knight, Pawn, Piece, Pos, Queen, Rook, UniquePiece}

object KagHelper {
  def fromlistOfTurnsAndUniquPiecesMovedStr(str: String):  (Int, Option[UniquePiece]) = {
    val seq =  str.split(",")
    if(seq.length == 3){
    val thruple = (seq(0), seq(1), seq(2))
     thruple match {
       case (turnNo: String, id: String, sym: String) => {
         val piece = KagHelper.pieceFromUnicode(sym)
         val uniquePiece = UniquePiece(Integer.parseInt(id), piece)
         (Integer.parseInt(turnNo), Some(uniquePiece))
       }
       case _ => {
         println(s"ERROR $thruple")
         (-2,None)
       }
     }}
    else{
      (-3,None)

    }

  }


  def tolistOfTurnsAndUniquPiecesMovedStr(tuple: (Int, Option[UniquePiece])):String = {
   val (id, someUPiece ) = tuple
    someUPiece match {
      case Some(uPiece) =>   id.toString + "," +toUPieceStr(uPiece)
      case  None => "-3,10000,♞"
    }

  }

  def fromPieceStr(str: String): UniquePiece = {
   val Seq(id:String,sym:String)=  str.split(",").toSeq
    val piece = KagHelper.pieceFromUnicode(sym)
    val uniquePiece = UniquePiece(Integer.parseInt(id) , piece)
    uniquePiece
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
