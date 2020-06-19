package lila.app
package templating

import chess.variant.Crazyhouse
import chess.{Board, Pos, UniquePiece}

object KagemushaHelper {

  val maybeKing = "maybeKing"

  def additionalIds(uPiece: UniquePiece, crazyData: Option[Kagemusha.Data]) = crazyData match {
    case Some(cData) => {
      val v1 = if(! cData.listOfOuts.contains(uPiece)) maybeKing else ""
      // val v2 = if(! cData.recentlyMoved(3).contains(uPiece)) "🛏️" else ""
      v1
    }
    case None => ""

  }
  val maybeKingCSS = "<style> .maybeKing { " +
    "  position: relative;\n  animation: mymove 1s infinite;" +
    "}" +
    "@keyframes mymove {" +
    "  from {left: 0px;}" +
    " to {left: 3px;}" +
    "}</style>"

  def drawPieceTemplate(board: Board, top: Pos => Double, left: Pos => Double) = {
    board.crazyData.map(_.pieceMap).getOrElse(Map.empty) map {
      case (uPiece, pos) => {
        val klass = s"${uPiece.genericPiece.color.name} ${uPiece.genericPiece.role.name}"
        val additionalVariantIdentifiers = additionalIds(uPiece, board.crazyData)
        s"""<piece class="$klass $additionalVariantIdentifiers" style="top:${top(pos)}%;left:${left(pos)}%" ></piece>"""
      }
    } mkString ""
  }


}
