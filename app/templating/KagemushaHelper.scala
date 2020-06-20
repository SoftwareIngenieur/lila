package lila.app
package templating

import chess.variant.Crazyhouse
import chess.{Board, Pos, UniquePiece}

object KagemushaHelper {

  val maybeKing = "maybeKing"

  def additionalIds(uPiece: UniquePiece, crazyData: Option[Crazyhouse.Data]) = crazyData match {
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
    def echo(p:Pos ) = left(p) + 0.4
    board.crazyData.map(_.pieceMap).getOrElse(Map.empty) map {
      case (uPiece, pos) => {
        val klass = s"${uPiece.genericPiece.color.name} ${uPiece.genericPiece.role.name}"
        val additionalVariantIdentifiers = additionalIds(uPiece, board.crazyData)
        s"""<piece class="$klass $additionalVariantIdentifiers" style="top:${top(pos)}%;left:${left(pos)}%" >
           |<img src="smiley.gif" alt="👑👑👑👑👑👑" style="color: #660202;z-index: 200;transform: translate(0.9833px);">
           |</piece>
           |<piece class="$klass " style="top:${top(pos)}%;left:${echo(pos)}%" ></piece>""".stripMargin
      }
    } mkString ""
  }


}
