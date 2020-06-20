package lila.app
package templating

import chess.variant.Crazyhouse
import chess.{Board, Pos, UniquePiece}
import lila.app.ui.ScalatagsTemplate.s

object KagemushaHelper {

  val maybeKing = "kagamushu"

  def additionalIds(uPiece: UniquePiece, crazyData: Option[Crazyhouse.Data]) = crazyData match {
    case Some(cData) => {
      val v1 = if(! cData.listOfOuts.contains(uPiece)) maybeKing else ""
      // val v2 = if(! cData.recentlyMoved(3).contains(uPiece)) "🛏️" else ""
      v1
    }
    case None => ""

  }


  def drawPieceTemplate(board: Board, top: Pos => Double, left: Pos => Double) = {
    def echo(p:Pos ) = left(p) + 0.4


    board.crazyData.map(_.pieceMap).getOrElse(Map.empty) map {
      case (uPiece, pos) => {
        val klass = s"${uPiece.genericPiece.color.name} ${uPiece.genericPiece.role.name} "
        if(
          board.crazyData.map(_.listOfOuts).map(_.contains(uPiece)).getOrElse(false)){
            println(s"adding effect to ${uPiece.id}, ${uPiece.genericPiece.unicode}")

            s"""<piece class="$klass kagemushu" style="top:${top(pos)}%;left:${left(pos)}%" >
               |</piece>""".stripMargin
          }else {
          s"""<piece class="$klass " style="top:${top(pos)}%;left:${left(pos)}%" >
             |</piece>""".stripMargin
        }
      }
    } mkString ""
  }


}
