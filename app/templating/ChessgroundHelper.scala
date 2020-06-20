package lila.app
package templating

import chess.variant.Crazyhouse
import chess.{Board, Color, Pos}
import lila.api.Context
import lila.app.ui.ScalatagsTemplate._
import lila.game.Pov

trait ChessgroundHelper {

  private val cgWrap      = div(cls := "cg-wrap")
  private val cgHelper    = tag("cg-helper")
  private val cgContainer = tag("cg-container")
  private val cgBoard     = tag("cg-board")
  val cgWrapContent       = cgHelper(cgContainer(cgBoard))

  def chessground(board: Board, orient: Color, lastMove: List[Pos] = Nil)(implicit ctx: Context): Frag = {

    println(board.crazyData.map(_.listOfOuts.mkString(",")).getOrElse(""))
    wrap {
      cgBoard {
        raw {
          if (ctx.pref.is3d) ""
          else {
            def top(p: Pos)  = orient.fold(8 - p.y, p.y - 1) * 12.5
            def left(p: Pos) = orient.fold(p.x - 1, 8 - p.x) * 12.5

            val pieces =
              if (ctx.pref.isBlindfold) ""
              else {
                if(board.variant == Crazyhouse) {
                  KagemushaHelper.drawPieceTemplate(board, top _, left _ )
                }else
                {
                  board.pieces.map {
                    case (pos, piece) =>
                      val klass = s"${piece.color.name} ${piece.role.name}"
                      s"""<piece class="$klass maybeKing" style="top:${top(pos)}%;left:${left(pos)}%">
                         |<img src="smiley.gif" alt="Smiley face" style="color: #660202;z-index: 200;transform: translate(47.9833px);">
                         |</piece>""".stripMargin
                  } mkString ""
                }
              }
            s"$highlights$nonOuted$pieces"

          }
        }
      }
    }
  }

  def chessground(pov: Pov)(implicit ctx: Context): Frag =
    chessground(
      board = pov.game.board,
      orient = pov.color,
      lastMove = pov.game.history.lastMove.map(_.origDest) ?? {
        case (orig, dest) => List(orig, dest)
      }
    )

  private def wrap(content: Frag): Frag =
    cgWrap {
      cgHelper {
        cgContainer {
          content
        }
      }
    }

  lazy val chessgroundBoard = wrap(cgBoard)
}
