package lila.app
package templating

import chess.variant.crazy._
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
            val highlights = ctx.pref.highlight ?? lastMove.distinct.map { pos =>
              s"""<square class="last-move" style="background: radial-gradient(ellipse at center, rgba(255, 0, 0, 1) 0%, rgba(231, 0, 0, 1) 25%, rgba(169, 0, 0, 0) 89%, rgba(158, 0, 0, 0) 100%);
                 | top:${top(pos)}%;left:${left(pos)}%"></square>""".stripMargin
            } mkString ""


            val highlightsLastThree = "" /* ctx.pref.highlight ?? board.crazyData.map(_.listOfTurnsAndUniquPiecesMoved)
                .map {
                  case LastThreeMoves(b1, b2, b3, w1, w2, w3) =>
                    Seq(b1, b2, b3, w1, w2, w3).zip(Seq("b","b","b","w","w","w")).map {

                      case (Some(pos), letter) =>
                        s"""<square class="last-move last-move-${letter}" style="background: radial-gradient(ellipse at center,
                           |rgba(255, 0, 0, 1) 20%,
                           |rgba(231, 0, 0, 1) 50%,
                           |rgba(169, 0, 0, 0) 40%,
                           |rgba(158, 0, 0, 0) 40%);
                           | top:${top(pos)}%;left:${left(pos)}%"></square>""".stripMargin
                      case (None,_) => ""

                    } mkString ""*/

            val pieces =
              if (ctx.pref.isBlindfold) ""
              else {
                if(board.variant == Crazyhouse) {

                  println("Crazy house, drawing template accordingly")

                  KagemushaHelper.drawPieceTemplate(board, top _, left _ )
                }else
                {
                  board.pieces.map {
                    case (pos, piece) =>
                      val klass = s"${piece.color.name} ${piece.role.name}"
                      s"""<piece class="$klass" style="top:${top(pos)}%;left:${left(pos)}%">
                         |</piece>""".stripMargin
                  } mkString ""
                }
              }
            s"$highlights$pieces"

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
