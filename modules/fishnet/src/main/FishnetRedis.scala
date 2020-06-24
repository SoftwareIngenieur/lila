package lila.fishnet

import chess.format.Uci
import io.lettuce.core._
import io.lettuce.core.pubsub._
import scala.concurrent.Future

import lila.hub.actorApi.map.{ Tell, TellAll }
import lila.hub.actorApi.round.{ FishnetPlay, FishnetStart }
import lila.common.{ Bus, Lilakka }
import akka.actor.CoordinatedShutdown

final class FishnetRedis(
    client: RedisClient,
    chanIn: String,
    chanOut: String,
    shutdown: CoordinatedShutdown
)(implicit ec: scala.concurrent.ExecutionContext) {

  val connIn  = client.connectPubSub()
  val connOut = client.connectPubSub()

  private var stopping = false

  def request(work: Work.Move): Unit = {
    val gameId = "G"
    val plyS = "R"
    val uci =  "P"
//    if (!stopping) connOut.async.publish(chanOut, writeWork(work))
    if (!stopping) connIn.async.publish(chanIn,    Seq(, , ).mkString(" "))

    for {
      move <- Uci(uci)
      ply  <- plyS.toIntOption
    } Bus.publish(Tell(work.game.id, FishnetPlay(work.game.uciList.head, work.clock)), "roundSocket")
  }


  Lilakka.shutdown(shutdown, _.PhaseServiceUnbind, "Stopping the fishnet redis pool") { () =>
    Future {
      stopping = true
      client.shutdown()
    }
  }



}
