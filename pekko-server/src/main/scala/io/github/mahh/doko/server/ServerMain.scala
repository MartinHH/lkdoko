package io.github.mahh.doko.server

import org.apache.pekko.Done
import org.apache.pekko.actor
import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.scaladsl.adapter.*
import org.apache.pekko.http.scaladsl.ConnectionContext
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.ServerBuilder
import com.typesafe.config.Config
import io.github.mahh.doko.logic.rules.DeckRule
import io.github.mahh.doko.logic.rules.Rules
import io.github.mahh.doko.server.tableactor.ActorBasedLogicFlowFactory
import io.github.mahh.doko.server.tableactor.TableActor
import io.github.mahh.doko.server.tableflow.FlowBasedLogicFlowFactory
import org.slf4j.LoggerFactory

import java.io.InputStream
import java.security.KeyStore
import java.security.SecureRandom
import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.SSLContext
import javax.net.ssl.TrustManagerFactory
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

object ServerMain extends App {

  // this also starts the logging system from the main thread, avoiding initialization-
  // related error output that may occur otherwise:
  private val mainLogger = LoggerFactory.getLogger("io.github.mahh.doko.server.ServerMain")
  mainLogger.info("Starting server...")

  ActorSystem[Done](
    Behaviors.setup[Done] { ctx =>

      // pekko-http doesn't know about pekko typed so we create an untyped system/materializer
      implicit val untypedSystem: actor.ActorSystem = ctx.system.toClassic
      implicit val ec: ExecutionContextExecutor = ctx.system.executionContext

      // TODO: configurable rules - make this configurable
      implicit val rules: Rules = Rules(DeckRule.WithNines)

      val flowFactory =
        // new FlowBasedLogicFlowFactory
        new ActorBasedLogicFlowFactory(ctx)

      val routes = new Routes(flowFactory)

      val config = ctx.system.settings.config
      val interface = config.getString("app.interface")
      val port = config.getInt("app.port")

      val bindingFuture: Future[Http.ServerBinding] =
        enableHttpsIfConfigured(config)(Http().newServerAt(interface, port)).bind(routes.route)

      bindingFuture.onComplete {
        case Success(binding) =>
          val localAddress = binding.localAddress
          mainLogger.info(
            s"Server is listening on ${localAddress.getHostName}:${localAddress.getPort}"
          )
        case Failure(e) =>
          mainLogger.error(s"Binding failed with ${e.getMessage}")
          ctx.self ! Done
      }

      Behaviors.receiveMessage { case Done =>
        Behaviors.stopped
      }

    },
    "LKDokoPekkoHttpServer"
  )

  private def enableHttpsIfConfigured(
    config: Config
  )(serverBuilder: ServerBuilder): ServerBuilder = {

    if (config.getBoolean("app.ssl")) {
      val password = config.getString("app.sslcertpw").toCharArray

      val ks: KeyStore = KeyStore.getInstance("PKCS12")
      val keystore: InputStream =
        getClass.getClassLoader.getResourceAsStream(config.getString("app.keystorepath"))

      ks.load(keystore, password)

      val kmf = KeyManagerFactory.getInstance("SunX509")
      kmf.init(ks, password)

      val tmf = TrustManagerFactory.getInstance("SunX509")
      tmf.init(ks)

      val sslContext: SSLContext = SSLContext.getInstance("TLS")
      sslContext.init(kmf.getKeyManagers, tmf.getTrustManagers, new SecureRandom)
      serverBuilder.enableHttps(ConnectionContext.httpsServer(sslContext))
    } else {
      serverBuilder
    }

  }

}
