package ppse.visu.server

import cats.effect._
import org.http4s.blaze.server._
import org.http4s.implicits._
import org.http4s.server.Router
import java.io.File


@main def server(data: String) =

  import cats.effect.unsafe.IORuntime
  implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global

  def dataFile = new File(data)

  val httpApp = Router("/" ->RootPage.routes, "/" -> APIServer(dataFile).routes/*, "/" -> DocumentationServer.routes*/).orNotFound

  val server =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(httpApp).allocated.unsafeRunSync()._2

  println("Press any key to stop")
  System.in.read()
  server.unsafeRunSync()

