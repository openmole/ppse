package ppse.visu.server

import org.http4s.HttpRoutes
//import scalatags.Text.all._
//import scalatags.Text.{all => tags}
import cats.effect._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`

object RootPage {

  val routes: HttpRoutes[IO] = HttpRoutes.of{
    case GET -> Root =>
      val ht =
        """<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>The Scala.js Tutorial</title>
    <script type="text/javascript" src="js/demo.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js" integrity="sha384-geWF76RCwLtnZ8qwWowPQNguL3RmwHVBC9FhGdlKrxdiJJigb/j/68SIy3Te4Bkz" crossorigin="anonymous"></script>
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-9ndCyUaIbzAi2FUVXJi0CjmCapSmO7SnpJef0486qhLnuZ2cdeRhO02iuK6FUUVM" crossorigin="anonymous">
</head>
<body>
<!-- Include Scala.js compiled code -->
<div id="content"></div>
<script>visualisation.graph();</script>
</body>
</html>
          |""".stripMargin
//        tags.html(
//          tags.head(
//            tags.meta(tags.httpEquiv := "Content-Type", tags.content := "text/html; charset=UTF-8"),
//            tags.link(tags.rel := "stylesheet", tags.`type` := "text/css", href := "css/styleWUI.css"),
//            tags.link(tags.rel := "stylesheet", tags.`type` := "text/css", href := "css/deps.css"),
//            tags.script(tags.`type` := "text/javascript", tags.src := "js/demo.js")
//          ),
//          body(
//            tags.div(id := "scalaWUI-content"),
//            tags.script("scalaWui.graph();")
//          )
//        )
      Ok.apply(ht).map(_.withContentType(`Content-Type`(MediaType.text.html)))
    case request @ GET -> Root / "js" / path =>
      import fs2.io.file.Path
      StaticFile.fromPath(Path(s"visu/server/target/webapp/js/$path"), Some(request)).getOrElseF(NotFound())
    case request @ GET -> Root / "css" / path =>
      import fs2.io.file.Path
      StaticFile.fromPath(Path(s"visu/server/target/webapp/css/$path"), Some(request)).getOrElseF(NotFound())
  }
}
