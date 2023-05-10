package ppse.visu.client

import java.nio.ByteBuffer
import org.scalajs.dom.*

import scala.concurrent.Future
import org.scalajs
import org.scalajs.dom.html.Canvas

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import com.raquo.laminar.api.L._

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

import typings.fabric.mod.fabric.Ellipse

@JSExportTopLevel (name="visualisation")
@JSExportAll
object App {

  //def main(args: Array[String]): Unit = {

  def graph() = {
    
    println("Hello world!")

    val canvas = document.querySelector(s"#canvas").asInstanceOf[Canvas]
    println(canvas)

    val w = 300
    canvas.width = w
    canvas.height = w

    type Ctx2D = CanvasRenderingContext2D
    val ctx = canvas.getContext("2d").asInstanceOf[Ctx2D]

    ctx.strokeStyle = "red"
    ctx.lineWidth = 3
    ctx.beginPath()
    ctx.moveTo(w / 3, 0)
    ctx.lineTo(w / 3, w / 3)
    ctx.moveTo(w * 2 / 3, 0)
    ctx.lineTo(w * 2 / 3, w / 3)
    ctx.moveTo(w, w / 2)
    ctx.arc(w / 2, w / 2, w / 2, 0, 3.14)

    ctx.stroke()
//    val nodes = Seq(
//      Graph.task("one", 400, 600),
//      Graph.task("two", 1000, 600),
//      Graph.task("three", 400, 100),
//      Graph.task("four", 1000, 100),
//      Graph.task("five", 105, 60)
//    )
//    val edges = Seq(
//      Graph.edge(nodes(0), nodes(1)),
//      Graph.edge(nodes(0), nodes(2)),
//      Graph.edge(nodes(3), nodes(1)),
//      Graph.edge(nodes(3), nodes(2)))
//
//    val graphCreator = new GraphCreator(nodes, edges)

//    val containerNode = document.querySelector("#scalaWUI-content")
//
//    println(containerNode)
//    render(containerNode, graphCreator.svgNode)
  }
}

//object Post extends autowire.Client[ByteBuffer, Pickler, Pickler] {
//
//  override def doCall(req: Request): Future[ByteBuffer] = {
//    dom.ext.Ajax.post(
//      url = req.path.mkString("/"),
//      data = Pickle.intoBytes(req.args),
//      responseType = "arraybuffer",
//      headers = Map("Content-Type" -> "application/octet-stream")
//    ).map(r => TypedArrayBuffer.wrap(r.response.asInstanceOf[ArrayBuffer]))
//  }
//
//  override def read[Result: Pickler](p: ByteBuffer) = Unpickle[Result].fromBytes(p)
//
//  override def write[Result: Pickler](r: Result) = Pickle.intoBytes(r)
//
//}
