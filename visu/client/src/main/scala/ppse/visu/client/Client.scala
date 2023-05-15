package ppse.visu.client

import java.nio.ByteBuffer
import org.scalajs.dom.*

import scala.concurrent.Future
import org.scalajs
import org.scalajs.dom.html.Canvas

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import com.raquo.laminar.api.L.*
import ppse.visu.shared.Data

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}
import typings.fabric.mod.fabric
import typings.fabric.fabricImplMod

@JSExportTopLevel (name="visualisation")
@JSExportAll
object App:

  //def main(args: Array[String]): Unit = {

  def graph() =
    val containerNode = document.querySelector("#content")
//    EventStream.fromFuture(APIClient.runData(()).future).foreach { data =>
//      println(data)
//    }

    val content =
      div(
        canvasTag(idAttr := "canvas")
      )
    render(containerNode, content)
    run()

  def run() = APIClient.runData(()).future.foreach { s =>
    val points = s.states.last.point

    def xSize = 800 // document.body.clientWidth
    def ySize = 800 //document.body.clientHeight

    def toX(v: Double) = v * xSize
    def toY(v: Double) = v * ySize

    val c = new fabric.Canvas("canvas")
    c.setWidth(xSize*2)
    c.setHeight(ySize*2)

    points.foreach { point=>
      val o = fabricImplMod.IEllipseOptions()
      o.left = toX(point(0))
      o.top = toY(point(1))
      o.rx = 1
      o.ry = 1
      o.fill = "black"
      o.opacity = 0.5
      val p = new fabric.Ellipse(o)
      c.add(p)
    }
    val o = fabricImplMod.IEllipseOptions()

    val eps = s.states.last.gmm.last.parameters.foreach { ep =>
      o.left = toX(ep.centerX)
      o.top = toY(ep.centerY)
      o.rx = toX(ep.radiusX)
      o.ry = toY(ep.radiusY)
      o.fill = "transparent"
      o.stroke = "green"
      o.strokeWidth = 1
      o.opacity = 0.5

      val e = new fabric.Ellipse(o)

      e.rotate(ep.angle)
      c.add(e)
    }
  }
/*
    val o = fabricImplMod.IEllipseOptions()

    o.left = 5
    o.top = 5
    o.rx = 23.66431913239847
    o.ry = 15.49193338482967
    o.fill = "red"

    o.borderColor = "blue"
    val e = new fabric.Ellipse(o)
    e.rotate(45.0)

    c.add(e)
*/



//    val canvas = document.querySelector(s"#canvas").asInstanceOf[Canvas]
//    println(canvas)
//
//    val w = 300
//    canvas.width = w
//    canvas.height = w
//
//    type Ctx2D = CanvasRenderingContext2D
//    val ctx = canvas.getContext("2d").asInstanceOf[Ctx2D]
//
//    ctx.strokeStyle = "red"
//    ctx.lineWidth = 3
//    ctx.beginPath()
//    ctx.moveTo(w / 3, 0)
//    ctx.lineTo(w / 3, w / 3)
//    ctx.moveTo(w * 2 / 3, 0)
//    ctx.lineTo(w * 2 / 3, w / 3)
//    ctx.moveTo(w, w / 2)
//    ctx.arc(w / 2, w / 2, w / 2, 0, 3.14)
//
//    ctx.stroke()
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


