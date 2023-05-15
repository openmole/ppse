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

import typings.svgdotjsSvgJs.mod.*

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
        div(idAttr := "svg-draw"),
        canvasTag(idAttr := "canvas")
      )


//    val s = Svg("svg-draw")
//    s.ellipse(200, 100)

    render(containerNode, content)
    runSVG()

  def runSVG() = APIClient.runData(()).future.foreach { s =>
    val ep = s.states.last.gmm.get.parameters.head
    def xSize = 800 // document.body.clientWidth
    def ySize = 800 //document.body.clientHeight

    def toX(v: Double) = v * xSize
    def toY(v: Double) = v * ySize

    val draw = Svg().addTo("body").size(toX(xSize), toY(ySize))

    draw.ellipse(toX(ep.radiusX), toY(ep.radiusY)).translate(toX(ep.centerX), toY(ep.centerY)).rotate(ep.angle)

//    val ep = s.states.view.flatMap(_.gmm).head.parameters.head
//
//    def xSize = 800 // document.body.clientWidth
//
//    def ySize = 800 //document.body.clientHeight
//
//    def toX(v: Double) = v * xSize
//
//    def toY(v: Double) = v * ySize
//
//    val c = new fabric.Canvas("canvas")
//    c.setWidth(xSize)
//    c.setHeight(ySize)
//
//    val o = fabricImplMod.IEllipseOptions()
//
//    o.left = toX(ep.centerX)
//    o.top = toY(ep.centerY)
//    o.rx = toX(ep.radiusX)
//    o.ry = toY(ep.radiusY)
//    o.fill = "green"
//
//    o.borderColor = "blue"
//
//    val e = new fabric.Ellipse(o)
//
//    e.rotate(ep.angle)
//    c.add(e)
  }


  def run() = APIClient.runData(()).future.foreach { s =>
    val ep = s.states.view.flatMap(_.gmm).head.parameters.head

    def xSize = 800 // document.body.clientWidth
    def ySize = 800 //document.body.clientHeight

    def toX(v: Double) = v * xSize
    def toY(v: Double) = v * ySize

    val c = new fabric.Canvas("canvas")
    c.setWidth(xSize)
    c.setHeight(ySize)

    val o = fabricImplMod.IEllipseOptions()

    o.left = toX(ep.centerX)
    o.top = toY(ep.centerY)
    o.rx = toX(ep.radiusX)
    o.ry = toY(ep.radiusY)
    o.fill = "green"

    o.borderColor = "blue"

    val e = new fabric.Ellipse(o)

    e.rotate(ep.angle)
    c.add(e)
  }
/*
>>>>>>> d912a8eb19dd170cd0c3bdb8472f9576acb40855
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


