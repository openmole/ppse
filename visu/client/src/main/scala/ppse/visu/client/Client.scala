package ppse.visu.client

import com.raquo.airstream.core

import java.nio.ByteBuffer
import org.scalajs.dom.*

import scala.concurrent.Future
import org.scalajs
import org.scalajs.dom.html.Canvas

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import com.raquo.laminar.api.L.*
import com.raquo.laminar.codecs
import com.raquo.laminar.api.features.unitArrows
import org.scalablytyped.runtime.StObject
import ppse.visu.shared.Data

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel, JSImport}
import typings.svgdotjsSvgJs.mod.*
//import typings.svgdotjsSvgPanzoomJs.mod.*

import scala.scalajs.js
import scala.scalajs.js.Object.isFrozen

//@JSExportTopLevel (name="libpanzoom")
//object PanzoomLib {
//  private lazy val dummy = typings.svgdotjsSvgPanzoomJs.mod.svgdotjsSvgJsAugmentingMod
//  def load() = dummy
//}


@JSExportTopLevel (name="visualisation")
@JSExportAll
object App:

  //def main(args: Array[String]): Unit = {
  /*val ep = s.states.last.gmm.get.parameters.head*/
  def xSize = 800 // document.body.clientWidth
  def ySize = 800 //document.body.clientHeight
  def toX(v: Double) = v * xSize
  def toY(v: Double) = v * ySize

  val forAttr = htmlAttr("for", codecs.StringAsIsCodec)

  def graph() =
    val containerNode = document.querySelector("#content")
//    EventStream.fromFuture(APIClient.runData(()).future).foreach { data =>
//      println(data)
//    }

    //val dataFile: Var[Option[String]] = Var(None)
    val runData: Var[Option[Data.RunData]] = Var(None)
    val selectedSlice: Var[Int] = Var(0)

    def getData(d: String) =
      APIClient.runData(d).future.foreach: s =>
        selectedSlice.set(s.states.size - 1)
        runData.set(Some(s))

        runSVG(draw, s, selectedSlice.now())
        val elt = document.getElementById("svg-draw").asInstanceOf[HTMLElement]
        val pan = typings.panzoomPanzoom.mod.default(elt)
        val opt = typings.panzoomPanzoom.mod.default.defaultOptions
        opt.maxScale = 100.0
        pan.setOptions(opt)
        elt.parentElement.addEventListener("wheel", pan.zoomWithWheel)




    lazy val draw = {
      // Make sure panzoom is loaded
      //isFrozen(MouseButton)
      Svg().addTo("#svg-draw").size(2 * xSize, 2 * ySize)
    }//.asInstanceOf[typings.svgdotjsSvgPanzoomJs.mod.svgdotjsSvgJsAugmentingMod.Svg].panZoom().asInstanceOf[Svg]



    val content =
      div(
        div(idAttr := "test"),
        div(cls := "row",
          div(
            cls := "col-1 text-center",
            button(`type` := "button", styleAttr := "margin-top: 10px", cls := "btn btn-secondary dropdown-toggle", idAttr := "selectDataMenu", dataAttr("bs-toggle") := "dropdown", aria.expanded := false, "Data files"),
            ul(cls := "dropdown-menu", aria.labelledBy := "selectDataMenu",
              children <--
                EventStream.fromFuture(APIClient.listRunData(()).future).map: data =>
                  data.map(d => li(cls := "dropdown-item", d, onClick --> getData(d)))
            ),
          ),
          div(
            cls := "col-5",
            child <-- runData.signal.map {
              case Some(data) =>
                div(
//                  button(`type` := "button", cls := "btn btn-secondary dropdown-toggle", idAttr := "selectStepMenu", dataAttr("bs-toggle") := "dropdown", aria.expanded := false, "Step"),
//                  ul(cls := "dropdown-menu", aria.labelledBy := "selectStepMenu",
//                    data.states.map: s =>
//                      li(cls := "dropdown-item", s.evaluation, onClick --> runSVG(draw, data, s.evaluation))
                  label(forAttr := "stepRange", cls := "form-label", child <-- selectedSlice.signal.map(s => s"Number of model evaluations: ${data.states(s).evaluation}")),
                  input(
                    `type` := "range",
                    idAttr := "stepRange",
                    cls := "form-range",
                    minAttr := "0",
                    maxAttr := (data.states.size - 1).toString,
                    stepAttr := "1",
                    value := selectedSlice.now().toString,
                    onInput.mapToValue --> { v => selectedSlice.set(v.toInt) },
                    onChange.mapToValue --> Observer[String] { v => runSVG(draw, data, v.toInt) }
                  )
                )
              //,  minAttr := data.states.head.evaluation.toString, maxAttr := data.states.last.evaluation.toString, onRe)
              case None => emptyNode
            }
          ),
          div(cls := "col-6"),
          div(
            cls := "col-12",
            div(idAttr := "svg-draw")
          ),
        )
//        (runData.signal combineWith selectedSlice.signal) --> Observer[(Option[Data.RunData], Option[Long])]{ (d, s) =>
//          println("test")
//          d.foreach(runSVG(_, s)) }
      )



//    val s = Svg("svg-draw")
//    s.ellipse(200, 100)

    render(containerNode, content)


  def runSVG(draw: Svg, d: Data.RunData, slice: Int) =
    val state = d.states(slice)

    draw.clear()

    /*draw.ellipse(toX(ep.radiusX), toY(ep.radiusY)).translate(toX(ep.centerX), toY(ep.centerY)).rotate(ep.angle)*/

    val points = state.point
    val rect_stroke = StrokeData()
    rect_stroke.width = 2
    rect_stroke.color = "#0f0"
    rect_stroke.opacity = 1.0
    draw.rect(toX(1.0),toY(1.0)).stroke(rect_stroke).fill("none")

    val point_stroke = StrokeData()
    point_stroke.width = 1
    point_stroke.color = "#ccc"
    point_stroke.opacity = 0.2
    points.foreach { point =>
      draw.circle(1).translate(toX(point(0)), toY(point(1))).stroke(point_stroke)
    }

    val gmm_stroke = StrokeData()
    gmm_stroke.width = 4
    gmm_stroke.color = "#f00"
    gmm_stroke.opacity = 0.2
    val eps =
      state.gmm.last.parameters.foreach { ep =>
        draw.ellipse(toX(ep.radiusX), toY(ep.radiusY)).center(toX(ep.centerX), toY(ep.centerY)).rotate(ep.angle).stroke(gmm_stroke).fill("none")
      }


