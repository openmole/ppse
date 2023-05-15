package ppse.visu.server

import cats.effect.*
import endpoints4s.http4s.server
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, EigenDecomposition}
import org.http4s.*
import ppse.tool.Serialization
import ppse.visu.shared.{APIEndpoint, Data}


object APIServer:

  def ellipseRoute(meanX: Double, meanY: Double, covariance: Array[Array[Double]]) =
    val m = new Array2DRowRealMatrix(covariance)
    val d = new EigenDecomposition(m)
    val v = d.getRealEigenvalues.map(p => 2.0 * math.sqrt(2) * math.sqrt(p))
    val w = d.getEigenvector(0)
    val u = w.toArray.map(p => p / w.getNorm)
    val angle = 180.0 * math.atan(u(1) / u(0)) / math.Pi
    Data.EllipseParameters(meanX, meanY, v(0), v(1), angle)

/** Defines a Play router (and reverse router) for the endpoints described
 * in the `CounterEndpoints` trait.
 */
class APIServer(data: java.io.File)
  extends server.Endpoints[IO]
    with APIEndpoint
    with server.JsonEntitiesFromCodecs { //with server.ChunkedEntities {

  val runDataRoute =
    runData.implementedBy { _ =>
      def toGMMData(gmm: ppse.em.GMM) =
        val params =
          (gmm.means zip gmm.covariances) map { (m, c) =>
            APIServer.ellipseRoute(m(0), m(1), c)
          }

        Data.GMMData(
          gmm.means,
          gmm.covariances,
          gmm.weights,
          params)


      val d = Serialization.load(data.listFiles().head)

      def toStateData =
        d.states.map { s =>
          Data.RunState(
            s.evaluation,
            s.gmm.map(toGMMData),
            s.point
          )
        }

      Data.RunData(toStateData)
    }



  //
//  val routes: Route =
//    uuidRoute ~ fooRoute

  val routes: HttpRoutes[IO] = HttpRoutes.of(
    routesFromEndpoints(runDataRoute)
  )

}