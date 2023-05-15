package ppse.visu.server

import cats.effect.*
import endpoints4s.http4s.server
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, EigenDecomposition}
import org.http4s.*
import ppse.tool.Serialization
import ppse.visu.shared.{APIEndpoint, Data}

/** Defines a Play router (and reverse router) for the endpoints described
 * in the `CounterEndpoints` trait.
 */
class APIServer(data: java.io.File)
  extends server.Endpoints[IO]
    with APIEndpoint
    with server.JsonEntitiesFromCodecs { //with server.ChunkedEntities {

  //def stringCodec[A](implicit codec: JsonCodec[A]) = codec.stringCodec
  //trait JsonSchema

  /** Simple implementation of an in-memory counter */
 // val counter = Ref(0)

  // Implements the `currentValue` endpoint
  val uuidRoute =
    uuid.implementedBy(_ => java.util.UUID.randomUUID().toString)

  // Implements the `increment` endpoint
  val fooRoute =
    foo.implementedBy(_ => Data.Foo(7))

  val runDataRoute =
    runData.implementedBy { _ =>
      val d = Serialization.load(data.listFiles().head)
      Data.RunData(d.states.map(d => Data.RunState(d.evaluation)))
    }

  val ellipseRoute =
    ellipse.implementedBy(cov => {
      val m = new Array2DRowRealMatrix(cov.covariance)
      val d = new EigenDecomposition(m)
      val v = d.getRealEigenvalues.map(p => 2.0 * math.sqrt(2) * math.sqrt(p))
      val w = d.getEigenvector(0)
      val u = w.toArray.map(p => p / w.getNorm)
      val angle = 180.0 * math.atan(u(1) / u(0)) / math.Pi
      Data.EllipseParameters(cov.meanX, cov.meanY, v(0), v(1), angle)
    })

  //
//  val routes: Route =
//    uuidRoute ~ fooRoute

  val routes: HttpRoutes[IO] = HttpRoutes.of(
    routesFromEndpoints(uuidRoute, fooRoute, runDataRoute, ellipseRoute)
  )

}