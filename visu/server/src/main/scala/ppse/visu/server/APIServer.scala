package ppse.visu.server

import cats.effect.*
import endpoints4s.http4s.server
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
      def toGMMData(gmm: ppse.em.GMM) =
        Data.GMMData(gmm.means, gmm.covariances, gmm.weights)


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
    routesFromEndpoints(uuidRoute, fooRoute, runDataRoute)
  )

}