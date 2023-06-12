package ppse.visu.server

import cats.effect.*
import endpoints4s.http4s.server
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, EigenDecomposition, MatrixUtils}
import org.apache.commons.math3.random.{CorrelatedRandomVectorGenerator, GaussianRandomGenerator, JDKRandomGenerator}
import org.http4s.*
import ppse.em.GMM
import ppse.tool.Serialization
import ppse.visu.shared.{APIEndpoint, Data}


object APIServer:

  def ellipseRoute(meanX: Double, meanY: Double, covariance: Array[Array[Double]]) =
    val m = new Array2DRowRealMatrix(covariance)
    val d = new EigenDecomposition(m)
    //val v = d.getRealEigenvalues.map(p => 2.0 * math.sqrt(2) * math.sqrt(p))
    val v = d.getRealEigenvalues.map(p => math.sqrt(5.991) * math.sqrt(p))// 95 % confidence interval
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
      /*
      val means = Array(Array(0.5,0.5))
      val covariances = Array(Array(Array(0.01,-0.005),Array(-0.005,0.01)))
      val weights = Array(1.0)
      import scala.jdk.CollectionConverters.*
      val factory = new CorrelatedRandomVectorGenerator(means(0), new Array2DRowRealMatrix(covariances(0)), 1.0E-12, new GaussianRandomGenerator(new JDKRandomGenerator()))
      val points = (0 to 10000).map(_=>factory.nextVector().toVector).toVector
      def testData = Seq(Data.RunState(0L, Some(toGMMData(GMM(means, covariances, weights))), points))
      Data.RunData(testData)
      */
      Data.RunData(toStateData)
    }



  //
//  val routes: Route =
//    uuidRoute ~ fooRoute

  val routes: HttpRoutes[IO] = HttpRoutes.of(
    routesFromEndpoints(runDataRoute)
  )

}