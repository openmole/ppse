package ppse.visu.shared

import endpoints4s.{algebra, circe}
import io.circe._
import io.circe.generic.auto._

/** Defines the HTTP endpoints description of a web service implementing a counter.
 * This web service has two endpoints: one for getting the current value of the counter,
 * and one for incrementing it.
 */
trait APIEndpoint
  extends algebra.Endpoints
    with algebra.circe.JsonEntitiesFromCodecs
    with circe.JsonSchemas {

  val runData: Endpoint[String, Data.RunData] =
    endpoint(
      post(path / "run-data", jsonRequest[String]),
      ok(jsonResponse[Data.RunData])
    )

  val listRunData: Endpoint[Unit, Seq[String]] =
    endpoint(
      post(path / "list-data", jsonRequest[Unit]),
      ok(jsonResponse[Seq[String]])
    )

}
