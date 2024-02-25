//> using dep io.circe::circe-parser:0.14.6
//> using dep io.circe::circe-generic:0.14.6
import io.circe.parser
import java.util.UUID
import io.circe.syntax.*
import io.circe.*
import io.circe.generic.semiauto.*

case class Message(src: String, dest: String, body: Body)
case class Body(
    `type`: String,
    msg_id: Option[Int],
    in_reply_to: Option[Int],
    echo: String
)

given Encoder[Message] = deriveEncoder
given Encoder[Body] = deriveEncoder
given Decoder[Message] = deriveDecoder
given Decoder[Body] = deriveDecoder

case class EchoNode(id: Int) {
  def handle(in: Message): Message = {
    Message(
      src = in.dest,
      dest = in.src,
      body = Body(
        `type` = "echo_ok",
        msg_id = Some(this.id),
        in_reply_to = in.body.msg_id,
        echo = "echo_ok"
      )
    )
  }

  def run() = {
    while (true) {
      val in = scala.io.StdIn.readLine()
      parser.parse(in).flatMap(_.as[Message]).map(handle).map(_.asJson.noSpaces).foreach(println)
    }
  }
}

object ID {
  def unique = scala.util.Random.nextInt(Int.MaxValue)
}
object Main extends App {
    // {"src":"c1","dest":"n1","body":{"type":"echo","msg_id":1,"echo":"Please echo 35"}}
  val node = EchoNode(ID.unique)
  node.run()
}
