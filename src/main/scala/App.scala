import scalaz.{Free, Functor}

// ADT
trait Interact[Next]
case class Ask[Next](ask: String, onAsk: String => Next) extends Interact[Next]
case class Tell[Next](msg: String, next: Next) extends Interact[Next]

object InteractImplicits {
  
  implicit def functorInteract = new Functor[Interact] {
    override def map[A, B](fa: Interact[A])(f: (A) => B): Interact[B] = fa match {
      case Ask(ask, onAsk) => Ask(ask, onAsk andThen f)
      case Tell(msg, next) => Tell(msg, f(next))
    }
  }

  def ask(ask: String): Free[Interact, String] = Free.liftF(Ask(ask, identity))
  def tell(msg: String): Free[Interact, Unit] = Free.liftF(Tell(msg, ()))
  
  val script: Free[Interact, Unit] = for {
    name <- ask("What is your name?")
    _ <- tell(s"Ah, you are $name. Nice!")
  } yield ()

}


object ConsoleInterpreter  {
  import InteractImplicits._

  def apply[A, B](fu: Free[Interact, A]): Unit = fu.resume.fold({
    case Ask(ask, onAsk) =>
      println(ask)
      val name = "Mateusz"
      apply(onAsk(name))
    case Tell(msg, next) =>
      println(msg)
  }, _ => ())
}

object App extends App {
  import InteractImplicits.script

  ConsoleInterpreter(script)

}