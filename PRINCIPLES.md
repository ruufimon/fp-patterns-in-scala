Purity and immutability
Replace mutation and implicit state with values and pure functions.

Functional composition
Use higher-order functions such as map, filter, fold, and function composition.

Algebraic domain modelling
Represent valid states with product and sum types:
enum PaymentResult:
  case Approved(id: PaymentId)
  case Declined(reason: DeclineReason)

Typed error handling
Make absence and failure explicit with Option, Either, and domain ADTs.

Effects as values
Replace performing effects immediately with constructing descriptions:
val program: IO[User] =
  repository.findUser(id)

Polymorphic capabilities and composition
Use typeclasses and abstractions such as Functor, Applicative, Monad, MonadError, and capability algebras.
trait UserRepository[F[_]]:
  def find(id: UserId): F[User]

Declarative programs and interpreters
Use Tagless Final, Free structures, or domain-specific ADTs to separate what a program means from how it runs.
enum UserAction[A]:
  case Find(id: UserId) extends UserAction[User]
  case Save(user: User) extends UserAction[Unit]