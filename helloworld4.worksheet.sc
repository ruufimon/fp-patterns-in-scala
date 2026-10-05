import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.Instant

object FunctionalShopLevel5:

  // =========================================================
  // 1. Generic functional typeclasses
  // =========================================================

  trait Functor[F[_]]:

    def map[A, B](value: F[A])(
        function: A => B
    ): F[B]

  trait Monad[F[_]] extends Functor[F]:

    def pure[A](value: A): F[A]

    def flatMap[A, B](value: F[A])(
        function: A => F[B]
    ): F[B]

    override def map[A, B](value: F[A])(
        function: A => B
    ): F[B] =
      flatMap(value) { result =>
        pure(function(result))
      }

  // Typeclass for converting a value to text.
  trait Show[A]:

    def show(value: A): String

  object Show:

    def apply[A](using instance: Show[A]): Show[A] =
      instance

    extension [A](value: A)(using instance: Show[A])

      def show: String =
        instance.show(value)

  // =========================================================
  // 2. Effect capabilities
  // =========================================================

  trait Console[F[_]]:

    def printLine(message: String): F[Unit]

  trait Audit[F[_]]:

    def append(message: String): F[Unit]

  // =========================================================
  // 3. Domain model
  // =========================================================

  enum Coupon:
    case NoCoupon
    case Sale10
    case Vip

  final case class RawPurchaseRequest(
      customer: String,
      quantity: Int,
      couponCode: String
  )

  final case class PurchaseRequest(
      customer: String,
      quantity: Int,
      coupon: Coupon
  )

  final case class Purchase(
      customer: String,
      quantity: Int,
      total: BigDecimal
  )

  final case class ShopState(
      stock: Int,
      revenue: BigDecimal
  )

  enum PurchaseError:
    case InvalidCustomer
    case InvalidQuantity(quantity: Int)
    case InvalidCoupon(code: String)
    case InsufficientStock(
        requested: Int,
        available: Int
    )

  final case class BatchResult(
      state: ShopState,
      purchases: List[Purchase],
      errors: List[PurchaseError]
  )

  // =========================================================
  // 4. Show instances
  // =========================================================

  given purchaseShow: Show[Purchase] with

    override def show(purchase: Purchase): String =
      s"${purchase.customer} bought " +
        s"${purchase.quantity} items for " +
        s"$$${purchase.total}"

  given purchaseErrorShow: Show[PurchaseError] with

    override def show(error: PurchaseError): String =
      error match
        case PurchaseError.InvalidCustomer =>
          "Customer cannot be empty"

        case PurchaseError.InvalidQuantity(quantity) =>
          s"Invalid quantity: $quantity"

        case PurchaseError.InvalidCoupon(code) =>
          s"Invalid coupon: $code"

        case PurchaseError.InsufficientStock(requested, available) =>
          s"Not enough stock: requested $requested, available $available"

  // =========================================================
  // 5. Pure validation
  // =========================================================

  def couponFromCode(code: String): Option[Coupon] =
    Option(code)
      .map(_.trim.toUpperCase)
      .flatMap {
        case "" | "NONE" =>
          Some(Coupon.NoCoupon)

        case "SALE10" =>
          Some(Coupon.Sale10)

        case "VIP" =>
          Some(Coupon.Vip)

        case _ =>
          None
      }

  def validateRequest(
      raw: RawPurchaseRequest
  ): Either[PurchaseError, PurchaseRequest] =

    val validCustomer =
      Option(raw.customer)
        .map(_.trim)
        .filter(_.nonEmpty)
        .toRight(PurchaseError.InvalidCustomer)

    val validQuantity =
      Either.cond(
        raw.quantity > 0,
        raw.quantity,
        PurchaseError.InvalidQuantity(raw.quantity)
      )

    val validCoupon =
      couponFromCode(raw.couponCode)
        .toRight(
          PurchaseError.InvalidCoupon(raw.couponCode)
        )

    for
      customer <- validCustomer
      quantity <- validQuantity
      coupon   <- validCoupon
    yield PurchaseRequest(
      customer = customer,
      quantity = quantity,
      coupon = coupon
    )

  // =========================================================
  // 6. Pure business logic
  // =========================================================

  type Discount =
    BigDecimal => BigDecimal

  def discountFor(coupon: Coupon): Discount =
    coupon match
      case Coupon.NoCoupon =>
        price => price

      case Coupon.Sale10 =>
        price => price * 90 / 100

      case Coupon.Vip =>
        price => price * 75 / 100

  def calculatePrice(
      quantity: Int,
      coupon: Coupon
  ): BigDecimal =

    val originalPrice =
      BigDecimal(quantity) * BigDecimal(20)

    discountFor(coupon)(originalPrice)

  def buyProduct(
      state: ShopState,
      request: PurchaseRequest
  ): Either[PurchaseError, (ShopState, Purchase)] =

    if request.quantity > state.stock then
      Left(
        PurchaseError.InsufficientStock(
          requested = request.quantity,
          available = state.stock
        )
      )
    else
      val total =
        calculatePrice(
          request.quantity,
          request.coupon
        )

      val purchase =
        Purchase(
          customer = request.customer,
          quantity = request.quantity,
          total = total
        )

      val nextState =
        state.copy(
          stock = state.stock - request.quantity,
          revenue = state.revenue + total
        )

      Right((nextState, purchase))

  def processPurchase(
      state: ShopState,
      raw: RawPurchaseRequest
  ): Either[PurchaseError, (ShopState, Purchase)] =

    for
      request <- validateRequest(raw)
      result  <- buyProduct(state, request)
    yield result

  def processPurchases(
      initialState: ShopState,
      requests: List[RawPurchaseRequest]
  ): BatchResult =

    requests.foldLeft(
      BatchResult(
        state = initialState,
        purchases = List.empty,
        errors = List.empty
      )
    ) { (batch, rawRequest) =>

      processPurchase(batch.state, rawRequest).fold(
        error =>
          batch.copy(
            errors = batch.errors :+ error
          ),
        success =>
          val (nextState, purchase) = success

          batch.copy(
            state = nextState,
            purchases = batch.purchases :+ purchase
          )
      )
    }

  // =========================================================
  // 7. Generic effectful program
  // =========================================================

  import Show.*

  def publishPurchase[F[_]](
      purchase: Purchase
  )(using
      monad: Monad[F],
      console: Console[F],
      audit: Audit[F]
  ): F[Unit] =

    val message =
      purchase.show

    monad.flatMap(console.printLine(message)) { _ =>
      audit.append(message)
    }

  def publishError[F[_]](
      error: PurchaseError
  )(using
      console: Console[F]
  ): F[Unit] =

    console.printLine(
      s"Purchase failed: ${error.show}"
    )

  def runAll[F[_]](
      effects: List[F[Unit]]
  )(using monad: Monad[F]): F[Unit] =

    effects.foldLeft(monad.pure(())) {
      (combined, next) =>
        monad.flatMap(combined)(_ => next)
    }

  // Notice that this function never mentions IO.
  def shopProgram[F[_]](
      batch: BatchResult
  )(using
      monad: Monad[F],
      console: Console[F],
      audit: Audit[F]
  ): F[Unit] =

    val purchaseEffects =
      batch.purchases.map { purchase =>
        publishPurchase[F](purchase)
      }

    val errorEffects =
      batch.errors.map { error =>
        publishError[F](error)
      }

    val summaryEffects =
      List(
        console.printLine(
          s"Revenue: ${batch.state.revenue}"
        ),
        console.printLine(
          s"Remaining stock: ${batch.state.stock}"
        )
      )

    val warningEffects =
      if batch.state.stock < 3 then
        List(
          console.printLine("WARNING: Stock is low!")
        )
      else
        List.empty[F[Unit]]

    runAll(
      purchaseEffects ++
        errorEffects ++
        summaryEffects ++
        warningEffects
    )

  // =========================================================
  // 8. Concrete IO implementation
  // =========================================================

  final case class IO[A](
      private val thunk: () => A
  ):

    def unsafeRunSync(): A =
      thunk()

  object IO:

    def pure[A](value: A): IO[A] =
      IO(() => value)

    def delay[A](effect: => A): IO[A] =
      IO(() => effect)

    given ioMonad: Monad[IO] with

      override def pure[A](value: A): IO[A] =
        IO.pure(value)

      override def flatMap[A, B](
          value: IO[A]
      )(
          function: A => IO[B]
      ): IO[B] =
        IO.delay {
          val result =
            value.unsafeRunSync()

          function(result).unsafeRunSync()
        }

    given ioConsole: Console[IO] with

      override def printLine(
          message: String
      ): IO[Unit] =
        IO.delay {
          println(message)
        }

    given ioAudit: Audit[IO] with

      override def append(
          message: String
      ): IO[Unit] =
        IO.delay {
          Files.writeString(
            Path.of("sales.log"),
            s"${Instant.now()}: $message\n",
            StandardOpenOption.CREATE,
            StandardOpenOption.APPEND
          )

          ()
        }


// =========================================================
// Worksheet execution
// =========================================================

import FunctionalShopLevel5.*

val initialState =
  ShopState(
    stock = 10,
    revenue = BigDecimal(0)
  )

val requests =
  List(
    RawPurchaseRequest(
      customer = "Alice",
      quantity = 2,
      couponCode = "SALE10"
    ),
    RawPurchaseRequest(
      customer = "Bob",
      quantity = 3,
      couponCode = "VIP"
    ),
    RawPurchaseRequest(
      customer = "Charlie",
      quantity = 20,
      couponCode = "NONE"
    ),
    RawPurchaseRequest(
      customer = "Diana",
      quantity = 1,
      couponCode = "UNKNOWN"
    )
  )

val batch =
  processPurchases(initialState, requests)

// The same generic program could use another effect type.
val program: IO[Unit] =
  shopProgram[IO](batch)

// Effects execute only here.
program.unsafeRunSync()