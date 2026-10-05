import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.Instant

object FunctionalShopLevel4:

  // ---------------------------------------------------------
  // A minimal educational IO effect
  // ---------------------------------------------------------

  final case class IO[A](private val thunk: () => A):

    def map[B](f: A => B): IO[B] =
      IO(() => f(thunk()))

    def flatMap[B](f: A => IO[B]): IO[B] =
      IO(() => f(thunk()).unsafeRunSync())

    def unsafeRunSync(): A =
      thunk()

  object IO:

    def pure[A](value: A): IO[A] =
      IO(() => value)

    def delay[A](effect: => A): IO[A] =
      IO(() => effect)

  // ---------------------------------------------------------
  // Domain model
  // ---------------------------------------------------------

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

  // ---------------------------------------------------------
  // Option: an operation that might not produce a value
  // ---------------------------------------------------------

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

  // ---------------------------------------------------------
  // Either: validation with a typed error
  // ---------------------------------------------------------

  def validateRequest(
      raw: RawPurchaseRequest
  ): Either[PurchaseError, PurchaseRequest] =

    val validCustomer: Either[PurchaseError, String] =
      Option(raw.customer)
        .map(_.trim)
        .filter(_.nonEmpty)
        .toRight(PurchaseError.InvalidCustomer)

    val validQuantity: Either[PurchaseError, Int] =
      Either.cond(
        raw.quantity > 0,
        raw.quantity,
        PurchaseError.InvalidQuantity(raw.quantity)
      )

    val validCoupon: Either[PurchaseError, Coupon] =
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

  // ---------------------------------------------------------
  // Pure business logic
  // ---------------------------------------------------------

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

      val newState =
        state.copy(
          stock = state.stock - request.quantity,
          revenue = state.revenue + total
        )

      Right((newState, purchase))

  // Validation and business processing compose through Either.
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

  // ---------------------------------------------------------
  // Pure rendering
  // ---------------------------------------------------------

  def renderPurchase(purchase: Purchase): String =
    s"${purchase.customer} bought " +
      s"${purchase.quantity} items for " +
      s"$$${purchase.total}"

  def renderError(error: PurchaseError): String =
    error match
      case PurchaseError.InvalidCustomer =>
        "Customer cannot be empty"

      case PurchaseError.InvalidQuantity(quantity) =>
        s"Invalid quantity: $quantity"

      case PurchaseError.InvalidCoupon(code) =>
        s"Invalid coupon: $code"

      case PurchaseError.InsufficientStock(requested, available) =>
        s"Not enough stock: requested $requested, available $available"

  // ---------------------------------------------------------
  // Effectful operations
  // ---------------------------------------------------------

  def printLine(message: String): IO[Unit] =
    IO.delay {
      println(message)
    }

  def appendAudit(message: String): IO[Unit] =
    IO.delay {
      Files.writeString(
        Path.of("sales.log"),
        s"${Instant.now()}: $message\n",
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )

      ()
    }

  def publishPurchase(purchase: Purchase): IO[Unit] =
    val message =
      renderPurchase(purchase)

    for
      _ <- printLine(message)
      _ <- appendAudit(message)
    yield ()

  def publishError(error: PurchaseError): IO[Unit] =
    printLine(
      s"Purchase failed: ${renderError(error)}"
    )

  // Converts a list of effects into one combined effect.
  def runAll(effects: List[IO[Unit]]): IO[Unit] =
    effects.foldLeft(IO.pure(())) {
      (combined, next) =>
        combined.flatMap(_ => next)
    }


// ---------------------------------------------------------
// Worksheet program
// ---------------------------------------------------------

import FunctionalShopLevel4.*

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

// Everything above this point remains pure.
val batch =
  processPurchases(initialState, requests)

// Construct effect descriptions without executing them.
val purchaseEffects: List[IO[Unit]] =
  batch.purchases.map(publishPurchase)

val errorEffects: List[IO[Unit]] =
  batch.errors.map(publishError)

val summaryEffects =
  List(
    printLine(s"Revenue: ${batch.state.revenue}"),
    printLine(s"Remaining stock: ${batch.state.stock}")
  )

val lowStockEffect =
  if batch.state.stock < 3 then
    List(printLine("WARNING: Stock is low!"))
  else
    List.empty[IO[Unit]]

// One description of the complete effectful program.
val program: IO[Unit] =
  runAll(
    purchaseEffects ++
      errorEffects ++
      summaryEffects ++
      lowStockEffect
  )

// The effects happen only here.
program.unsafeRunSync()