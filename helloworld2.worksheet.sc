import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.Instant

object FunctionalShopLevel3:

  // Product type: combines customer, quantity, and coupon.
  final case class PurchaseRequest(
      customer: String,
      quantity: Int,
      coupon: Coupon
  )

  // Product type representing a completed purchase.
  final case class Purchase(
      customer: String,
      quantity: Int,
      total: BigDecimal
  )

  // Product type representing the complete shop state.
  final case class ShopState(
      stock: Int,
      revenue: BigDecimal,
      purchases: List[Purchase]
  )

  // Sum type: a coupon must be exactly one of these cases.
  enum Coupon:
    case NoCoupon
    case Sale10
    case Vip

  // Sum type representing every expected business error.
  enum PurchaseError:
    case InvalidCustomer
    case InvalidQuantity(quantity: Int)
    case InsufficientStock(
        requested: Int,
        available: Int
    )

  // Sum type replacing the Boolean success flag.
  enum PurchaseResult:
    case Accepted(
        state: ShopState,
        purchase: Purchase
    )

    case Rejected(
        state: ShopState,
        error: PurchaseError
    )

  final case class BatchResult(
      state: ShopState,
      results: List[PurchaseResult]
  )

  // Functions continue to be treated as values.
  type Discount =
    BigDecimal => BigDecimal

  val noDiscount: Discount =
    price => price

  val tenPercentDiscount: Discount =
    price => price * 90 / 100

  val vipDiscount: Discount =
    price => price * 75 / 100

  def discountFor(coupon: Coupon): Discount =
    coupon match
      case Coupon.NoCoupon =>
        noDiscount

      case Coupon.Sale10 =>
        tenPercentDiscount

      case Coupon.Vip =>
        vipDiscount

  def calculatePrice(
      quantity: Int,
      coupon: Coupon
  ): BigDecimal =

    val unitPrice =
      BigDecimal(20)

    val originalPrice =
      BigDecimal(quantity) * unitPrice

    val applyDiscount =
      discountFor(coupon)

    applyDiscount(originalPrice)

  // Pure business logic.
  def buyProduct(
      state: ShopState,
      request: PurchaseRequest
  ): PurchaseResult =

    if request.customer.trim.isEmpty then
      PurchaseResult.Rejected(
        state,
        PurchaseError.InvalidCustomer
      )

    else if request.quantity <= 0 then
      PurchaseResult.Rejected(
        state,
        PurchaseError.InvalidQuantity(request.quantity)
      )

    else if request.quantity > state.stock then
      PurchaseResult.Rejected(
        state,
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
          revenue = state.revenue + total,
          purchases = state.purchases :+ purchase
        )

      PurchaseResult.Accepted(
        state = newState,
        purchase = purchase
      )

  // Higher-order orchestration from Level 2.
  def processPurchases(
      initialState: ShopState,
      requests: List[PurchaseRequest]
  ): BatchResult =

    requests.foldLeft(
      BatchResult(initialState, List.empty)
    ) { (batch, request) =>

      val result =
        buyProduct(batch.state, request)

      val nextState =
        result match
          case PurchaseResult.Accepted(state, _) =>
            state

          case PurchaseResult.Rejected(state, _) =>
            state

      BatchResult(
        state = nextState,
        results = batch.results :+ result
      )
    }

  // Pure presentation functions.

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

      case PurchaseError.InsufficientStock(requested, available) =>
        s"Not enough stock: requested $requested, available $available"


// Worksheet execution starts here.

import FunctionalShopLevel3.*

val initialState =
  ShopState(
    stock = 10,
    revenue = BigDecimal(0),
    purchases = List.empty
  )

val requests =
  List(
    PurchaseRequest(
      customer = "Alice",
      quantity = 2,
      coupon = Coupon.Sale10
    ),
    PurchaseRequest(
      customer = "Bob",
      quantity = 3,
      coupon = Coupon.Vip
    ),
    PurchaseRequest(
      customer = "Charlie",
      quantity = 20,
      coupon = Coupon.NoCoupon
    )
  )

val batch =
  processPurchases(initialState, requests)

// Pattern matching extracts only accepted purchases.

val acceptedPurchases =
  batch.results.collect {
    case PurchaseResult.Accepted(_, purchase) =>
      purchase
  }

// Pattern matching extracts only errors.

val errors =
  batch.results.collect {
    case PurchaseResult.Rejected(_, error) =>
      error
  }

val successfulMessages =
  acceptedPurchases.map(renderPurchase)

val failureMessages =
  errors.map(renderError)

// Side effects remain at the worksheet boundary.

successfulMessages.foreach { message =>
  println(message)

  Files.writeString(
    Path.of("sales.log"),
    s"${Instant.now()}: $message\n",
    StandardOpenOption.CREATE,
    StandardOpenOption.APPEND
  )

  ()
}

failureMessages.foreach { message =>
  println(s"Purchase failed: $message")
}

println(s"Revenue: ${batch.state.revenue}")
println(s"Remaining stock: ${batch.state.stock}")

if batch.state.stock < 3 then
  println("WARNING: Stock is low!")