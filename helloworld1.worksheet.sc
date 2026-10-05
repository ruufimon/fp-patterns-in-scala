import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.Instant

object FunctionalShopLevel1:

  // Tuples remain for now.
  // Level 3 will replace these with proper domain models.
  type ShopState =
    (Int, Double, List[String])

  type PurchaseResult =
    (ShopState, String, Boolean)

  def calculatePrice(
      quantity: Int,
      coupon: String
  ): Double =

    val originalPrice = quantity * 20.0

    if coupon == "SALE10" then
      originalPrice * 0.90
    else if coupon == "VIP" then
      originalPrice * 0.75
    else
      originalPrice

  // Pure function:
  // - no mutation
  // - no println
  // - no file access
  // - no clock access
  def buyProduct(
      state: ShopState,
      customer: String,
      quantity: Int,
      coupon: String
  ): PurchaseResult =

    val (stock, revenue, purchases) = state

    if customer == null then
      (state, "Customer cannot be null", false)

    else if quantity <= 0 then
      (state, "Quantity must be positive", false)

    else if quantity > stock then
      (state, "Not enough stock", false)

    else
      val price =
        calculatePrice(quantity, coupon)

      val description =
        s"$customer bought $quantity items for $$$price"

      val newState: ShopState =
        (
          stock - quantity,
          revenue + price,
          purchases :+ description
        )

      (newState, description, true)


// Worksheet execution starts here

val initialState: FunctionalShopLevel1.ShopState =
  (10, 0.0, List.empty[String])

val (stateAfterAlice, aliceMessage, aliceSucceeded) =
  FunctionalShopLevel1.buyProduct(
    initialState,
    customer = "Alice",
    quantity = 2,
    coupon = "SALE10"
  )

val (finalState, bobMessage, bobSucceeded) =
  FunctionalShopLevel1.buyProduct(
    stateAfterAlice,
    customer = "Bob",
    quantity = 3,
    coupon = "VIP"
  )

// Side effects exist only at the worksheet boundary.

if aliceSucceeded then
  println(aliceMessage)

  Files.writeString(
    Path.of("sales.log"),
    s"${Instant.now()}: $aliceMessage\n",
    StandardOpenOption.CREATE,
    StandardOpenOption.APPEND
  )
else
  println(s"Purchase failed: $aliceMessage")

if bobSucceeded then
  println(bobMessage)

  Files.writeString(
    Path.of("sales.log"),
    s"${Instant.now()}: $bobMessage\n",
    StandardOpenOption.CREATE,
    StandardOpenOption.APPEND
  )
else
  println(s"Purchase failed: $bobMessage")

val (remainingStock, totalRevenue, purchases) =
  finalState

println(s"Revenue: $totalRevenue")
println(s"Remaining stock: $remainingStock")
println(s"Purchases: $purchases")

if remainingStock < 3 then
  println("WARNING: Stock is low!")