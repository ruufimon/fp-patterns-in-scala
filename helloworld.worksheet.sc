import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.Instant
import scala.collection.mutable.ListBuffer

object ImperativeShop:

  var stock: Int = 10
  var revenue: Double = 0.0

  val purchases: ListBuffer[String] =
    ListBuffer.empty[String]

  def buyProduct(
      customer: String,
      quantity: Int,
      coupon: String
  ): Unit =

    if customer == null then
      throw new RuntimeException("Customer is null")

    if quantity <= 0 then
      throw new RuntimeException("Invalid quantity")

    if quantity > stock then
      throw new RuntimeException("Not enough stock")

    var price = quantity * 20.0

    if coupon == "SALE10" then
      price = price * 0.90
    else if coupon == "VIP" then
      price = price * 0.75

    stock = stock - quantity
    revenue = revenue + price

    val description =
      customer + " bought " +
        quantity + " items for $" + price

    purchases += description

    // Direct console side effect
    println(description)

    // Direct clock and file-system effects
    val auditMessage = s"${Instant.now()}: $description\n"

    Files.writeString(
      Path.of("sales.log"),
      auditMessage,
      StandardOpenOption.CREATE,
      StandardOpenOption.APPEND
    )

    if stock < 3 then
      println("WARNING: Stock is low!")


// Worksheet execution begins here

ImperativeShop.buyProduct(
  customer = "Alice",
  quantity = 2,
  coupon = "SALE10"
)

ImperativeShop.buyProduct(
  customer = "Bob",
  quantity = 3,
  coupon = "VIP"
)

println(s"Revenue: ${ImperativeShop.revenue}")
println(s"Remaining stock: ${ImperativeShop.stock}")
println(s"Purchases: ${ImperativeShop.purchases.toList}")