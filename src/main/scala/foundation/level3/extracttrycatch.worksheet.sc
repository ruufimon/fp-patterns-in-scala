enum ConvertError derives CanEqual:
  case UnconvertableString(value: String, message: String)

import scala.util.control.NonFatal
object TryToConvert:
    private def attempt[A](onError: Throwable => ConvertError)(
        operation: => Either[ConvertError, A]
    ): Either[ConvertError, A] =
        try operation
        catch
            case NonFatal(error) =>
                Left(onError(error))

    def toInt(target: String): Either[ConvertError, Int] =
        attempt(error => ConvertError.UnconvertableString(target, errorMessage(error)))(
            Right(target.toInt)
        )

    def errorMessage(error: Throwable): String =
        error.getMessage

val result1: Either[ConvertError, Int] = TryToConvert.toInt("123")
val result2: Either[ConvertError, Int] = TryToConvert.toInt("abc")

val sum = result1.flatMap(r1 => result2.map(_ + r1))
