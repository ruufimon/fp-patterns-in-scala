package level3.adt

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.util.control.NonFatal

// ---------------------------------------------------------
// Effect types
// ---------------------------------------------------------

type ReadFile =
  SourceFile => Either[FeedError, XmlContent]

type WriteFile =
  (DestinationFile, RenderedFeed) => Either[FeedError, Unit]

type Log =
  String => Either[FeedError, Unit]

private[adt] object EffectSafety:

  def message(error: Throwable): String =
    Option(error.getMessage).getOrElse(error.getClass.getSimpleName)

  def attempt[A](onError: Throwable => FeedError)(
      operation: => Either[FeedError, A]
  ): Either[FeedError, A] =
    try operation
    catch
      case NonFatal(error) =>
        Left(onError(error))

// ---------------------------------------------------------
// Concrete side effects
// ---------------------------------------------------------

object RSSEffects:

  val readFile: ReadFile =
    source =>
      EffectSafety.attempt(error =>
        FeedError.ReadError(source.path, EffectSafety.message(error))
      ) {
        val content =
          Files.readString(
            source.path,
            StandardCharsets.UTF_8
          )

        Right(XmlContent(content))
      }

  val writeFile: WriteFile =
    (destination, renderedFeed) =>
      EffectSafety.attempt(error =>
        FeedError.WriteError(destination.path, EffectSafety.message(error))
      ) {
        Option(destination.path.getParent).foreach {
          parent =>
            Files.createDirectories(parent)
        }

        Files.writeString(
          destination.path,
          renderedFeed.value,
          StandardCharsets.UTF_8
        )

        Right(())
      }

  val log: Log =
    message =>
      EffectSafety.attempt(error =>
        FeedError.LogError(message, EffectSafety.message(error))
      ) {
        println(message)
        Right(())
      }
