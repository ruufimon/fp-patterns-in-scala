import java.nio.charset.StandardCharsets
import java.nio.file.{AccessDeniedException, Files, InvalidPathException, NoSuchFileException, Path, Paths}
import scala.util.control.NonFatal

val inputPath = Paths.get("output/business_news.txt")
val outputPath = Paths.get("output/business_news_copy.txt")

// -------------------------
// Domain
// -------------------------
enum FileError derives CanEqual:
  case NotFound(path: Path)
  case AccessDenied(path: Path)
  case InvalidPath(path: Path)
  case IoFailure(path: Path, message: String)

// -------------------------
// Ports
// -------------------------
type ReadFile =
  Path => Either[FileError, String]

type WriteFile =
  (Path, String) => Either[FileError, Unit]

// -------------------------
// Infrastructure adapter
// -------------------------
object FileEffects:
  // Convert technical exceptions into business-level errors.
  private def attempt[A](
        operation: => A)(
        errorMapper: Throwable => FileError
  ): Either[FileError, A] =
    try
      Right(operation)
    catch
      case NonFatal(error) =>
        Left(errorMapper(error))

  private def message(error: Throwable): String =
    Option(error.getMessage).getOrElse(error.getClass.getSimpleName)

  val readFile: ReadFile =
    path =>
      attempt(
        Files.readString(path, StandardCharsets.UTF_8))(
        error => mapReadError(path, error)
      )

  val writeFile: WriteFile =
    (path, content) =>
      attempt {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.writeString(path, content, StandardCharsets.UTF_8)
      }(
        error => mapWriteError(path, error)
      ).map(_ => ())

  private def mapReadError(
      path: Path,
      error: Throwable
  ): FileError =
    error match
      case _: AccessDeniedException =>
        FileError.AccessDenied(path)
      case _: NoSuchFileException =>
        FileError.NotFound(path)
      case _: InvalidPathException =>
        FileError.InvalidPath(path)
      case _ =>
        FileError.IoFailure(path, message(error))

  private def mapWriteError(
      path: Path,
      error: Throwable
  ): FileError =
    error match
      case _: AccessDeniedException =>
        FileError.AccessDenied(path)
      case _: InvalidPathException =>
        FileError.InvalidPath(path)
      case _ =>
        FileError.IoFailure(path, message(error))

// -------------------------
// Application service
// -------------------------
def copyFile(
    input: Path,
    output: Path,
    read: ReadFile,
    write: WriteFile
): Either[FileError, Unit] =
  for
    content <- read(input)
    _ <- write(output, content)
  yield ()

// -------------------------
// Composition root
// -------------------------
val result =
  copyFile(
    inputPath,
    outputPath,
    FileEffects.readFile,
    FileEffects.writeFile
  )

result match
  case Right(_) =>
    println("File copied successfully.")
  case Left(error) =>
    println(s"File operation failed: $error")
