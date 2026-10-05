import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.util.control.NonFatal
import java.nio.file.{AccessDeniedException, NoSuchFileException}    

val inputPath = Paths.get("output/business_news.txt")
val outputPath = Paths.get("output/business_news_copy.txt")
// -------------------------
// Domain
// -------------------------

// Business-level errors.
// The domain does not depend on Java filesystem exceptions.
enum FileError derives CanEqual:
  case NotFound(path: Path)
  case AccessDenied(path: Path)
  case InvalidPath(path: Path)
  case IoFailure(path: Path, message: String)


// -------------------------
// Port / application boundary
// -------------------------

// These functions describe what the application needs.
// They do not describe how files are accessed.
type ReadFile =
  Path => Either[FileError, String]

type WriteFile =
  (Path, String) => Either[FileError, Unit]


// -------------------------
// Infrastructure adapter
// -------------------------

object FileEffects:
    // Converts exceptions into Either values.
    // This keeps technical Java exceptions inside the adapter.
    private def attempt[A](
        operation: => A
    )(errorMapper: Throwable => FileError): Either[FileError, A] =
        try
            Right(operation)
        catch
            case NonFatal(error) =>
                Left(errorMapper(error))

    // Adapter for the ReadFile port.
    // It translates Java filesystem exceptions into FileError.
    val readFile: ReadFile =
        path => attempt(Files.readString(path, StandardCharsets.UTF_8)):
            error => mapReadError(path, error)
        
    // Adapter for the WriteFile port.
    val writeFile: WriteFile =
        (path, content) =>  attempt(Files.writeString(path, content, StandardCharsets.UTF_8)) :
            error => mapWriteError(path, error)
        .map(_ => ())

    // Technical-to-business error translation.
    private def mapReadError(
        path: Path,
        error: Throwable
    ): FileError =
        error match
        case _: AccessDeniedException => FileError.AccessDenied(path)
        case _: NoSuchFileException => FileError.NotFound(path)
        case _ => FileError.IoFailure(path, error.getMessage)


  private def mapWriteError(
      path: Path,
      error: Throwable
  ): FileError =
        error match
            case _: AccessDeniedException => FileError.AccessDenied(path)
            case _ => FileError.IoFailure(path, error.getMessage)

// -------------------------
// Application service
// -------------------------

// This is the use case.
// It depends only on ports, not on Files or Java exceptions.
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
// The concrete adapter is connected to the application here.
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