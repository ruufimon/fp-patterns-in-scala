import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.util.control.NonFatal
import java.nio.file.{AccessDeniedException, NoSuchFileException}    

val inputPath = Paths.get("output/business_news.txt")
val outputPath = Paths.get("output/business_news_copy.txt")


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
  (Path, String) => Either[FileError, String]

object FileEffects:
    private def attempt[A]
        (operation: => A)
        (errorMapper: Throwable => FileError)
    : Either[FileError, A] =
        try Right(operation)
        catch
            case NonFatal(error) => Left(errorMapper(error))        

    def readFile(path: Path): Either[FileError, String] =
        attempt(Files.readString(path, StandardCharsets.UTF_8)) {
            case error => FileEffects.mapReadError(path, error)
        }

    def writeFile(path: Path, content: String): Either[FileError, String] =
        attempt(Files.writeString(path, content, StandardCharsets.UTF_8)) {
            case error => FileEffects.mapWriteError(path, error)
        }.map(_ => content)

    private def mapReadError(path: Path, error: Throwable): FileError =
        error match
            case _ : AccessDeniedException => FileError.AccessDenied(path)
            case _ : NoSuchFileException => FileError.NotFound(path)
            case _ => FileError.IoFailure(path, error.getMessage)

    private def mapWriteError(path: Path, error: Throwable): FileError =
        error match
            case _ : AccessDeniedException => FileError.AccessDenied(path)
            case _ => FileError.IoFailure(path, error.getMessage)



val result =
    for
        content <- FileEffects.readFile(inputPath)
        _ <- FileEffects.writeFile(outputPath, content)
    yield ()

result match
    case Right(_) =>
        println("File copied successfully.")
    case Left(error) =>
        println(s"File operation failed: $error")