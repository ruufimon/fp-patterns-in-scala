package level3.adt

import java.nio.file.Path

// ---------------------------------------------------------
// Sum types
// ---------------------------------------------------------

enum FeedCategory derives CanEqual:
  case Technology
  case Business
  case Other(name: String)

enum FeedError derives CanEqual:
    case ReadError(source: Path, message: String)
    case ParseError(message: String)
    case WriteError(target: Path, message: String)
    case LogError(message: String, detail: String)

// ---------------------------------------------------------
// Product types
// ---------------------------------------------------------

final case class SourceFile(path: Path)

final case class DestinationFile(path: Path)

final case class Feed(
    category: FeedCategory,
    source: SourceFile,
    destination: DestinationFile
)

final case class XmlContent(value: String)

final case class Title(value: String)

final case class ArticleLink(value: String)

final case class RSSItem(
    title: Title,
    link: ArticleLink
)

final case class RSSDocument(
    items: List[RSSItem]
)

final case class RenderedFeed(
    value: String
)

final case class FeedReport(
    processingResult: Either[FeedError, Unit],
    logErrors: List[FeedError]
)

final case class RunReport(
    feedReports: List[FeedReport],
    logErrors: List[FeedError]
)
