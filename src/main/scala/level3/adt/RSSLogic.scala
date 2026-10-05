package level3.adt

import scala.xml.{Elem, XML}
import scala.util.control.NonFatal

// ---------------------------------------------------------
// Pure logic
// ---------------------------------------------------------

object RSSLogic:

  val parseXml: XmlContent => Either[FeedError, Elem] =
    content =>
      try
        Right(XML.loadString(content.value))
      catch
        case NonFatal(error) =>
          Left(
            FeedError.ParseError(
              Option(error.getMessage).getOrElse(error.getClass.getSimpleName)
            )
          )

  val extractDocument: Elem => RSSDocument =
    xml =>
      val items =
        (xml \\ "item").toList.map { node =>
          RSSItem(
            title =
              Title(
                (node \ "title").text.trim
              ),
            link =
              ArticleLink(
                (node \ "link").text.trim
              )
          )
        }

      RSSDocument(items)

  val formatItem: RSSItem => String =
    item =>
      s"""Title: ${item.title.value}
         |Link: ${item.link.value}
         |---""".stripMargin

  val renderDocument: RSSDocument => RenderedFeed =
    document =>
      val content =
        document.items
          .map(formatItem)
          .mkString("\n")

      RenderedFeed(content)

  val processXml: XmlContent => Either[FeedError, RenderedFeed] =
    content =>
      parseXml(content)
        .map(extractDocument)
        .map(renderDocument)

  val categoryName: FeedCategory => String =
    category =>
      category match
        case FeedCategory.Technology =>
          "Technology"

        case FeedCategory.Business =>
          "Business"

        case FeedCategory.Other(name) =>
          name
