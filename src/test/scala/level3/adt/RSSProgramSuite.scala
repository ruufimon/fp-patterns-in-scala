package level3.adt

import java.nio.file.Paths

class RSSProgramSuite extends munit.FunSuite:

  private val feed =
    Feed(
      category = FeedCategory.Technology,
      source = SourceFile(Paths.get("input.xml")),
      destination = DestinationFile(Paths.get("output.txt"))
    )

  private val validXml =
    XmlContent(
      "<rss><channel><item><title>Headline</title><link>https://example.com</link></item></channel></rss>"
    )

  test("processes a feed successfully") {
    var written: Option[RenderedFeed] = None
    val process =
      RSSProgram.processFeed(
        _ => Right(validXml),
        (_, rendered) =>
          written = Some(rendered)
          Right(()),
        _ => Right(())
      )

    assertEquals(process(feed), FeedReport(Right(()), Nil))
    assertEquals(
      written,
      Some(RenderedFeed("Title: Headline\nLink: https://example.com\n---"))
    )
  }

  test("returns a typed read error") {
    val expected = FeedError.ReadError(feed.source.path, "unavailable")
    val process =
      RSSProgram.processFeed(
        _ => Left(expected),
        (_, _) => fail("write should not run after a read error"),
        _ => Right(())
      )

    assertEquals(process(feed).processingResult, Left(expected))
  }

  test("returns a typed parse error") {
    val process =
      RSSProgram.processFeed(
        _ => Right(XmlContent("<rss>")),
        (_, _) => fail("write should not run after a parse error"),
        _ => Right(())
      )

    process(feed).processingResult match
      case Left(FeedError.ParseError(message)) =>
        assert(message.nonEmpty)
      case result =>
        fail(s"expected a parse error, got $result")
  }

  test("returns a typed write error") {
    val expected = FeedError.WriteError(feed.destination.path, "read-only")
    val process =
      RSSProgram.processFeed(
        _ => Right(validXml),
        (_, _) => Left(expected),
        _ => Right(())
      )

    assertEquals(process(feed).processingResult, Left(expected))
  }

  test("continues processing later feeds after a failure") {
    val nextFeed = feed.copy(
      source = SourceFile(Paths.get("next.xml")),
      destination = DestinationFile(Paths.get("next.txt"))
    )
    val results =
      RSSProgram.run(
        feeds = List(feed, nextFeed),
        readFile = source =>
          if source.path.toString == feed.source.path.toString then
            Left(FeedError.ReadError(source.path, "unavailable"))
          else
            Right(validXml),
        writeFile = (_, _) => Right(()),
        log = _ => Right(())
      )

    assert(results.feedReports.head.processingResult.isLeft)
    assertEquals(results.feedReports(1).processingResult, Right(()))
  }

  test("converts thrown read callbacks into typed errors") {
    val report =
      RSSProgram.processFeed(
        _ => throw new IllegalStateException("read exploded"),
        (_, _) => Right(()),
        _ => Right(())
      )(feed)

    assertEquals(
      report.processingResult,
      Left(FeedError.ReadError(feed.source.path, "read exploded"))
    )
  }

  test("converts thrown write callbacks into typed errors") {
    val report =
      RSSProgram.processFeed(
        _ => Right(validXml),
        (_, _) => throw new IllegalStateException("write exploded"),
        _ => Right(())
      )(feed)

    assertEquals(
      report.processingResult,
      Left(FeedError.WriteError(feed.destination.path, "write exploded"))
    )
  }

  test("preserves processing and logging failures independently") {
    val readError = FeedError.ReadError(feed.source.path, "unavailable")
    val report =
      RSSProgram.processFeed(
        _ => Left(readError),
        (_, _) => Right(()),
        message =>
          if message.contains("[Start]") then Right(())
          else Left(FeedError.LogError(message, "logger unavailable"))
      )(feed)

    assertEquals(report.processingResult, Left(readError))
    assertEquals(report.logErrors, List(FeedError.LogError(
      s"  [Failed] Could not process Technology from ${feed.source.path}: $readError",
      "logger unavailable"
    )))
  }

  test("records thrown run-level logger failures and still completes") {
    val report =
      RSSProgram.run(
        feeds = Nil,
        readFile = _ => Right(validXml),
        writeFile = (_, _) => Right(()),
        log = _ => throw new IllegalStateException("logger exploded")
      )

    assertEquals(report.feedReports, Nil)
    assertEquals(report.logErrors.length, 2)
    assert(report.logErrors.forall {
      case FeedError.LogError(_, "logger exploded") => true
      case _ => false
    })
  }