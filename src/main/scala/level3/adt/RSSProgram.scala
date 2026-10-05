package level3.adt

// ---------------------------------------------------------
// Orchestration
// ---------------------------------------------------------

object RSSProgram:

  def processFeed(
      readFile: ReadFile,
      writeFile: WriteFile,
      log: Log
  ): Feed => FeedReport =

    feed =>
      val category =
        RSSLogic.categoryName(feed.category)

      val startLogErrors =
        logSafely(
          log,
          s"  [Start] Processing $category from ${feed.source.path}"
        ).left.toOption.toList

      val processingResult =
        EffectSafety.attempt(error =>
          FeedError.ReadError(feed.source.path, EffectSafety.message(error))
        )(readFile(feed.source))
          .flatMap(RSSLogic.processXml)
          .flatMap { rendered =>
            EffectSafety.attempt(error =>
              FeedError.WriteError(
                feed.destination.path,
                EffectSafety.message(error)
              )
            )(writeFile(feed.destination, rendered))
          }

      val completionMessage =
        processingResult match
          case Right(_) =>
            s"  [Done] Saved $category to ${feed.destination.path}"
          case Left(error) =>
            s"  [Failed] Could not process $category from ${feed.source.path}: $error"

      val completionLogErrors =
        logSafely(log, completionMessage).left.toOption.toList

      FeedReport(
        processingResult,
        startLogErrors ++ completionLogErrors
      )

  def run(
      feeds: List[Feed],
      readFile: ReadFile,
      writeFile: WriteFile,
      log: Log
  ): RunReport =

    val startLogErrors =
      logSafely(log, "🚀 Starting Level 3 RSS processor...").left.toOption.toList

    val feedReports =
      feeds.map(processFeed(readFile, writeFile, log))

    val allSuccessful =
      feedReports.forall(_.processingResult.isRight) &&
        feedReports.forall(_.logErrors.isEmpty) &&
        startLogErrors.isEmpty

    val completionMessage =
      if allSuccessful then
        "✅ All feeds processed."
      else
        "⚠️ Feed processing completed with errors."

    val completionLogErrors =
      logSafely(log, completionMessage).left.toOption.toList

    RunReport(
      feedReports,
      startLogErrors ++ completionLogErrors
    )

  private def logSafely(
      log: Log,
      message: String
  ): Either[FeedError, Unit] =
    EffectSafety.attempt(error =>
      FeedError.LogError(message, EffectSafety.message(error))
    )(log(message))
