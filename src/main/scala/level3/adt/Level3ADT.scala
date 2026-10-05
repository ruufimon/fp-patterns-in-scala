package level3.adt

// ---------------------------------------------------------
// Application boundary
// ---------------------------------------------------------

@main def runRSSProcess(): Unit =
  RSSProgram.run(
    feeds = Level3Processor.defaultFeeds,
    readFile = RSSEffects.readFile,
    writeFile = RSSEffects.writeFile,
    log = RSSEffects.log
  )
