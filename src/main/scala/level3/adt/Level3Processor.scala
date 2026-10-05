package level3.adt

import java.nio.file.Paths

// ---------------------------------------------------------
// Configuration
// ---------------------------------------------------------

object Level3Processor:

  val defaultFeeds: List[Feed] =
    List(
      Feed(
        category = FeedCategory.Technology,
        source = SourceFile(
          Paths.get("sample-data/level4/tech.xml")
        ),
        destination = DestinationFile(
          Paths.get("output/tech_news.txt")
        )
      ),
      Feed(
        category = FeedCategory.Business,
        source = SourceFile(
          Paths.get("sample-data/level4/business.xml")
        ),
        destination = DestinationFile(
          Paths.get("output/business_news.txt")
        )
      )
    )
