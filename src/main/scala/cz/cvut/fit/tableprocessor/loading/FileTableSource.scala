package cz.cvut.fit.tableprocessor.loading

import cz.cvut.fit.tableprocessor.domain.Table
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path

final class FileTableSource(csvTableParser: CsvTableParser) {
  def load(path: Path, separator: String): Either[String, Table] = {
    try {
      val content = Files.readString(path)
      csvTableParser.parse(content, separator)
    } catch {
      case error: IOException => Left(s"Cannot read file '$path': ${error.getMessage}")
    }
  }
}
