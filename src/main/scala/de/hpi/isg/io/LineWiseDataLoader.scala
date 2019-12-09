package de.hpi.isg.io

import java.io.FileReader

import com.opencsv.{CSVParserBuilder, CSVReaderBuilder}
import de.hpi.isg.elements.{Line, LineMetadata}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

/**
 * @author Lan Jiang
 * @since 11/21/19
 */
class LineWiseDataLoader(override val inputDataFolderPath: String, override val labelFilePath: String)
        extends DataLoader(inputDataFolderPath, labelFilePath) {

  override def loadData(): Array[Line] = {
    val dataReader = new DataReader(inputDataFolderPath)
    val inputData = dataReader.read()

    // read ground truth
    val csvParser = new CSVParserBuilder().withQuoteChar('\"').withSeparator(',').build()
    val csvReader = new CSVReaderBuilder(new FileReader(labelFilePath)).withSkipLines(1).withCSVParser(csvParser).build()
    val labels = csvReader.readAll().asScala.map(line => {
      val excelFileName = line(0)
      val spreadsheetName = line(1)
      val lineNumber = Try{
        line(2).toInt
      } match {
        case Success(value) => value
        case Failure(exception) => throw exception
      }
      val annotation = line(3)
      LineMetadata(excelFileName, spreadsheetName, lineNumber, annotation)
    }).toArray

    val lines = super.constructLines(inputData, labels)
    lines
  }
}
