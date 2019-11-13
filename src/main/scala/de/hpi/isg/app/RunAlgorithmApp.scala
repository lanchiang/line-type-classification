package de.hpi.isg.app

import com.opencsv.{CSVParser, CSVParserBuilder}
import de.hpi.isg.{Algorithm, Features}
import de.hpi.isg.elements.Line
import de.hpi.isg.evaluation.ResultEvaluation
import de.hpi.isg.io.DataReader

/**
 * @author Lan Jiang
 * @since 11/12/19
 */
class RunAlgorithmApp(val inputDataFolderPath: String, val labelFilePath: String, val delimiter: Char = ',', val quote: Char = '\"') {

  def run(): Unit = {
    val lines = preprocessing(loadData())

    val features = new Features
    features.createFeatureVector(lines)

    val algorithm = new Algorithm()
  }

  def loadData(): Array[Line] = {
    val dataReader = new DataReader(inputDataFolderPath)
    val inputData = dataReader.read() // input data
    val labels = ResultEvaluation.loadGroundTruth(labelFilePath) // labels of the input data

    val csvParser = new CSVParserBuilder().withSeparator(delimiter).withQuoteChar(quote).build()

    val lines = inputData.map(tuple => {
      val excelName = tuple._1
      val sheetName = tuple._2
      val lineNumber = tuple._4
      val result = labels.toStream.filter(label =>
        label.excelFileName.equals(excelName) &&
                label.spreadsheetName.equals(sheetName) &&
                label.lineNumber.equals(lineNumber)).head

      new Line(csvParser.parseLine(tuple._3), result)
    })
    lines
  }

  def preprocessing(lines: Array[Line]): Array[Line] = {
    lines
  }
}

object RunAlgorithmApp {

  def main(args: Array[String]): Unit = {
    val inputDataFolderPath = getClass.getResource("/excel-csv-files").toURI.getPath
    val labelFilePath = getClass.getClassLoader.getResource("annotation_result.json").toURI.getPath
    val app = new RunAlgorithmApp(inputDataFolderPath, labelFilePath)
    app.run()
  }
}


