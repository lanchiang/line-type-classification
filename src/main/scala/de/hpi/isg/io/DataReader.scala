package de.hpi.isg.io

import java.io.{File, FileReader}

import com.opencsv.{CSVParserBuilder, CSVReaderBuilder}
import de.hpi.isg.utils.FileNameSplitor

import scala.collection.JavaConverters._

/**
 * @author Lan Jiang
 * @since 11/12/19
 */
class DataReader(val inputFileFolderPath: String, val delimiter: Char = ',', val quote: Char = '\"', val escape: Char = '\0') {

  /**
   * create for each line a 4-tuple that represents (excelName, sheetName, line, lineNumber)
   * @return the 4-tuple
   */
  def read(): Array[(String, String, Array[String], Int)] = {
    val csvParser = new CSVParserBuilder().withSeparator(delimiter).withQuoteChar(quote).withEscapeChar(escape).build()
//    val csvParser = new CSVParserBuilder().withQuoteChar('\"').withSeparator(',').withEscapeChar('\0').build

    val files = new File(inputFileFolderPath).listFiles().filter(file => !file.getName.equals(".DS_Store"))
    val result = files.flatMap(file => {
//      if (file.getName == "Git_Hub_024.csv") {
//        val stop = 0
//      }
      val csvReader = new CSVReaderBuilder(new FileReader(file)).withCSVParser(csvParser).build()
      val lines = csvReader.readAll().asScala.zipWithIndex.toStream

      val (excelName, sheetName) = FileNameSplitor.getFileNameAndSheetName(file.getName)
//      val (excelName, sheetName) = (file.getName, file.getName)
      val dataWithMetadata = lines.map(pair => (excelName, sheetName, pair._1, pair._2 + 1)).toArray

      csvReader.close()
      dataWithMetadata
    })
//    val fileResult = result.filter(tuple => tuple._1.equals("john_lavorato__16476__Schedule C NA.xlsx")).toArray
    result
  }
}

object DataReader {
  def main(args: Array[String]): Unit = {
    val string = Array("1","1.d", "2,3", "d").mkString(",")
    println(string)
  }
}

