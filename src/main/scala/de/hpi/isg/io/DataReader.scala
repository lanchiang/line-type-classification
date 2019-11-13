package de.hpi.isg.io

import java.io.File

import de.hpi.isg.utils.FileNameSplitor

import scala.io.Source

/**
 * @author Lan Jiang
 * @since 11/12/19
 */
class DataReader(val inputFileFolderPath: String) {

  /**
   * create for each line a 4-tuple that represents (excelName, sheetName, line, lineNumber)
   * @return the 4-tuple
   */
  def read(): Array[(String, String, String, Int)] = {
    val files = new File(inputFileFolderPath).listFiles().filter(file => !file.getName.equals(".DS_Store"))
    val result = files.flatMap(file => {
      val bufferedSource = Source.fromFile(file)
      val lines = bufferedSource.getLines().toStream.zipWithIndex

      val (excelName, sheetName) = FileNameSplitor.getFileNameAndSheetName(file.getName)
      val dataWithMetadata = lines.map(pair => (excelName, sheetName, pair._1, pair._2 + 1)).toArray

      bufferedSource.close()
      dataWithMetadata
    })
    result
  }
}
