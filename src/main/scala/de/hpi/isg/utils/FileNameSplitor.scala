package de.hpi.isg.utils

/**
 * @author Lan Jiang
 * @since 11/12/19
 */
object FileNameSplitor {

  def getFileNameAndSheetName(fileName: String): (String, String) = {
    val nameSplits = fileName.split("@")
    val excelFileName = nameSplits(0)
    val spreadsheetName = nameSplits(1).split(".csv")(0)
    (excelFileName, spreadsheetName)
  }
}
