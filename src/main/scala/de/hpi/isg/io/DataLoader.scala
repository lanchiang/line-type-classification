package de.hpi.isg.io

import de.hpi.isg.elements.{Line, LineMetadata}

/**
 * @author Lan Jiang
 * @since 11/21/19
 */
abstract class DataLoader(val inputDataFolderPath: String, val labelFilePath: String) {

  def loadData(): Array[Line]

  def constructLines(inputData: Array[(String, String, Array[String], Int)], labels: Array[LineMetadata]): Array[Line] = {
    val groupedLabels = labels.groupBy(label =>(label.excelFileName, label.spreadsheetName))
    val groupedLabelsByExcelSheetLineNumber = labels.groupBy(label => (label.excelFileName, label.spreadsheetName, label.lineNumber))

    val lines = inputData.zipWithIndex.filter(tuple => {
      println(tuple._2)
//      val excelName = tuple._1
//      val sheetName = tuple._2
//      val sheetLabelCount = labels.count(label => label.excelFileName.equals(excelName) && label.spreadsheetName.equals(sheetName))
//      sheetLabelCount match {
//        case 0 => false
//        case _ => true
//      }
      val excelName = tuple._1._1
      val sheetName = tuple._1._2
//      val sheetLabelCount = labels.count(label => label.excelFileName.equals(excelName) && label.spreadsheetName.equals(sheetName))
      val sheetLabelCount = groupedLabels.get((excelName, sheetName)).size
      sheetLabelCount match {
        case 0 => false
        case _ => true
      }
    }).map(tuple => {
//      val excelName = tuple._1
//      val sheetName = tuple._2
//      val lineNumber = tuple._4
//      val result = labels.filter(label =>
//        label.excelFileName.equals(excelName) &&
//                label.spreadsheetName.equals(sheetName) &&
//                label.lineNumber.equals(lineNumber)).head
//      Line(tuple._3, result)
      val excelName = tuple._1._1
      val sheetName = tuple._1._2
      val lineNumber = tuple._1._4
//      val result = labels.filter(label => label.excelFileName.equals(excelName) &&
//              label.spreadsheetName.equals(sheetName) &&
//              label.lineNumber.equals(lineNumber)).head
      val result = groupedLabelsByExcelSheetLineNumber((excelName, sheetName, lineNumber)).head
      Line(tuple._1._3, result)
    })
    lines
  }
}
