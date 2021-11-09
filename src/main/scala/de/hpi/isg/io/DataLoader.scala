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

//    val gerald = groupedLabelsByExcelSheetLineNumber.filter(tuple => tuple._1._1 == "john_griffith__15562__StMary020601.xlsx")
//    val data = inputData.filter(tupe => tupe._1 == "john_griffith__15562__StMary020601.xlsx")

    val lines = inputData.zipWithIndex.filter(tuple => {
      val excelName = tuple._1._1
      val sheetName = tuple._1._2
      val sheetLabelCount = groupedLabels.get((excelName, sheetName)).size
      sheetLabelCount match {
        case 0 => false
        case _ => true
      }
    }).map(tuple => {
      val excelName = tuple._1._1
      val sheetName = tuple._1._2
      val lineNumber = tuple._1._4

      val result = groupedLabelsByExcelSheetLineNumber((excelName, sheetName, lineNumber)).head
      Line(tuple._1._3, result)
    })
    lines
  }
}
