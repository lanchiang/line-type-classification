package de.hpi.isg

import de.hpi.isg.elements.Line

/**
 * @author Lan Jiang
 * @since 10/22/19
 */
object Features {

  /**
   * Get the input data, create a feature vector out of it.
   * @param inputData
   * @return
   */
  def createFeatureVector(inputData: Array[Line]): Array[(Line, String, Double)] = {
    // is the first line?
    val isHeaderVector = inputData.toStream
            .map(line => {
              val isHeader = line.lineNumber match {
                case 1 => 1d
                case _ => 0d
              }
              (line, "IsHeader", isHeader)
            })

    // is the last line?
    val linesBySheet = inputData.groupBy(line => (line.excelFileName, line.spreadsheetName))

    val isTailVector = inputData.toStream
            .map(line => {
              val numOfLines = linesBySheet((line.excelFileName, line.spreadsheetName)).length
              var isTail = if ((line.lineNumber == numOfLines)) {
                1d
              } else {
                0d
              }
              (line, "IsTail", isTail)
            })


    null
  }
}
