package de.hpi.isg

import java.io.File

import de.hpi.isg.elements.{Line, LineMetadata, LineValue}
import de.hpi.isg.utils.ValueFunctions.DataType.DataType
import de.hpi.isg.utils.{CollectionFunctions, ValueFunctions}

import scala.util.{Failure, Success, Try}

/**
 * @author Lan Jiang
 * @since 10/22/19
 */
class Features {

  /**
   * N is the number of lines looking backward or forward
   */
  val N = 5

  /**
   * Get the input data, create a feature vector out of it.
   *
   * @param inputData
   * @return
   */
  def createFeatureVector(inputData: Array[Line]): Array[(Line, String, Double)] = {
    // is the first line?
    val isHeaderVector = inputData.toStream
            .map(line => {
              val isHeader = line.lineMetadata.lineNumber match {
                case 1 => 1d
                case _ => 0d
              }
              (line, "IsHeader", isHeader)
            }).toArray

    // is the last line?
    val linesBySheet = inputData.groupBy(line => (line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
    val isTailVector = inputData.toStream
            .map(line => {
              val numOfLines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName)).length
              val isTail = if ((line.lineMetadata.lineNumber == numOfLines)) {
                1d
              } else {
                0d
              }
              (line, "IsTail", isTail)
            }).toArray

    // how many empty lines are before this line?
    val emptyBefore = inputData.toStream
            .map(line => {
              val lines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
              val startOfBefore = line.lineMetadata.lineNumber - N
              val linesBefore = lines.filter(thatLine => thatLine.lineMetadata.lineNumber >= startOfBefore && thatLine.lineMetadata.lineNumber < line.lineMetadata.lineNumber)
              val result = linesBefore.length match {
                case 0 => 0d
                case _ => linesBefore.count(thatLine => thatLine.lineMetadata.lineType.contains("Empty")).toDouble / linesBefore.length.toDouble
              }
              (line, "EmptyBefore", result)
            }).toArray

    // how many empty lines are after this line?
    val emptyAfter = inputData.toStream
            .map(line => {
              val lines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
              val endOfAfter = line.lineMetadata.lineNumber + N
              val linesAfter = lines.filter(thatLine => thatLine.lineMetadata.lineNumber > line.lineMetadata.lineNumber && thatLine.lineMetadata.lineNumber <= endOfAfter)
              val result = linesAfter.length match {
                case 0 => 0d
                case _ => linesAfter.count(thatLine => thatLine.lineMetadata.lineType.contains("Empty")).toDouble / linesAfter.length.toDouble
              }
              (line, "EmptyAfter", result)
            }).toArray

    // histogram difference of this line and the line before
    val histdiffBefore = inputData.toStream
            .map(line => {
              val thisLineValues = line.cellValues
              val thisLineValueLength = valueLength(thisLineValues)
              val result = (line.lineMetadata.lineNumber - 1) match {
                case 0 => 0d
                case index: Int => {
                  val thatLine = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
                          .filter(line => line.lineMetadata.lineNumber == index).head
                  val thatLineValues = inputData.filter(line => line.equals(thatLine)).head.cellValues
                  val thatLineValueLength = valueLength(thatLineValues)
                  CollectionFunctions.histogramDifference(thisLineValueLength, thatLineValueLength)
                }
              }
              (line, "HistDiffBefore", result)
            }).toArray

    // histogram difference of this line and the line after
    val histdiffAfter = inputData.toStream
            .map(line => {
              val thisLineValues = line.cellValues
              val thisLineValueLength = valueLength(thisLineValues)
              val numOfLines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName)).length
              val result = if (line.lineMetadata.lineNumber + 1 > numOfLines) {
                0d
              } else {
                val index = line.lineMetadata.lineNumber + 1
                val thatLine = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
                        .filter(line => line.lineMetadata.lineNumber == index).head
                val thatLineValues = inputData.filter(line => line.equals(thatLine)).head.cellValues
                val thatLineValueLength = valueLength(thatLineValues)
                CollectionFunctions.histogramDifference(thisLineValueLength, thatLineValueLength)
              }
              (line, "HistDiffAfter", result)
            }).toArray

    // cell data type unmatch ratio with the previous line
    val datatypeBefore = inputData.toStream
            .map(line => {
              val cellValues = line.cellValues
              val cellDataTypes = valueDataType(cellValues)
              val result = (line.lineMetadata.lineNumber - 1) match {
                case 0 => 0d
                case index: Int => {
                  val thatLine = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
                          .filter(line => line.lineMetadata.lineNumber == index).head
                  val thatCellDataTypes = valueDataType(inputData.filter(line => line.equals(thatLine)).head.cellValues)
                  cellDataTypes.zip(thatCellDataTypes).count(pair => pair._1.equals(pair._2)).toDouble / cellDataTypes.length.toDouble
                }
              }
              (line, "DataTypeMatchnessBefore", result)
            }).toArray

    // cell data type unmatch ratio with the following line
    val datatypeAfter = inputData.toStream
            .map(line => {
              val cellValues = line.cellValues
              val cellDataTypes = valueDataType(cellValues)
              val numOfLines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName)).length
              val result = if (line.lineMetadata.lineNumber + 1 > numOfLines) {
                0d
              } else {
                val index = line.lineMetadata.lineNumber + 1
                val thatLine = linesBySheet(line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName)
                        .filter(line => line.lineMetadata.lineNumber == index).head
                val thatCellDataTypes = valueDataType(inputData.filter(line => line.equals(thatLine)).head.cellValues)
                cellDataTypes.zip(thatCellDataTypes).count(pair => pair._1.equals(pair._2)).toDouble / cellDataTypes.length.toDouble
              }
              (line, "DataTypeMatchnessAfter", result)
            }).toArray

    val result = isHeaderVector
            .union(isTailVector)
            .union(emptyBefore)
            .union(emptyAfter)
            .union(histdiffBefore)
            .union(histdiffAfter)
            .union(datatypeBefore)
            .union(datatypeAfter)
    result
  }

  /**
   * Return the length of each cell of a row.
   *
   * @param values the list of cells of a row.
   * @return the list of cell length of a row.
   */
  def valueLength(values: Array[String]): Array[Double] = {
    values.map(value => {
      Try {
        value.length
      } match {
        case Success(_) => value.trim.length.toDouble
        case Failure(_) => 0d
      }
    })
  }

  def valueDataType(values: Array[String]): Array[DataType] = {
    values.map(value => {
      val dataType = ValueFunctions.detectDataType(value)
      dataType
    })
  }
}
