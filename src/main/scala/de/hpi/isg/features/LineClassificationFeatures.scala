package de.hpi.isg.features

import de.hpi.isg.elements.Line
import de.hpi.isg.utils.ValueFunctions.DataType.DataType
import de.hpi.isg.utils.{CollectionFunctions, MathFunctions, ValueFunctions}

import scala.util.{Failure, Success, Try}

/**
 * @author Lan Jiang
 * @since 10/22/19
 */
class LineClassificationFeatures extends Features {

  /**
   * N is the number of lines looking backward or forward
   */
  private val N = 5

  private val AggregationIndicativeWords = Array("Average", "Avg", "Sum", "Total")

  /**
   * Get the input data, create a feature vector out of it.
   *
   * @param inputData the list of [[Line]]s, each of which represents a line of a data file
   * @return
   */
  override def createFeatureVector(inputData: Array[Line]): Array[(Line, String, Double)] = {
    val linesBySheet = inputData.groupBy(line => (line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
    val lines = inputData.groupBy(line => (line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName, line.lineMetadata.lineNumber))
                    .mapValues(_.head)
    // position of the line
    println("Calculating line position feature")
    val linePosition = inputData.toStream
            .map(line => {
              val lineNumber = line.lineMetadata.lineNumber
              val numOfLines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName)).length
              (line, "LinePosition", lineNumber.toDouble / numOfLines.toDouble)
            })

    // aggregation indicative words existence
    println("Calculating aggregation indicative words existence feature...")
    val aggIndicativeWord = inputData.toStream
            .map(line => {
              val wordsInCells = line.cellValues.flatMap(cell => cell.split("\\s+"))
              val wordExistence = wordsInCells.intersect(AggregationIndicativeWords).length match {
                case 0 => 0d
                case _ => 1d
              }
              (line, "AggregationIndicativeWordExistence", wordExistence)
            }).toArray

    // discounted cumulative gain (DCG) for each line
    println("Calculating discounted cumulative gain feature...")
    val dcgVector = inputData.toStream
            .map(line => {
              val trimmedCellValues = line.cellValues.map(cellVal => cellVal.trim)
                      .map(trimmedCellValue => {
                        if (trimmedCellValue.length == 0) {
                          0d
                        } else {
                          1d
                        }
                      })
              val dcg = MathFunctions.normalizedDiscountedCumulativeGain(trimmedCellValues)
              (line, "DiscountedCumulativeGain", dcg)
            }).toArray

    println("Calculate empty cell ratio feature...")
    val emptyCellRatio = inputData.toStream
            .map(line => {
              val trimmedCellValues = line.cellValues.map(cellVale => cellVale.trim)
              val numEmptyCells = trimmedCellValues.count(cell => cell.length == 0)
              (line, "EmptyCellRatio", numEmptyCells.toDouble / trimmedCellValues.length.toDouble)
            }).toArray

    // dcg for value length
    //    println("Calculating discounted cumulative gain on value length feature...")
    //    val dcgVector = inputData.toStream
    //            .map(line => {
    //              val trimmedCellValues = line.cellValues.map(cellVal => cellVal.trim).map(_.length.toDouble)
    //              val dcg = MathFunctions.normalizedDiscountedCumulativeGain(trimmedCellValues)
    //              (line, "DiscountedCumulativeGainValueLength", dcg)
    //            }).toArray

    // is the first line?
    println("Calculating isHeader feature...")
    val isHeaderVector = inputData.toStream
            .map(line => {
              val isHeader = line.lineMetadata.lineNumber match {
                case 1 => 1d
                case _ => 0d
              }
              (line, "IsHeader", isHeader)
            }).toArray

    // is the last line?
    println("Calculate isTail feature")
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
    println("Calculating emptyBefore feature...")
    val emptyBefore = inputData.toStream
            .map(line => {
              val lines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
              val startOfBefore = line.lineMetadata.lineNumber - N
              val linesBefore = lines.filter(thatLine => thatLine.lineMetadata.lineNumber >= startOfBefore && thatLine.lineMetadata.lineNumber < line.lineMetadata.lineNumber)
              val result = linesBefore.length match {
                case 0 => 0d
                case _ => {
                  linesBefore.count(thatLine => thatLine.lineMetadata.lineType.startsWith("E")).toDouble / linesBefore.length.toDouble
//                  linesBefore.count(thatLine => thatLine.lineMetadata.lineType.contains("Empty")).toDouble / linesBefore.length.toDouble
                }
              }
              (line, "EmptyBefore", result)
            }).toArray

    // how many empty lines are after this line?
    println("Calculating emptyAfter feature...")
    val emptyAfter = inputData.toStream
            .map(line => {
              val lines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
              val endOfAfter = line.lineMetadata.lineNumber + N
              val linesAfter = lines.filter(thatLine => thatLine.lineMetadata.lineNumber > line.lineMetadata.lineNumber && thatLine.lineMetadata.lineNumber <= endOfAfter)
              val result = linesAfter.length match {
                case 0 => 0d
                case _ => {
                  linesAfter.count(thatLine => thatLine.lineMetadata.lineType.startsWith("E")).toDouble / linesAfter.length.toDouble
//                  linesAfter.count(thatLine => thatLine.lineMetadata.lineType.contains("Empty")).toDouble / linesAfter.length.toDouble
                }
              }
              (line, "EmptyAfter", result)
            }).toArray

    // histogram difference of this line and the line before
    println("Calculating histdiffBefore feature...")
    val histdiffBefore = inputData.toStream
            .map(line => {
              val thisLineValues = line.cellValues
              val thisLineValueLength = valueLength(thisLineValues)
              val result = (line.lineMetadata.lineNumber - 1) match {
                case 0 => 0d
                case index: Int => {
                  val thatLine = lines(line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName, index)
//                  val thatLine = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
//                          .filter(line => line.lineMetadata.lineNumber == index).head
//                  val thatLineValues = inputData.filter(line => line.equals(thatLine)).head.cellValues
                  val thatLineValues = thatLine.cellValues
                  val thatLineValueLength = valueLength(thatLineValues)
                  CollectionFunctions.histogramDifference(thisLineValueLength, thatLineValueLength)
                }
              }
              (line, "HistDiffBefore", result)
            }).toArray

    // histogram difference of this line and the line after
    println("Calculating histdiffAfter feature...")
    val histdiffAfter = inputData.toStream
            .map(line => {
              val thisLineValues = line.cellValues
              val thisLineValueLength = valueLength(thisLineValues)
              val numOfLines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName)).length
              val result = if (line.lineMetadata.lineNumber + 1 > numOfLines) {
                0d
              } else {
                val index = line.lineMetadata.lineNumber + 1
                val thatLineValues = lines(line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName, index).cellValues
                val thatLineValueLength = valueLength(thatLineValues)
                CollectionFunctions.histogramDifference(thisLineValueLength, thatLineValueLength)
              }
              (line, "HistDiffAfter", result)
            }).toArray

    // cell data type unmatch ratio with the previous line
    println("Calculating datatypeBefore feature...")
    val datatypeBefore = inputData.toStream
            .map(line => {
              val cellValues = line.cellValues
              val cellDataTypes = valueDataType(cellValues)
              val result = (line.lineMetadata.lineNumber - 1) match {
                case 0 => 0d
                case index: Int => {
                  val thatLine = lines(line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName, index)
                  val thatCellDataTypes = valueDataType(thatLine.cellValues)
                  cellDataTypes.zip(thatCellDataTypes).count(pair => pair._1.equals(pair._2)).toDouble / cellDataTypes.length.toDouble
                }
              }
              (line, "DataTypeMatchnessBefore", result)
            }).toArray

    // cell data type unmatch ratio with the following line
    println("Calculating datatypeAfter feature...")
    val datatypeAfter = inputData.toStream
            .map(line => {
              val cellValues = line.cellValues
              val cellDataTypes = valueDataType(cellValues)
              val numOfLines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName)).length
              val result = if (line.lineMetadata.lineNumber + 1 > numOfLines) {
                0d
              } else {
                val index = line.lineMetadata.lineNumber + 1
                val thatLine = lines(line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName, index)
//                val thatLine = linesBySheet(line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName)
//                        .filter(line => line.lineMetadata.lineNumber == index).head
//                val thatCellDataTypes = valueDataType(inputData.filter(line => line.equals(thatLine)).head.cellValues)
                val thatCellDataTypes = valueDataType(thatLine.cellValues)
                cellDataTypes.zip(thatCellDataTypes).count(pair => pair._1.equals(pair._2)).toDouble / cellDataTypes.length.toDouble
              }
              (line, "DataTypeMatchnessAfter", result)
            }).toArray

    // is empty line?
    println("Calculating isEmptyLine feature...")
    val isEmptyLine = inputData.toStream
            .map(line => {
              val trimmedCellValues = line.cellValues.map(cellVal => cellVal.trim)
              val result = trimmedCellValues.count(cellValue => cellValue.length != 0) match {
                case 0 => 1d
                case _ => 0d
              }
              (line, "IsEmptyLine", result)
            }).toArray

    val result = emptyCellRatio
//            .union(isHeaderVector)
//            .union(isTailVector)
            .union(emptyBefore)
            .union(emptyAfter)
            .union(histdiffBefore)
            .union(histdiffAfter)
            .union(datatypeBefore)
            .union(datatypeAfter)
            .union(dcgVector)
            .union(aggIndicativeWord)
//            .union(isEmptyLine)
            .union(linePosition)
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
