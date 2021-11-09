package de.hpi.isg.features

import de.hpi.isg.elements.Line
import de.hpi.isg.utils.ValueFunctions.DataType
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

    // global feature: number of connected components?
    println("Calculating number of connected line components feature...")
    var numBlockCache = collection.mutable.Map[(String, String), Double]()
    val numConnectedComponents = inputData.toStream
      .map(line => {
        val linesOfFile = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
          .filter(_.lineMetadata.lineType != "empty").sortBy(_.lineMetadata.lineNumber)

        val numCC = numBlockCache.getOrElse((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName), {
          val lineNumDiffs = linesOfFile.sliding(2).map(pair => pair(1).lineMetadata.lineNumber - pair(0).lineMetadata.lineNumber)
          val numBlocks = (lineNumDiffs.count(diff => diff > 1) + 1).toDouble
          numBlockCache = numBlockCache + ((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName) -> numBlocks)
          numBlocks
        })
        (line, "NumberOfConnectedComponents", numCC)
      })

    val lines = inputData.groupBy(line => (line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName, line.lineMetadata.lineNumber))
      .mapValues(_.head)
    // position of the line
    println("Calculating line position feature...")
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
    val positionPattern = inputData.toStream
      .map(line => {
        val trimmedCellValues = line.cellValues.map(cellVal => cellVal.trim)
          .map(trimmedCellValue => {
            if (trimmedCellValue.length == 0) {
              0d
            } else {
              1d
            }
          })
//        val dcg = MathFunctions.normalizedDiscountedCumulativeGain(trimmedCellValues)
        (line, trimmedCellValues)
      }).toArray

    val dcgVector = positionPattern.toStream
      .map(line => {
        val dcg = MathFunctions.normalizedDiscountedCumulativeGain(line._2)
        (line._1, "DiscountedCumulativeGain", dcg)
      }).toArray

    //
    println("Calculating pattern difference from the most common positioning pattern...")
    val patternBySheet = positionPattern.groupBy(posPat => (posPat._1.lineMetadata.excelFileName, posPat._1.lineMetadata.spreadsheetName))

    val mostCommonPatternBySheet = patternBySheet.map(pattern => {
      val sheet_key = pattern._1
      val positionPatternFrequency = pattern._2.toStream.map(_._2).groupBy(_.mkString(" ")).mapValues(_.size)
      val mostCommonPositionPattern = positionPatternFrequency.maxBy(_._2)._1
        .split(" ")
        .map(s => s.toDouble)
      if (pattern._1._1.equals("10s0719.xls")) {
        val stop = 0
      }
      (sheet_key, mostCommonPositionPattern)
    })

    val positionSimFromMostCommon = positionPattern.toStream
        .map(line => {
          val filename = line._1.lineMetadata.excelFileName
          val sheetname = line._1.lineMetadata.spreadsheetName

          if (filename.equals("10s0006.xls")) {
            val stop = 0
          }

          val mostCommonPositionPattern = mostCommonPatternBySheet((filename, sheetname))
          val diff = line._2.zip(mostCommonPositionPattern).count(pair => pair._1 == pair._2).toDouble / mostCommonPositionPattern.length.toDouble
          (line._1, "PositionSimFromCommon", diff)
        }).toArray

//    println("Calculating emptyBefore feature...")
//    val emptyBefore = inputData.toStream
//      .map(line => {
//        val lines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
//        val startOfBefore = line.lineMetadata.lineNumber - N
//        val linesBefore = lines.filter(thatLine => thatLine.lineMetadata.lineNumber >= startOfBefore && thatLine.lineMetadata.lineNumber < line.lineMetadata.lineNumber)
//        val result = linesBefore.length match {
//          case 0 => 0d
//          case _ =>
//            linesBefore.count(thatLine => thatLine.lineMetadata.lineType.equals("empty")).toDouble / linesBefore.length.toDouble
//          //                  linesBefore.count(thatLine => thatLine.lineMetadata.lineType.contains("Empty")).toDouble / linesBefore.length.toDouble
//        }
//        (line, "EmptyBefore", result)
//      }).toArray

    // percentage of empty cells in the line
    println("Calculate empty cell ratio feature...")
    val emptyCellRatio = inputData.toStream
      .map(line => {
        val trimmedCellValues = line.cellValues.map(cellVale => cellVale.trim)
        val numEmptyCells = trimmedCellValues.count(cell => cell.length == 0)
        (line, "EmptyCellRatio", numEmptyCells.toDouble / trimmedCellValues.length.toDouble)
      }).toArray

    // how many numeric cells are there in the line?
    println("Calculate numeric cell ratio feature...")
    val numericCellRatio = inputData.toStream
      .map(line => {
        val cellValues = line.cellValues
        val cellDataTypes = valueDataType(cellValues)
        val result = cellDataTypes.count(cellDataType => cellDataType.equals(DataType.Float) || cellDataType.equals(DataType.Int)).toDouble / cellDataTypes.length.toDouble
        (line, "NumericCellRatio", result)
      }).toArray

    // how many string cells are there in the line?
    println("Calculate string cell ratio feature...")
    val stringCellRatio = inputData.toStream
      .map(line => {
        val cellValues = line.cellValues
        val cellDataTypes = valueDataType(cellValues)
        val result = cellDataTypes.count(cellDataType => cellDataType.equals(DataType.String)).toDouble / cellDataTypes.length.toDouble
        (line, "StringCellRatio", result)
      }).toArray

    // average word counts in non-empty cells
    println("Calculate average word count feature...")
    val avgWordCount = inputData.toStream
      .map(line => {
        val wordsInCells = line.cellValues.flatMap(cell => cell.split("\\s+"))
        val trimmedCellValues = line.cellValues.map(cellVale => cellVale.trim)
        val numNonEmptyCells = trimmedCellValues.count(cell => cell.length != 0)
        val result = numNonEmptyCells match {
          case 0 => 0d
          case _ => wordsInCells.length.toDouble / numNonEmptyCells.toDouble
        }
        (line, "AverageWordCount", result)
      })

    // how many empty lines are before this line?
    println("Calculating emptyBefore feature...")
    val emptyBefore = inputData.toStream
      .map(line => {
        val lines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
        val startOfBefore = line.lineMetadata.lineNumber - N
        val linesBefore = lines.filter(thatLine => thatLine.lineMetadata.lineNumber >= startOfBefore && thatLine.lineMetadata.lineNumber < line.lineMetadata.lineNumber)
        val result = linesBefore.length match {
          case 0 => 0d
          case _ =>
            linesBefore.count(thatLine => thatLine.lineMetadata.lineType.equals("empty")).toDouble / linesBefore.length.toDouble
          //                  linesBefore.count(thatLine => thatLine.lineMetadata.lineType.contains("Empty")).toDouble / linesBefore.length.toDouble
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
          case _ =>
            linesAfter.count(thatLine => thatLine.lineMetadata.lineType.equals("empty")).toDouble / linesAfter.length.toDouble
          //                  linesAfter.count(thatLine => thatLine.lineMetadata.lineType.contains("Empty")).toDouble / linesAfter.length.toDouble
        }
        (line, "EmptyAfter", result)
      }).toArray

    // histogram difference of this line and the line before
    println("Calculating histdiffBefore feature...")
    val histdiffBefore = inputData.toStream
      .map(line => {
        val thisLineValues = line.cellValues
        val thisLineValueLength = valueLength(thisLineValues)

        val lines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
        val startOfBefore = line.lineMetadata.lineNumber - N
        val linesBefore = lines.filter(thatLine => thatLine.lineMetadata.lineNumber >= startOfBefore
          && thatLine.lineMetadata.lineNumber < line.lineMetadata.lineNumber
          && thatLine.lineMetadata.lineType != "empty")
        val result = linesBefore.length match {
          case 0 => 0d
          case _ =>
            val thatLine = linesBefore.maxBy(_.lineMetadata.lineNumber)
            val thatLineValues = thatLine.cellValues
            val thatLineValueLength = valueLength(thatLineValues)
            CollectionFunctions.histogramDifference(thisLineValueLength, thatLineValueLength)
        }
        (line, "HistDiffBefore", result)
      }).toArray

    // histogram difference of this line and the line after
    println("Calculating histdiffAfter feature...")
    val histdiffAfter = inputData.toStream
      .map(line => {
        val thisLineValues = line.cellValues
        val thisLineValueLength = valueLength(thisLineValues)

        val lines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
        val endOfAfter = line.lineMetadata.lineNumber + N
        val linesAfter = lines.filter(thatLine => thatLine.lineMetadata.lineNumber > line.lineMetadata.lineNumber
          && thatLine.lineMetadata.lineNumber <= endOfAfter
          && thatLine.lineMetadata.lineType != "empty")
        val result = linesAfter.length match {
          case 0 => 0d
          case _ =>
            val thatLine = linesAfter.minBy(_.lineMetadata.lineNumber)
            val thatLineValues = thatLine.cellValues
            val thatLineValueLength = valueLength(thatLineValues)
            CollectionFunctions.histogramDifference(thisLineValueLength, thatLineValueLength)
        }

        (line, "HistDiffAfter", result)
      }).toArray

    // cell data type unmatch ratio with the previous line
    println("Calculating datatypeBefore feature...")
    val datatypeBefore = inputData.toStream
      .map(line => {
        val thisLineValues = line.cellValues
        val cellDataTypes = valueDataType(thisLineValues)

        val lines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
        val startOfBefore = line.lineMetadata.lineNumber - N
        val linesBefore = lines.filter(thatLine => thatLine.lineMetadata.lineNumber >= startOfBefore
          && thatLine.lineMetadata.lineNumber < line.lineMetadata.lineNumber
          && thatLine.lineMetadata.lineType != "empty")
        val result = linesBefore.length match {
          case 0 => 0d
          case _ =>
            val thatLine = linesBefore.maxBy(_.lineMetadata.lineNumber)
            val thatCellDataTypes = valueDataType(thatLine.cellValues)
            cellDataTypes.zip(thatCellDataTypes).count(pair => pair._1.equals(pair._2)).toDouble / cellDataTypes.length.toDouble
        }
        (line, "DataTypeMatchnessBefore", result)
      }).toArray

    // cell data type unmatch ratio with the following line
    println("Calculating datatypeAfter feature...")
    val datatypeAfter = inputData.toStream
      .map(line => {
        val thisLineValues = line.cellValues
        val cellDataTypes = valueDataType(thisLineValues)

        val lines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
        val endOfAfter = line.lineMetadata.lineNumber + N
        val linesAfter = lines.filter(thatLine => thatLine.lineMetadata.lineNumber <= endOfAfter
          && thatLine.lineMetadata.lineNumber > line.lineMetadata.lineNumber
          && thatLine.lineMetadata.lineType != "empty")
        val result = linesAfter.length match {
          case 0 => 0d
          case _ =>
            val thatLine = linesAfter.minBy(_.lineMetadata.lineNumber)
            val thatCellDataTypes = valueDataType(thatLine.cellValues)
            cellDataTypes.zip(thatCellDataTypes).count(pair => pair._1.equals(pair._2)).toDouble / cellDataTypes.length.toDouble
        }
        (line, "DataTypeMatchnessAfter", result)
      }).toArray

    // global feature: number of empty lines in this file.
    println("Calculating empty line percentage feature...")
    val emptyLinePercentage = inputData.toStream
      .map(line => {
        val linesOfSheet = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))
        val emptyLines = linesOfSheet.filter(line => "empty".equals(line.lineMetadata.lineType))
        val numOfLines = linesOfSheet.length
        (line, "EmptyLineRatio", emptyLines.length.toDouble / numOfLines.toDouble)
      })

    // global feature: number of lines
    println("Calculating number of lines feature...")
    val numOfLines = inputData.toStream
      .map(line => {
        val numOfLines = linesBySheet((line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName)).length
        // Todo: shall I normalized this feature with the maximum number of lines across all training files? Answer: for random forest not needed
        (line, "NumberOfLines", numOfLines.toDouble)
      })

    // global feature: number of columns
    println("Calculating number of columns feature...")
    val numOfColumns = inputData.toStream
      .map(line => {
        val numOfCols = line.cellValues.length
        (line, "NumberOfColumns", numOfCols.toDouble)
      })

    val result = emptyCellRatio
      .union(emptyBefore)
      .union(emptyAfter)
      .union(numericCellRatio)
      .union(stringCellRatio)
      .union(avgWordCount)
//      .union(positionSimFromMostCommon)
      .union(histdiffBefore)
      .union(histdiffAfter)
      .union(datatypeBefore)
      .union(datatypeAfter)
      .union(dcgVector)
      .union(aggIndicativeWord)
      .union(linePosition)
//      .union(emptyLinePercentage)
//      .union(numOfLines)
//      .union(numOfColumns)
//        .union(numConnectedComponents)
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
