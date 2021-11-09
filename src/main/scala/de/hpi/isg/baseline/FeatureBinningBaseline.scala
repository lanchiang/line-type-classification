package de.hpi.isg.baseline

import java.io.{BufferedWriter, File, FileWriter}

import de.hpi.isg.app.RunAlgorithmApp
import de.hpi.isg.app.RunAlgorithmApp.getClass
import de.hpi.isg.elements.Line
import de.hpi.isg.utils.{DateFormatUtils, MathFunctions}

/**
 * @author Lan Jiang
 * @since 12/9/19
 */
class FeatureBinningBaseline {

  def createFeatureVector(inputData: Array[Line]): Array[(Line, String, String)] = {
    val groupsBySheet = inputData.groupBy(line => (line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))

    val samples = inputData.flatMap(line => line.cellValues).filter(cell => cell.trim.length != 0)
    val meanValueLength = samples.map(_.length).sum.toDouble / samples.length.toDouble
    val valueLengthStandardDeviation = MathFunctions.std(samples.map(_.length))

    val result = groupsBySheet.flatMap(group => {
      val sortedLines = group._2.sortBy(_.lineMetadata.lineNumber) // the lines of a single file sorted by the line number

      val isEmptyFeature = sortedLines.map(line => {
        val emptyCount = line.cellValues.count(cell => cell.trim.length == 0)
        val emptyFeatureBin = FeatureBinning.getBinFeature(emptyCount, line.cellValues.length)
        (line, "IsEmpty", emptyFeatureBin)
      })

      var isEmptySimFeature = Array((sortedLines(0), "IsEmptySim", "-"))
      val isEmptySimFeatureHinter = sortedLines.sliding(2).map(linePair => {
        val line = linePair(1).cellValues.zip(linePair(0).cellValues)
        val bothEmptyCount = line.count(pair => pair._1.trim.length == 0 && pair._2.trim.length == 0)
        val bothEmptyFeatureBin = FeatureBinning.getBinFeature(bothEmptyCount, line.length)
        (linePair(1), "IsEmptySim", bothEmptyFeatureBin)
      }).toArray
      isEmptySimFeature = isEmptySimFeature ++ isEmptySimFeatureHinter

      var isEmptyNotSimFeature = Array((sortedLines(0), "IsEmptyNotSim", "-"))
      val isEmptyNotSimFeatureHinter = sortedLines.sliding(2).map(linePair => {
        val line = linePair(1).cellValues.zip(linePair(0).cellValues)
        val inverseBothEmptyCount = line.count(pair => pair._1.trim.length == 0 && pair._2.trim.length != 0)
        val inverseBothEmptyFeatureBin = FeatureBinning.getBinFeature(inverseBothEmptyCount, line.length)
        (linePair(1), "IsEmptyNotSim", inverseBothEmptyFeatureBin)
      }).toArray
      isEmptyNotSimFeature = isEmptyNotSimFeature ++ isEmptyNotSimFeatureHinter

      val isTextFeature = sortedLines.map(line => {
        val isTextCount = line.cellValues.count(cell => (!cell.equals("") && (cell forall Character.isLetter)))
        val isTextFeatureBin = FeatureBinning.getBinFeature(isTextCount, line.cellValues.length)
        (line, "IsText", isTextFeatureBin)
      })

      var isTextSimFeature = Array((sortedLines(0), "IsTextSim", "-"))
      val isTextSimFeatureHinter = sortedLines.sliding(2).map(linePair => {
        val line = linePair(1).cellValues.zip(linePair(0).cellValues)
        val bothTextCount = line.count(pair => isText(pair._1) && isText(pair._2))
        val bothTextFeatureBin = FeatureBinning.getBinFeature(bothTextCount, line.length)
        (linePair(1), "IsTextSim", bothTextFeatureBin)
      }).toArray
      isTextSimFeature = isTextSimFeature ++ isTextSimFeatureHinter

      var isTextNotSimFeature = Array((sortedLines(0), "IsTextNotSim", "-"))
      val isTextNotSimFeatureHinter = sortedLines.sliding(2).map(linePair => {
        val line = linePair(1).cellValues.zip(linePair(0).cellValues)
        val inverseBothTextCount = line.count(pair => isText(pair._1) && !isText(pair._2))
        val inverseBothTextFeatureBin = FeatureBinning.getBinFeature(inverseBothTextCount, line.length)
        (linePair(1), "IsTextNotSim", inverseBothTextFeatureBin)
      }).toArray
      isTextNotSimFeature = isTextNotSimFeature ++ isTextNotSimFeatureHinter

      val isNumericFeature = sortedLines.map(line => {
        val isNumericCount = line.cellValues.count(cell => isNumeric(cell))
        val isDigitFeatureBin = FeatureBinning.getBinFeature(isNumericCount, line.cellValues.length)
        (line, "IsNumeric", isDigitFeatureBin)
      })

      var isNumericSimFeature = Array((sortedLines(0), "IsNumericSim", "-"))
      val isNumericSimFeatureHinter = sortedLines.sliding(2).map(linePair => {
        val line = linePair(1).cellValues.zip(linePair(0).cellValues)
        val bothNumericCount = line.count(pair => isNumeric(pair._1) && isNumeric(pair._2))
        val bothNumericFeatureBin = FeatureBinning.getBinFeature(bothNumericCount, line.length)
        (linePair(1), "IsNumericSim", bothNumericFeatureBin)
      }).toArray
      isNumericSimFeature = isNumericSimFeature ++ isNumericSimFeatureHinter

      var isNumericNotSimFeature = Array((sortedLines(0), "IsNumericNotSim", "-"))
      val isNumericNotSimFeatureHinter = sortedLines.sliding(2).map(linePair => {
        val line = linePair(1).cellValues.zip(linePair(0).cellValues)
        val inverseBothNumericCount = line.count(pair => isNumeric(pair._1) && !isNumeric(pair._2))
        val inverseBothNumericFeatureBin = FeatureBinning.getBinFeature(inverseBothNumericCount, line.length)
        (linePair(1), "IsNumericNotSim", inverseBothNumericFeatureBin)
      }).toArray
      isNumericNotSimFeature = isNumericNotSimFeature ++ isNumericNotSimFeatureHinter

      val isDateFeature = sortedLines.map(line => {
        val isDateCount = line.cellValues.count(cell => DateFormatUtils.isDate(cell))
        val isDateFeatureBin = FeatureBinning.getBinFeature(isDateCount, line.cellValues.length)
        (line, "IsDate", isDateFeatureBin)
      })

      var isDateSimFeature = Array((sortedLines(0), "IsDateSim", "-"))
      val isDateSimFeatureHinter = sortedLines.sliding(2).map(linePair => {
        val line = linePair(1).cellValues.zip(linePair(0).cellValues)
        val bothDateCount = line.count(pair => DateFormatUtils.isDate(pair._1) && DateFormatUtils.isDate(pair._2))
        val bothDateFeatureBin = FeatureBinning.getBinFeature(bothDateCount, line.length)
        (linePair(1), "IsNumericSim", bothDateFeatureBin)
      }).toArray
      isDateSimFeature = isDateSimFeature ++ isDateSimFeatureHinter

      var isDateNotSimFeature = Array((sortedLines(0), "IsDateNotSim", "-"))
      val isDateNotSimFeatureHinter = sortedLines.sliding(2).map(linePair => {
        val line = linePair(1).cellValues.zip(linePair(0).cellValues)
        val inverseBothDateCount = line.count(pair => DateFormatUtils.isDate(pair._1) && !DateFormatUtils.isDate(pair._2))
        val inverseBothDateFeatureBin = FeatureBinning.getBinFeature(inverseBothDateCount, line.length)
        (linePair(1), "IsNumericNotSim", inverseBothDateFeatureBin)
      }).toArray
      isDateNotSimFeature = isDateNotSimFeature ++ isDateNotSimFeatureHinter

      val isShortTextFeature = sortedLines.map(line => {
        val isShortCount = line.cellValues.count(cell => cell.length.toDouble < (meanValueLength - valueLengthStandardDeviation))
        val isShortTextFeatureBin = FeatureBinning.getBinFeature(isShortCount, line.cellValues.length)
        (line, "IsShortText", isShortTextFeatureBin)
      })

      var isShortTextSimFeature = Array((sortedLines(0), "IsShortTextSim", "-"))
      val isShortTextSimFeatureHinter = sortedLines.sliding(2).map(linePair => {
        val line = linePair(1).cellValues.zip(linePair(0).cellValues)
        val bothShortTextCount = line.count(pair => isShortText(pair._1, meanValueLength, valueLengthStandardDeviation) && isShortText(pair._2, meanValueLength, valueLengthStandardDeviation))
        val bothShortTextFeatureBin = FeatureBinning.getBinFeature(bothShortTextCount, line.length)
        (linePair(1), "IsShortTextSim", bothShortTextFeatureBin)
      }).toArray
      isShortTextSimFeature = isShortTextSimFeature ++ isShortTextSimFeatureHinter

      var isShortTextNotSimFeature = Array((sortedLines(0), "IsShortTextNotSim", "-"))
      val isShortTextNotSimFeatureHinter = sortedLines.sliding(2).map(linePair => {
        val line = linePair(1).cellValues.zip(linePair(0).cellValues)
        val inverseBothShortTextCount = line.count(pair => isShortText(pair._1, meanValueLength, valueLengthStandardDeviation) && !isShortText(pair._2, meanValueLength, valueLengthStandardDeviation))
        val inverseBothLongTextFeatureBin = FeatureBinning.getBinFeature(inverseBothShortTextCount, line.length)
        (linePair(1), "IsShortTextNotSim", inverseBothLongTextFeatureBin)
      }).toArray
      isShortTextNotSimFeature = isShortTextNotSimFeature ++ isShortTextNotSimFeatureHinter

      val isLongTextFeature = sortedLines.map(line => {
        val isLongTextCount = line.cellValues.count(cell => cell.length.toDouble > (meanValueLength + valueLengthStandardDeviation))
        val isLongTextFeatureBin = FeatureBinning.getBinFeature(isLongTextCount, line.cellValues.length)
        (line, "IsLongText", isLongTextFeatureBin)
      })

      var isLongTextSimFeature = Array((sortedLines(0), "IsLongTextSim", "-"))
      val isLongTextSimFeatureHinter = sortedLines.sliding(2).map(linePair => {
        val line = linePair(1).cellValues.zip(linePair(0).cellValues)
        val bothLongTextCount = line.count(pair => isLongText(pair._1, meanValueLength, valueLengthStandardDeviation) && isLongText(pair._2, meanValueLength, valueLengthStandardDeviation))
        val bothLongTextFeatureBin = FeatureBinning.getBinFeature(bothLongTextCount, line.length)
        (linePair(1), "IsLongTextSim", bothLongTextFeatureBin)
      }).toArray
      isLongTextSimFeature = isLongTextSimFeature ++ isLongTextSimFeatureHinter

      var isLongTextNotSimFeature = Array((sortedLines(0), "IsLongTextNotSim", "-"))
      val isLongTextNotSimFeatureHinter = sortedLines.sliding(2).map(linePair => {
        val line = linePair(1).cellValues.zip(linePair(0).cellValues)
        val inverseBothLongTextCount = line.count(pair => isLongText(pair._1, meanValueLength, valueLengthStandardDeviation) && !isLongText(pair._2, meanValueLength, valueLengthStandardDeviation))
        val inverseBothLongTextFeatureBin = FeatureBinning.getBinFeature(inverseBothLongTextCount, line.length)
        (linePair(1), "IsLongTextNotSim", inverseBothLongTextFeatureBin)
      }).toArray
      isLongTextNotSimFeature = isLongTextNotSimFeature ++ isLongTextNotSimFeatureHinter

      val isTotal = sortedLines.map(line => {
        val isTotalCount = line.cellValues.count(cell => cell.toUpperCase.contains("TOTAL"))
        val isTotalFeatureBin = FeatureBinning.getBinFeature(isTotalCount, line.cellValues.length)
        (line, "IsTotal", isTotalFeatureBin)
      })

      var isTotalSimFeature = Array((sortedLines(0), "IsTotalSim", "-"))
      val isTotalSimFeatureHinter = sortedLines.sliding(2).map(linePair => {
        val line = linePair(1).cellValues.zip(linePair(0).cellValues)
        val bothTotalCount = line.count(pair => pair._1.toUpperCase.contains("TOTAL") && pair._2.toUpperCase.contains("TOTAL"))
        val bothTotalFeatureBin = FeatureBinning.getBinFeature(bothTotalCount, line.length)
        (linePair(1), "IsTotalSim", bothTotalFeatureBin)
      }).toArray
      isTotalSimFeature = isTotalSimFeature ++ isTotalSimFeatureHinter

      var isTotalNotSimFeature = Array((sortedLines(0), "IsTotalNotSim", "-"))
      val isTotalNotSimFeatureHinter = sortedLines.sliding(2).map(linePair => {
        val line = linePair(1).cellValues.zip(linePair(0).cellValues)
        val inverseBothTotalCount = line.count(pair => pair._1.toUpperCase.contains("TOTAL") && !pair._2.toUpperCase.contains("TOTAL"))
        val inverseBothTotalFeatureBin = FeatureBinning.getBinFeature(inverseBothTotalCount, line.length)
        (linePair(1), "IsTotalNotSim", inverseBothTotalFeatureBin)
      }).toArray
      isTotalNotSimFeature = isTotalNotSimFeature ++ isTotalNotSimFeatureHinter

      val result = isEmptyFeature
              .union(isTextFeature)
              .union(isNumericFeature)
              .union(isDateFeature)
              .union(isShortTextFeature)
              .union(isLongTextFeature)
              .union(isTotal)
              .union(isEmptySimFeature).union(isEmptyNotSimFeature)
              .union(isTextSimFeature).union(isTextNotSimFeature)
              .union(isNumericSimFeature).union(isNumericNotSimFeature)
              .union(isDateSimFeature).union(isDateNotSimFeature)
              .union(isShortTextSimFeature).union(isShortTextNotSimFeature)
              .union(isLongTextSimFeature).union(isLongTextNotSimFeature)
              .union(isTotalSimFeature).union(isTotalNotSimFeature)
      result
    })
    result.toArray
  }

  def isText(cell: String): Boolean = {
    (!cell.equals("") && (cell forall Character.isLetter))
  }

  def isNumeric(cell: String): Boolean = {
    (!cell.equals("") && (cell forall Character.isDigit))
  }

  def isLongText(cell: String, meanValueLength: Double, valueLengthStandardDeviation: Double): Boolean = {
    cell.length.toDouble > (meanValueLength + valueLengthStandardDeviation)
  }

  def isShortText(cell: String, meanValueLength: Double, valueLengthStandardDeviation: Double): Boolean = {
    cell.length.toDouble < (meanValueLength - valueLengthStandardDeviation)
  }
}

object FeatureBinningBaseline {

  def main(args: Array[String]): Unit = {
    //    val inputDataFolderPath = getClass.getResource("/excel-csv-files").toURI.getPath
    //    val labelFilePath = getClass.getClassLoader.getResource("annotation_result.json").toURI.getPath

//    val inputDataFolderPath = getClass.getResource("/final_csv").toURI.getPath
//    val labelFilePath = getClass.getClassLoader.getResource("metadata.csv").toURI.getPath

    val inputDataFolderPath = getClass.getResource("/data340").toURI.getPath
    val labelFilePath = getClass.getClassLoader.getResource("annotation_result.csv").toURI.getPath

//    val dataset_name = "deex"

//    val inputDataFolderPath = getClass.getResource("/new_annotations/" + dataset_name).toURI.getPath
//    val labelFilePath = getClass.getResource("/new_annotations/annotations_" + dataset_name + ".csv").toURI.getPath

    val app = new RunAlgorithmApp(inputDataFolderPath, labelFilePath)
    val lines = app.createLines()
    val baseline = new FeatureBinningBaseline
    val features = baseline.createFeatureVector(lines)

    val groups = features.groupBy(feature => (feature._1.lineMetadata.excelFileName, feature._1.lineMetadata.spreadsheetName))
    val training = groups.map(entry => entry._2.sortBy(pair => (pair._1.lineMetadata.lineNumber, pair._2))).toArray
    val headers = features.groupBy(_._1).head._2.sortBy(_._2).map(_._2)

    val file = new File("training_FeatureBin.csv")
    val bufferedWriter = new BufferedWriter(new FileWriter(file))
    bufferedWriter.write(headers.mkString(",") + ",ExcelName,SheetName,LineNumber,LineType")
    bufferedWriter.newLine()
    training.foreach(array => {
      val groupsByLineNumber = array.groupBy(_._1).toArray.sortBy(_._1.lineMetadata.lineNumber)
      groupsByLineNumber.foreach(groupByLineNumber => {
        val excelName = groupByLineNumber._1.lineMetadata.excelFileName
        val sheetName = groupByLineNumber._1.lineMetadata.spreadsheetName
        val lineNumber = groupByLineNumber._1.lineMetadata.lineNumber
        val lineType = groupByLineNumber._1.lineMetadata.lineType
        bufferedWriter.write(groupByLineNumber._2.map(pair => "\"" + pair._3 + "\"").mkString(",") + ",\"" + excelName + "\",\"" + sheetName + "\",\"" + lineNumber + "\",\"" + lineType + "\"")
        bufferedWriter.newLine()
      })
    })
    bufferedWriter.close()
  }
}

