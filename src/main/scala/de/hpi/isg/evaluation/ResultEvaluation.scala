package de.hpi.isg.evaluation

import de.hpi.isg.elements.LineMetadata
import de.hpi.isg.pojo.SpreadSheetPojo

import scala.collection.JavaConverters._

/**
 * @author Lan Jiang
 * @since 2019-07-15
 */
object ResultEvaluation {

  def loadGroundTruth(filePath: String): Array[LineMetadata] = {
    val jsonReader = new JsonReader
    val resultPojo = jsonReader.read(filePath)

    val spreadSheetPojos = resultPojo.getSpreadSheetPojos

    val result = spreadSheetPojos.asScala.toStream
            .filter(sheetPojo => sheetPojo.getIsMultitableFile.equals("false"))
            .flatMap(spreadSheetPojo =>
              Stream.continually(spreadSheetPojo.getExcelFileName)
                      .zip(Stream.continually(spreadSheetPojo.getSpreadsheetName))
                      .zip(getBoundaryGroundtruth(spreadSheetPojo))
            )
            .map(flattened => LineMetadata(flattened._1._1, flattened._1._2, flattened._2._1, flattened._2._2))
            .toArray
    result
  }

  def getBoundaryGroundtruth(spreadSheetPojo: SpreadSheetPojo): Array[(Int, String)] = {
    val annotations = spreadSheetPojo.getAnnotationPojos
    val result = annotations.asScala.toStream.flatMap(annotation => {
      val range = (annotation.getStartLineNumber to annotation.getEndLineNumber).toArray
      val typeRepetition = List.fill(range.length)(annotation.getLineType)
      range.zip(typeRepetition)
    }).toArray
    result
  }

  def main(args: Array[String]): Unit = {
    val groundTruth = ResultEvaluation.loadGroundTruth("/Users/Fuga/Documents/hpi/code/labeling-excel-gui/annotation_result.json")
    println(groundTruth.length)
  }
}
