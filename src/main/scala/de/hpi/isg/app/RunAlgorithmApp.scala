package de.hpi.isg.app

import java.io.{BufferedWriter, File, FileWriter}

import de.hpi.isg.elements.Line
import de.hpi.isg.evaluation.ResultEvaluation
import de.hpi.isg.features.LineClassificationFeatures
import de.hpi.isg.io.{DataLoader, DataReader, JsonDataLoader, LineWiseDataLoader}

/**
 * @author Lan Jiang
 * @since 11/12/19
 */
class RunAlgorithmApp(val inputDataFolderPath: String, val labelFilePath: String, val delimiter: Char = ',', val quote: Char = '\"') {

  def run(): Unit = {
//        val dataLoader: DataLoader = new JsonDataLoader(inputDataFolderPath, labelFilePath)
    val dataLoader: DataLoader = new LineWiseDataLoader(inputDataFolderPath, labelFilePath)
    val lines = preprocessing(dataLoader.loadData())

    val f = new LineClassificationFeatures

    val features = f.createFeatureVector(lines)

    val groups = features.groupBy(_._1) // group the feature scores by line.
    val training = groups.map(entry => entry._2.sortBy(pair => pair._2)).map(feature => feature.map(pair => pair._3)).toArray
    val headers = groups.head._2.sortBy(_._2).map(_._2)
    val labels = groups.keys.toArray.map(line => line.lineMetadata.lineType)
    val lineMetadata = groups.keys.toArray.map(line => (line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName, line.lineMetadata.lineNumber, line.lineMetadata.lineType))
    val result = training.zip(labels)

    val file = new File("training.csv")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(headers.mkString(",") + "," + "Label")
    bw.newLine()
    result.foreach(pair => {
      bw.write(pair._1.mkString(",") + "," + pair._2)
      bw.newLine()
    })
    bw.close()

    val metadataFile = new File("metadata.csv")
    val bwMetadata = new BufferedWriter(new FileWriter(metadataFile))
    bwMetadata.write("Excel file name,Spreadsheet name,Line number,Line type\n")
    lineMetadata.foreach(pair => {
      bwMetadata.write("\"" + pair._1 + "\",\"" + pair._2 + "\",\"" + pair._3 + "\",\"" + pair._4 + "\"\n")
    })
    bwMetadata.close()
  }

  def preprocessing(lines: Array[Line]): Array[Line] = {
    val linesBySheet = lines.groupBy(line => (line.lineMetadata.excelFileName, line.lineMetadata.spreadsheetName))

    // remove the files that contain no data lines.
    val result = linesBySheet.values
            .filter(lines => {
              if (lines.map(line => line.lineMetadata.lineType).count(lineType => lineType.equals("Data (D)")) == 0) {
                false
              } else true
            })
            .flatten.toArray
    result
  }
}

object RunAlgorithmApp {

  def main(args: Array[String]): Unit = {
//        val inputDataFolderPath = getClass.getResource("/excel-csv-files").toURI.getPath
//        val labelFilePath = getClass.getClassLoader.getResource("annotation_result.json").toURI.getPath

    val inputDataFolderPath = getClass.getResource("/selection240").toURI.getPath
    val labelFilePath = getClass.getClassLoader.getResource("annotation_result_line.csv").toURI.getPath
    val app = new RunAlgorithmApp(inputDataFolderPath, labelFilePath)
    app.run()
  }
}


