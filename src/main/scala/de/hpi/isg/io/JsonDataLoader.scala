package de.hpi.isg.io

import de.hpi.isg.elements.Line
import de.hpi.isg.evaluation.ResultEvaluation

/**
 * @author Lan Jiang
 * @since 11/21/19
 */
class JsonDataLoader(override val inputDataFolderPath: String, override val labelFilePath: String)
        extends DataLoader(inputDataFolderPath, labelFilePath) {

  override def loadData(): Array[Line] = {
    val dataReader = new DataReader(inputDataFolderPath)
    val inputData = dataReader.read() // input data
    val labels = ResultEvaluation.loadGroundTruth(labelFilePath) // labels of the input data

    val lines = super.constructLines(inputData, labels)
    lines
  }
}
