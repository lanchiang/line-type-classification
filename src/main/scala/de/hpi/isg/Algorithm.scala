package de.hpi.isg

import de.hpi.isg.elements.Line
import de.hpi.isg.utils.ValueFunctions
import smile.classification.randomForest
import smile.validation.loocv
import smile._

/**
 * The line type classification algorithm.
 *
 * @author Lan Jiang
 * @since 11/12/19
 */
class Algorithm(val features: Array[(Line, String, Double)]) {

  // run the algorithm, get the result, and send the result to evaluation processor
  def run(): Unit = {
    classification()
  }

  private def classification(): Unit = {
    val groups = features.groupBy(_._1) // group the feature scores by line.
    val training = groups.map(entry => entry._2.sortBy(pair => pair._2)).map(feature => feature.map(pair => pair._3)).toArray
    val labels = features.groupBy(_._1).keys.toArray.sorted.map(line => ValueFunctions.labelToIndex(line.lineMetadata.lineType))

    val classifier = randomForest(training, labels)
  }
}

object Algorithm {
}


