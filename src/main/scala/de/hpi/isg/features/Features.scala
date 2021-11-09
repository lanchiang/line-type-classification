package de.hpi.isg.features

import de.hpi.isg.elements.Line

/**
 * @author Lan Jiang
 * @since 12/6/19
 */
abstract class Features {

  def createFeatureVector(inputData: Array[Line]): Array[(Line, String, Double)]
}
