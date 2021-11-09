package de.hpi.isg.baseline

/**
 * Calculate the logarithmic binning feature. [[numAffectedCells]] is the number of cells in a row that exhibits
 * a feature, while [[numCells]] is the number of cells in the row.
 *
 * @author Lan Jiang
 * @since 12/9/19
 */
object FeatureBinning {

  /**
   * Calculate the logarithmic binning feature from [[numAffectedCells]] and [[numCells]]
   *
   * @param numAffectedCells the number of cells in a row that exhibits a feature
   * @param numCells the number of cells in the row
   * @return a tuple pair (a, b).
   */
  def getBinFeature(numAffectedCells: Int, numCells: Int): String = {
    val a = if (numAffectedCells == 0) {
      "0"
    } else if (numAffectedCells == numCells) {
      "0-"
    } else if (numAffectedCells > 0 && numAffectedCells <= numCells / 2) {
      (Math.log(numAffectedCells.toDouble) / Math.log(2.0) + 1).toInt.toString
    } else if (numAffectedCells > numCells / 2 && numAffectedCells < numCells) {
      (Math.log((numCells - numAffectedCells).toDouble) / Math.log(2.0) + 1).toInt + "-"
    } else {
      throw new RuntimeException("The arithmetic relation between numAffectedCells and numCells is undefined!")
    }
    val b = (Math.log(numCells.toDouble) / Math.log(2.0)).toInt.toString
    "(" + a + "," + b + ")"
  }

  def main(args: Array[String]): Unit = {
    println(getBinFeature(2, 4))
    println(getBinFeature(2, 5))
    println(getBinFeature(2, 6))
    println(getBinFeature(2, 7))
    println(getBinFeature(3, 4))
    println(getBinFeature(3, 5))
    println(getBinFeature(3, 6))
    println(getBinFeature(3, 7))
    println(getBinFeature(1, 4))
    println(getBinFeature(1, 5))
    println(getBinFeature(1, 6))
    println(getBinFeature(1, 7))
  }
}
