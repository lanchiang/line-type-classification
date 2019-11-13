package de.hpi.isg.utils

import de.hpi.isg.utils.CollectionFunctions.HISTOGRAM_ALGORITHM.HISTOGRAM_ALGORITHM

/**
  * @author Lan Jiang
  * @since 2019-07-08
  */
object CollectionFunctions {

  /**
    * Calculates the histogram difference between the two given histograms represented as two double sequences.
    *
    * @param histogram1 is the first histogram
    * @param histogram2 is the second histogram
    * @param algorithm is the algorithm used to calculate the histogram difference.
    * @return the histogram difference as a double
    */
  def histogramDifference(histogram1: Seq[Double], histogram2: Seq[Double], algorithm: HISTOGRAM_ALGORITHM = HISTOGRAM_ALGORITHM.Bhattacharyya): Double = {
    val ave_hist1 = histogram1.sum / histogram1.size
    val ave_hist2 = histogram2.sum / histogram2.size

    val sum = histogram1.zip(histogram2).map {
      case (x, y) => algorithm match {
        case HISTOGRAM_ALGORITHM.Min => Math.min(x, y)
        case HISTOGRAM_ALGORITHM.Correlation => (x - ave_hist1) * (y - ave_hist2)
        case HISTOGRAM_ALGORITHM.Chi_square =>
          x match {
            case 0.0 => 0.0
            case _ => Math.pow(x - y, 2) / x
          } // asymmetric to (hist1, hist2) and (hist2, hist1)
        case HISTOGRAM_ALGORITHM.Bhattacharyya => Math.sqrt(x * y)
      }
    }.sum

    val difference = algorithm match {
      case HISTOGRAM_ALGORITHM.Bhattacharyya => {
        val bhattacharyya_factor = 1 / Math.sqrt(histogram1.sum*histogram2.sum)
        1 - bhattacharyya_factor*sum match {
          case value if value > 0 => Math.sqrt(value)
          case _ => 0.0
        }
      }
      case _ => sum
    }
    difference
  }

  object HISTOGRAM_ALGORITHM extends Enumeration {
    type HISTOGRAM_ALGORITHM = Value
    val Min, Correlation, Chi_square, Bhattacharyya = Value
  }
}
