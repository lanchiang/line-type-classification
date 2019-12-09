package de.hpi.isg.utils

/**
  * Functions for conducting various mathematics calculations.
  *
  * @author Lan Jiang
  * @since 2019-05-20
  */
object MathFunctions {

  /**
    * Calculate the variance of a sequence of data.
    * @param seq the data sequence
    * @tparam T data type of the sequence, must be numerical
    * @return the variance
    */
  def variance[T](seq: Seq[T]): Float = {
    val floatSeq = seq.map(value => value.toString.toFloat)

    val avg = floatSeq.sum / floatSeq.size.toFloat
    val variance = floatSeq.map(a => math.pow(a - avg, 2)).sum.toFloat / floatSeq.size.toFloat
    variance
  }

  /**
    * Calculate the standard deviation of a sequence of data.
    * @param seq the data sequence
    * @tparam T data type of the sequence, must be numerical
    * @return the variance
    */
  def std[T](seq: Seq[T]): Float = {
    Math.sqrt(variance(seq)).toFloat
  }

  def average[T](seq: Seq[T]): Float = {
    val floatSeq = seq.map(value => value.toString.toFloat)
    floatSeq.sum / floatSeq.size.toFloat
  }

  def coeffientOfVariation(seq: Seq[Double]): Double = {
    std(seq) / average(seq)
  }

  /**
    * Return the information entropy of the input data sequence.
    *
    * @param seq the input data sequence.
    * @tparam T
    * @return the information entropy
    */
  def entropy[T](seq: Seq[T]): Float = {
    val grouped = seq.groupBy(value => value)
    val group_amount = grouped.values.map(valueSet => valueSet.length.toFloat)
    val frequencies = group_amount.map(amount => amount / seq.length.toFloat)
    val entropy = frequencies.map(frequency => - (frequency * Math.log(frequency) / Math.log(2))).sum.toFloat
    entropy
  }

  /**
    * Return the normalized information entropy of the input data sequence. If there are only one value, return 0.
    *
    * @param seq the input data sequence.
    * @tparam T
    * @return the information entropy
    */
  def normalizedEntropy[T](seq: Seq[T]): Float = {
    val grouped = seq.groupBy(value => value)
    val frequencies = grouped.values.map(valueSet => valueSet.length.toFloat / seq.length.toFloat)
    val entropy = grouped.size match {
      case 1 => 0f
      case _ => frequencies.map(frequency => - (frequency * Math.log(frequency) / Math.log(grouped.size))).sum.toFloat
    }
    entropy
  }

  /**
   * Calculate the discounted cumulative gain of the list of the values. the graded relevance equals to 1
   * if the cell is empty, 0 if not.
   * @param seq
   * @return
   */
  def normalizedDiscountedCumulativeGain(seq: Array[Double]): Double = {
    val dcg = discountedCumulativeGain(seq)
    val idealSeq = seq.sorted(Ordering.Double.reverse)
    val idcg = discountedCumulativeGain(idealSeq)
    if (idcg == 0) {
      0d
    } else {
      dcg / idcg
    }
  }

  def discountedCumulativeGain(seq: Array[Double]): Double = {
    val indexedSeq = seq.zipWithIndex.map(pair => (pair._1, pair._2 + 1))
    indexedSeq.map(pair => {
      if (pair._2 == 1) {
        pair._1
      } else {
        val partialDCG = pair._1 / (Math.log(pair._2.toDouble) / Math.log(2.0))
        partialDCG
      }
    }).sum
  }
}
