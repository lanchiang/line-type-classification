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
}
