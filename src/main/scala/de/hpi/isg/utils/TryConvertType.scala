package de.hpi.isg.utils

import scala.util.Try

/**
 * @author Lan Jiang
 * @since 11/13/19
 */
object TryConvertType {

  def main(args: Array[String]): Unit = {
    var value = "1.3"
    println(Try(value.toDouble).isSuccess)
    println(Try(value.toInt).isSuccess)

    value = "1.d3"
    println(Try(value.toDouble).isSuccess)
    println(Try(value.toInt).isSuccess)


  }
}
