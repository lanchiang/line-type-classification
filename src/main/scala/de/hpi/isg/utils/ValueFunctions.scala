package de.hpi.isg.utils

import de.hpi.isg.utils.ValueFunctions.DataType.DataType

/**
 * @author Lan Jiang
 * @since 11/12/19
 */
object ValueFunctions {

  def detectDataType(value: String): DataType = {
    ???
  }

  object DataType extends Enumeration {
    type DataType = Value
    val String, Int, Float = Value
  }
}
