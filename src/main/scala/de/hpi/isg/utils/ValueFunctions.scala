package de.hpi.isg.utils

import de.hpi.isg.utils.ValueFunctions.DataType.DataType

import scala.util.Try

/**
 * @author Lan Jiang
 * @since 11/12/19
 */
object ValueFunctions {

  def detectDataType(value: String): DataType = {
    if (Try(value.toDouble).isSuccess) {
      DataType.Float
    } else {
      if (Try(value.toInt).isSuccess) {
        DataType.Int
      } else {
        if (DateFormatUtils.toDate(value)) {
          DataType.Date
        } else {
          if (value.length == 0) {
            DataType.Empty
          } else {
            DataType.String
          }
        }
      }
    }
  }

  object DataType extends Enumeration {
    type DataType = Value
    val String, Int, Float, Date, Empty = Value
  }

  /**
   * Convert the string labels to the corresponding integer representation. (0 - Preamble, 1 - Header, 2 - Data, 3 - Aggregation,
   * 4 - Footnote, 5 - Group title, 6 - Empty)
   * @param label
   * @return
   */
  def labelToIndex(label: String): Int = {
    label match {
      case "Preamble (P)" => 0
      case "Header (H)" => 1
      case "Data (D)" => 2
      case "Aggregation (A)" => 3
      case "Footnote (F)" => 4
      case "Group header (G)" => 5
      case "Empty (E)" => 6
      case _ => throw new RuntimeException("The given label string cannot be converted to any integer representation.")
    }
  }
}
