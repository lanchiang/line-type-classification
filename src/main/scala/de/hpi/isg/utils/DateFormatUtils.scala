package de.hpi.isg.utils

import org.joda.time.format.DateTimeFormat

import scala.util.{Failure, Success, Try}

/**
 * @author Lan Jiang
 * @since 11/13/19
 */
object DateFormatUtils {
  private val DATE_PATTERNS = Array("dd/MM/yyyy", "yyyy/MM/dd", "dd.MM.yyyy", "yyyy.MM.dd", "dd-MM-yyyy", "yyyy-MM-dd")

  def isDate(value: String): Boolean = {
    val result = DateFormatUtils.DATE_PATTERNS.map(pattern => {
      val fmt = DateTimeFormat forPattern(pattern)
      Try(fmt parseDateTime(value)) match {
        case Success(_) => true
        case Failure(_) => false
      }
    })
    result.contains(true)
  }
}

