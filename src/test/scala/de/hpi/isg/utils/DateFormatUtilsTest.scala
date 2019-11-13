package de.hpi.isg.utils

import org.scalatest.{BeforeAndAfterEach, FlatSpecLike, Matchers}

/**
 * @author Lan Jiang
 * @since 11/13/19
 */
class DateFormatUtilsTest extends FlatSpecLike with Matchers with BeforeAndAfterEach {

  "Whether a string is a date" should "be correctly recognized" in {
    val dateFormatUtils = new DateFormatUtils
    var result = dateFormatUtils.toDate("12-13-1989")
    result shouldEqual false

    result = dateFormatUtils.toDate("13.12.2008")
    result shouldEqual true

    result = dateFormatUtils.toDate("2019/1/13")
    result shouldEqual true

    result = dateFormatUtils.toDate("12d-13-1989")
    result shouldEqual false

    result = dateFormatUtils.toDate("13.13.1989")
    result shouldEqual false

    result = dateFormatUtils.toDate("2019/1a/f3")
    result shouldEqual false

    result = dateFormatUtils.toDate("1934/3/10")
    result shouldEqual true

    result = dateFormatUtils.toDate("1934/3-10")
    result shouldEqual false
  }

}
