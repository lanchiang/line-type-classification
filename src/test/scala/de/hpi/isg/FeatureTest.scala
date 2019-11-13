package de.hpi.isg

import java.io.File

import org.scalatest.{BeforeAndAfterEach, FlatSpecLike, Matchers}

/**
 * @author Lan Jiang
 * @since 11/12/19
 */
class FeatureTest extends FlatSpecLike with Matchers with BeforeAndAfterEach {

  "Each feature" should "obtains a correct value" in {
    val filePath = getClass.getClassLoader.getResource("/resources/excel-csv-files").toURI.getPath
    val files = new File(filePath).listFiles().filter(file => !file.getName.equals(".DS_Store"))
    files.map(file => file)

    val features = new Features
  }

}
