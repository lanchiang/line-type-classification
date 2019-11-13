package de.hpi.isg.io

import org.scalatest.{BeforeAndAfterEach, FlatSpecLike, Matchers}

/**
 * @author Lan Jiang
 * @since 11/12/19
 */
class DataReaderTest extends FlatSpecLike with Matchers with BeforeAndAfterEach {

  "DataReader" should "put all data from the valid files to the main memory" in {
    val inputFileFolderPath = getClass.getResource("/excel-csv-files").toURI.getPath
    val dataReader = new DataReader(inputFileFolderPath)
    val result = dataReader.read()

    result.length shouldEqual(28)
  }
}
