package de.hpi.isg.elements

/**
 * A [[Line]] instance represents the information of a line in a data file. The information includes 1) the value
 * of each cell; 2) the line metadata, such as line number, and the line type.
 *
 * @author Lan Jiang
 * @since 11/12/19
 */
case class Line(cellValues: Array[String], lineMetadata: LineMetadata) extends Ordered[Line] {
  import scala.math.Ordered.orderingToOrdered

  override def compare(that: Line): Int = {
    (this.lineMetadata.excelFileName, this.lineMetadata.spreadsheetName, this.lineMetadata.lineNumber) compare
            (that.lineMetadata.excelFileName, that.lineMetadata.spreadsheetName, that.lineMetadata.lineNumber)
  }
}
