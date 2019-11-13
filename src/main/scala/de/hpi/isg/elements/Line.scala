package de.hpi.isg.elements

/**
 * A [[Line]] instance represents the information of a line in a data file. The information includes 1) the value
 * of each cell; 2) the line metadata, such as line number, and the line type.
 *
 * @author Lan Jiang
 * @since 11/12/19
 */
class Line(val cellValues: Array[String], val lineMetadata: LineMetadata) {

}
