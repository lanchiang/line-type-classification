package de.hpi.isg.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;

import java.util.Collection;

/**
 * @author Lan Jiang
 * @since 9/18/19
 */
public class SpreadSheetPojo {

    @JsonProperty("spreadsheet_name")
    private String spreadsheetName;

    @JsonProperty("excel_file_name")
    private String excelFileName;

    @JsonProperty("time_expense")
    private long timeExpense;

    @JsonProperty("is_multitable_file")
    private String isMultitableFile;

    @JsonProperty("number_of_lines")
    private int numLines;

    @JsonProperty("annotations")
    @JacksonXmlElementWrapper(useWrapping = false)
    private Collection<AnnotationPojo> annotationPojos;

    public String getSpreadsheetName() {
        return spreadsheetName;
    }

    public void setSpreadsheetName(String spreadsheetName) {
        this.spreadsheetName = spreadsheetName;
    }

    public String getExcelFileName() {
        return excelFileName;
    }

    public void setExcelFileName(String excelFileName) {
        this.excelFileName = excelFileName;
    }

    public long getTimeExpense() {
        return timeExpense;
    }

    public void setTimeExpense(long timeExpense) {
        this.timeExpense = timeExpense;
    }

    public String getIsMultitableFile() {
        return isMultitableFile;
    }

    public void setIsMultitableFile(String isMultitableFile) {
        this.isMultitableFile = isMultitableFile;
    }

    public int getNumLines() {
        return numLines;
    }

    public void setNumLines(int numLines) {
        this.numLines = numLines;
    }

    public Collection<AnnotationPojo> getAnnotationPojos() {
        return annotationPojos;
    }

    public void setAnnotationPojos(Collection<AnnotationPojo> annotationPojos) {
        this.annotationPojos = annotationPojos;
    }
}
