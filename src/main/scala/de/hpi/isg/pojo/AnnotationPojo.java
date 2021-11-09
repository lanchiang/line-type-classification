package de.hpi.isg.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Lan Jiang
 * @since 9/18/19
 */
public class AnnotationPojo {

    @JsonProperty("start_line_number")
    private int startLineNumber;

    @JsonProperty("end_line_number")
    private int endLineNumber;

    @JsonProperty("line_type")
    private String lineType;

    public int getStartLineNumber() {
        return startLineNumber;
    }

    public void setStartLineNumber(int startLineNumber) {
        this.startLineNumber = startLineNumber;
    }

    public int getEndLineNumber() {
        return endLineNumber;
    }

    public void setEndLineNumber(int endLineNumber) {
        this.endLineNumber = endLineNumber;
    }

    public String getLineType() {
        return lineType;
    }

    public void setLineType(String lineType) {
        this.lineType = lineType;
    }
}
