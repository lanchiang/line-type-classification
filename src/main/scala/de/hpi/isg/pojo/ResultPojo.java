package de.hpi.isg.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;

import java.util.Collection;

/**
 * @author Lan Jiang
 * @since 10/7/19
 */
public class ResultPojo {

    @JsonProperty("result")
    @JacksonXmlElementWrapper(useWrapping = false)
    private Collection<SpreadSheetPojo> spreadSheetPojos;

    public Collection<SpreadSheetPojo> getSpreadSheetPojos() {
        return spreadSheetPojos;
    }

    public void setSpreadSheetPojos(Collection<SpreadSheetPojo> spreadSheetPojos) {
        this.spreadSheetPojos = spreadSheetPojos;
    }
}
