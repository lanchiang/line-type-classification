package de.hpi.isg.evaluation;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.hpi.isg.pojo.ResultPojo;

import java.io.File;
import java.io.IOException;

/**
 * @author Lan Jiang
 * @since 10/7/19
 */
public class JsonReader {

    private ObjectMapper objectMapper = new ObjectMapper();

    public ResultPojo read(String path) throws IOException {
        return objectMapper.readValue(new File(path), ResultPojo.class);
    }

    public static void main(String[] args) throws IOException {
        JsonReader jsonReader = new JsonReader();
        ResultPojo resultPojo = jsonReader.read("/Users/Fuga/Documents/hpi/code/labeling-excel-gui/annotation_result.json");
        return;
    }
}
