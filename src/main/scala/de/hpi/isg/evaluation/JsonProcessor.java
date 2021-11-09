package de.hpi.isg.evaluation;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.hpi.isg.pojo.AnnotationPojo;
import de.hpi.isg.pojo.ResultPojo;

import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Comparator;
import java.util.Optional;

/**
 * @author Lan Jiang
 * @since 10/8/19
 */
public class JsonProcessor {

    public static void main(String[] args) throws IOException {
        JsonReader jsonReader = new JsonReader();
        ResultPojo resultPojo = jsonReader.read("/Users/Fuga/Documents/hpi/code/labeling-excel-gui/annotation_result.json");

        resultPojo.getSpreadSheetPojos().forEach(spreadSheetPojo -> {
            Optional<AnnotationPojo> optionalAnnotationPojo = spreadSheetPojo.getAnnotationPojos().stream().max(Comparator.comparingInt(AnnotationPojo::getEndLineNumber));
            if (!optionalAnnotationPojo.isPresent()) {
                return;
            }
            int numLine = optionalAnnotationPojo.get().getEndLineNumber();
            spreadSheetPojo.setNumLines(numLine);
            return;
        });

        String outputPath = "./annotation_result.json";
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.writeValue(new FileOutputStream(outputPath), resultPojo);
    }
}
