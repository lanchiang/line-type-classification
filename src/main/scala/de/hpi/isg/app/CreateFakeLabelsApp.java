package de.hpi.isg.app;

import com.opencsv.*;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author lan
 * @since 2021/7/16
 */
public class CreateFakeLabelsApp {

    public static void main(String[] args) throws IOException {
        String inputFolder = "/Users/lan/Documents/hpi/mazhar/structured_datasets/updated";

        File[] files = new File(inputFolder).listFiles();

        assert files != null;

        List<String[]> outputLines = new ArrayList<>();
        outputLines.add(new String[]{"FileName", "SheetName", "LineNumber", "AnnotationLabel"});
        CSVParser csvParser = new CSVParserBuilder().withQuoteChar('\"').withSeparator(',').build();

        for (File file : files) {
            if (file.getName().equals(".DS_Store")) {
                continue;
            }
//            if (!file.getName().equals("MND_007.txt")) {
//                continue;
//            }
            CSVReader csvReader = new CSVReaderBuilder(new FileReader(file)).withCSVParser(csvParser).build();

            List<String[]> lines = csvReader.readAll();
            for (int i = 0; i < lines.size(); i++) {
                outputLines.add(new String[]{file.getName(), file.getName(), String.valueOf(i+1), "None"});
            }
            csvReader.close();
        }

        ICSVWriter csvWriter = new CSVWriterBuilder(
                new FileWriter("/Users/lan/Documents/hpi/mazhar/Dataset_labels/annotations_updated.csv")
        ).withParser(csvParser).build();
        csvWriter.writeAll(outputLines);
        csvWriter.close();
    }
}
