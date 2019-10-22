package de.hpi.isg.evaluation;

import java.io.*;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Map;
import java.util.TreeMap;

/**
 * @author Lan Jiang
 * @since 10/17/19
 */
public class PrecisionRecall {

    public static void main(String[] args) throws URISyntaxException, IOException {
        File[] inputFiles = new File("statistics").listFiles();

        double count = 0d;
        Map<Double, Double> precisions = new TreeMap<>();
        Map<Double, Double> recalls = new TreeMap<>();
        Arrays.stream(inputFiles)
                .filter(file -> file.isDirectory())
                .map(folder -> Arrays.stream(folder.listFiles()).filter(file -> file.getName().endsWith(".csv")).findFirst().get())
                .forEach(file -> {
                    try {
                        BufferedReader bufferedReader = new BufferedReader(new FileReader(file));
                        String line;
                        while ((line = bufferedReader.readLine())!= null) {
                            String[] strings = line.split(",");
                            if (strings[1].equals("NaN")) {
                                strings[1] = "1.0";
                            }
                            if (strings[2].equals("NaN")) {
                                bufferedReader.close();
                                return;
                            }
                            if (!precisions.containsKey(Double.parseDouble(strings[0]))) {
                                precisions.put(Double.parseDouble(strings[0]), Double.parseDouble(strings[1]));
                            } else {
                                precisions.put(Double.parseDouble(strings[0]), precisions.get(Double.parseDouble(strings[0])) + Double.parseDouble(strings[1]));
                            }
                            if (!recalls.containsKey(Double.parseDouble(strings[0]))) {
                                recalls.put(Double.parseDouble(strings[0]), Double.parseDouble(strings[2]));
                            } else {
                                recalls.put(Double.parseDouble(strings[0]), recalls.get(Double.parseDouble(strings[0])) + Double.parseDouble(strings[2]));
                            }
                        }
                        bufferedReader.close();
                    } catch (FileNotFoundException e) {
                        e.printStackTrace();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                });
        BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter("./precision.csv"));
        for (Map.Entry<Double, Double> entry : precisions.entrySet()) {
            bufferedWriter.write(entry.getKey() + "," + entry.getValue() / 94.0);
            bufferedWriter.newLine();
        }
        bufferedWriter.close();

        bufferedWriter = new BufferedWriter(new FileWriter("./recall.csv"));
        for (Map.Entry<Double, Double> entry : recalls.entrySet()) {
            bufferedWriter.write(entry.getKey() + "," + entry.getValue() / 94.0);
            bufferedWriter.newLine();
        }
        bufferedWriter.close();
    }
}
