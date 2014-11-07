package salo_dm;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class kmedoids {
	public final static int N = 517;
	public final static int cluster_quantity = 10;
	public final static int objects_quantity = 12;
	public static int[] ground_truth;
	public static int[] clusters;
	public static double[][] data;
	public static double distMatrix[][];
	static int[] medoids;
	static byte[][] cluster_matrix;
	static byte[][] ground_truth_matrix;
	static Map<String, Double> index;

	public static void main(String[] args) throws FileNotFoundException {
		initVariables();
		readData();
		Instant startTime = Instant.now();
		fillDistMatrix();

		p(getCost());
		cluster();
		p(getCost());

		fillClusterIds();
		index = RandJaccardIndex();
		p(index);

		Instant endTime = Instant.now();
		p(Duration.between(startTime, endTime));
		printReportToFile();
	}

	static Map<String, Double> RandJaccardIndex() {
		cluster_matrix = new byte[N][N];
		ground_truth_matrix = new byte[N][N];
		for (int i = 0; i < N; i++)
			for (int j = 0; j < N; j++) {
				if (clusters[i] == clusters[j])
					cluster_matrix[i][j] = 1;
				else
					cluster_matrix[i][j] = 0;
				if (ground_truth[i] == ground_truth[j])
					ground_truth_matrix[i][j] = 1;
				else
					ground_truth_matrix[i][j] = 0;
			}
		int SS = 0;
		int DD = 0;
		int SD = 0;
		int DS = 0;
		for (int i = 0; i < N; i++)
			for (int j = 0; j < N; j++) {
				if (cluster_matrix[i][j] == 1 && ground_truth_matrix[i][j] == 1)
					SS++;
				if (cluster_matrix[i][j] == 0 && ground_truth_matrix[i][j] == 0)
					DD++;
				if (cluster_matrix[i][j] == 1 && ground_truth_matrix[i][j] == 0)
					SD++;
				if (cluster_matrix[i][j] == 0 && ground_truth_matrix[i][j] == 1)
					DS++;
			}
		double Rand = 1.0 * (SS + DD) / (SS + DD + SD + DS);
		double JaccardCoef = 1.0 * (SS) / (SS + SD + DS);
		index = new HashMap<String, Double>();
		index.put("Rand", Rand);
		index.put("Jaccard", JaccardCoef);
		return index;
	}

	static void cluster() {
		double cost = getCost();
		for (int i = 0; i < N; i++) {
			int pos = getNearestMedoid(i);
			// try swap
			int old_medoid = medoids[pos];
			medoids[pos] = i;
			updateDistMatrix(pos, i);
			double costSwap = getCost();
			if (costSwap > cost) { // revert changes
				medoids[pos] = old_medoid;
				updateDistMatrix(pos, old_medoid);
			} else{
				// update cost
				cost = costSwap;
				//p(cost);
			}
		}
	}
	
	static void clusterRand() {
		double cost = getCost();
		for (int i = 0; i < 1000 * N; i++) {
			int candidate = (int)(Math.random() * N);
			int pos = getNearestMedoid(candidate);
			// try swap
			int old_medoid = medoids[pos];
			medoids[pos] = candidate;
			updateDistMatrix(pos, candidate);
			double costSwap = getCost();
			if (costSwap > cost) { // revert changes
				medoids[pos] = old_medoid;
				updateDistMatrix(pos, old_medoid);
			} else{
				// update cost
				cost = costSwap;
				//p(cost);
			}
		}
	}

	static void fillClusterIds() {
		for (int i = 0; i < N; i++)
			clusters[i] = getNearestMedoid(i);
	}

	static void updateDistMatrix(int pos, int value) {
		for (int i = 0; i < N; i++)
			distMatrix[i][pos] = getDist(i, value);
	}

	static int getNearestMedoid(int point) {
		int posmin = 0;
		double min = distMatrix[point][posmin];
		for (int j = 1; j < cluster_quantity; j++)
			if (distMatrix[point][j] < min) {
				min = distMatrix[point][j];
				posmin = j;
			}
		return (posmin);
	}

	static double getCost() {
		double cost = 0;
		double[] sorted_ith;
		for (int i = 0; i < N; i++) {
			sorted_ith = distMatrix[i].clone();
			Arrays.sort(sorted_ith);
			cost += sorted_ith[0];
		}
		return (cost);
	}

	static void fillDistMatrix() {
		for (int i = 0; i < N; i++)
			for (int j = 0; j < cluster_quantity; j++)
				distMatrix[i][j] = getDist(i, medoids[j]);
	}

	static double getDist(int a, int b) {
		double dist = 0;
		for (int j = 0; j < objects_quantity; j++)
			dist += Math.abs(data[a][j] - data[b][j]);
		return (dist);
	}

	static void initVariables() {
		clusters = new int[N];
		ground_truth = new int[N];
		data = new double[N][objects_quantity];
		distMatrix = new double[N][cluster_quantity];
		medoids = new int[] { 49, 99, 149, 199, 249, 299, 349, 399, 449, 499 };
	}

	static void readData() throws FileNotFoundException {
		Scanner sc = new Scanner(new BufferedReader(new FileReader(
				"assignment3_data.txt")));
		for (int i = 0; i < N; i++) {
			sc.next();
			ground_truth[i] = sc.nextInt();
			for (int j = 0; j < objects_quantity; j++)
				data[i][j] = sc.nextDouble();
		}
		sc.close();
	}

	static void printData(double[][] mat) {
		for (int i = 0; i < mat.length; i++) {
			for (int j = 0; j < mat[0].length; j++) {
				System.out.print(mat[i][j]);
				System.out.print(", ");
			}
			System.out.println();
		}
	}

	static void p(Object o) {
		System.out.println(o);
	}
	
	static void printReportToFile(){
		PrintWriter writer = null;
		try {
			writer = new PrintWriter("salo_assignment3.txt", "UTF-8");
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		//data = new double[N][objects_quantity]
		for (int i = 0; i < N; i++){
			writer.print(i + 1 + " ");
			writer.print(ground_truth[i] + " ");
			writer.print(clusters[i]);
			writer.println();
		}
		writer.close();
	}
}