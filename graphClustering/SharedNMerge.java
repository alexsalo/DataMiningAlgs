package salo_dm;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Scanner;

public class SharedNMerge {
	private static final String FILENAME = "assignment4_data.txt";
	private static ArrayList<String> inputStrings;
	private static final double TRESHOLD = 0.3;
	private static HashMap<String, ArrayList<String>> graph;
	private static HashMap<Integer, HashSet<String>> clusters = new HashMap<Integer, HashSet<String>>();
	private static ArrayList<HashSet<String>> final_clusters = new ArrayList<HashSet<String>>();
	private static double[][] distanceMatix;
	private static ArrayList<String> names;
	private static ArrayList<String> removedNames = new ArrayList<String>();

	public static void main(String[] args) {
		Instant startTime = Instant.now();

		readFile();
		initGraph();
		initClusters();
		initDistanceMatrix();
		merge();
		System.out.println(final_clusters);
		// findDisconnectedSubgraphs();
		printReportToFile();

		Instant endTime = Instant.now();
		System.out.println(Duration.between(startTime, endTime));
	}

	static boolean merge() {
		String a = null;
		String b = null;
		int ai = 0;
		int bj = 0;
		do {
			int[] result = findNextMostSimilar();
			a = names.get(result[0]);
			b = names.get(result[1]);
			ai = findCluster(a);
			bj = findCluster(b);
			// merge
			System.out.println(clusters.size());
			HashSet<String> aCluster = new HashSet<String>(clusters.get(ai));
			HashSet<String> bCluster = new HashSet<String>(clusters.get(bj));
			clusters.get(ai).addAll(bCluster);
			clusters.remove(bj);
			removedNames.addAll(bCluster);
			if (density(clusters.get(ai)) < TRESHOLD) {
				final_clusters.add(aCluster);
				final_clusters.add(bCluster);
				clusters.remove(ai);
				removedNames.addAll(aCluster);
				System.out.print("clustered: " + a + " -> " + b + " size: ");
				System.out.println(aCluster.size() + bCluster.size());
			} else
				System.out.println("removed: " + a + " -> " + b);
		} while (clusters.size() > 1);

		return true;
	}

	static void initClusters() {
		int i = 0;
		for (String key : graph.keySet())
			clusters.put(i++, new HashSet<String>(Arrays.asList(key)));
	}

	static double jaccard(String s1, String s2) {
		ArrayList<String> intersection = new ArrayList<String>(graph.get(s1));
		ArrayList<String> union = new ArrayList<String>(intersection);
		intersection.removeAll(graph.get(s2));
		union.addAll(graph.get(s2));
		return (double) intersection.size() / union.size();
	}

	static int union(String s1, String s2) {
		ArrayList<String> union = new ArrayList<String>(graph.get(s1));
		union.addAll(graph.get(s2));
		return union.size();
	}

	static double density(HashSet<String> subgraph) {
		double E = 0;
		for (String s : subgraph)
			E += graph.get(s).size();
		int V = subgraph.size();
		return E / (V * (V - 1)); // edges already counted twice
	}

	private static void readFile() {
		Scanner sc;
		try {
			sc = new Scanner(new BufferedReader(new FileReader(FILENAME)));
			inputStrings = new ArrayList<String>();
			while (sc.hasNext())
				inputStrings.add(sc.next() + " " + sc.next());
			sc.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}

	private static void initGraph() {
		graph = new HashMap<String, ArrayList<String>>();
		Scanner sc = null;
		for (String s : inputStrings) {
			sc = new Scanner(s);
			String s1 = sc.next();
			String s2 = sc.next();
			// one way
			if (graph.containsKey(s1)) {
				graph.get(s1).add(s2);
			} else {
				ArrayList<String> links = new ArrayList<String>();
				links.add(s2);
				graph.put(s1, links);
			}
			// opposite way
			if (graph.containsKey(s2)) {
				graph.get(s2).add(s1);
			} else {
				ArrayList<String> links = new ArrayList<String>();
				links.add(s1);
				graph.put(s2, links);
			}
		}
		sc.close();
	}

	static void initDistanceMatrix() {
		names = new ArrayList<String>(graph.keySet());
		int N = names.size();
		distanceMatix = new double[N][N];
		for (int i = 0; i < N - 1; i++)
			for (int j = i + 1; j < N; j++)
				distanceMatix[i][j] = jaccard(names.get(i), names.get(j));
		System.out.println("matrix initiated");
	}

	static int[] findNextMostSimilar() {
		double max = 0;
		int imax = 0;
		int jmax = 0;
		ArrayList<Integer> indicies = getIndices();
		for (int i : indicies)
			for (int j : indicies)
				if (j > i && distanceMatix[i][j] > max
						&& !isTheSameCluster(names.get(i), names.get(j))) {
					max = distanceMatix[i][j];
					imax = i;
					jmax = j;
				}
		return new int[] { imax, jmax };
	}

	static ArrayList<Integer> getIndices() {
		ArrayList<Integer> list = new ArrayList<Integer>();
		for (int i = 0; i < names.size(); i++)
			if (!removedNames.contains(names.get(i)))
				list.add(i);
		return list;
	}

	static boolean isTheSameCluster(String s1, String s2) {
		for (int c : clusters.keySet())
			if (clusters.get(c).size() > 1)
				if (clusters.get(c).contains(s1)
						&& clusters.get(c).contains(s2))
					return true;
		return false;
	}

	static int findCluster(String s) {
		for (int c : clusters.keySet())
			if (clusters.get(c).contains(s))
				return c;
		return -1;
	}

	static void SortClusters() {
		// delete all that less than 3
		Iterator<HashSet<String>> itr = final_clusters.iterator();
		while (itr.hasNext()) {
			HashSet<String> i = itr.next();
			if (i.size() < 3) {
				itr.remove();
			}
		}

		// sort by size
		Collections.sort(final_clusters, new Comparator<HashSet<String>>() {
			public int compare(HashSet<String> a1, HashSet<String> a2) {
				return a2.size() - a1.size(); // assumes you want biggest to
												// smallest
			}
		});
	}

	static void printReportToFile() {
		SortClusters();
		PrintWriter writer = null;
		try {
			writer = new PrintWriter("salo_assignment6.txt", "UTF-8");
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		writer.print(final_clusters.size());
		for (int i = 0; i < final_clusters.size(); i++) {
			writer.print("\n");
			writer.print(final_clusters.get(i).size());
			for (String s : final_clusters.get(i))
				writer.print(" " + s);
		}
		writer.close();
	}

}
