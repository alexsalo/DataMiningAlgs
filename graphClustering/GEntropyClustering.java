package salo_dm;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Scanner;

public class GEntropyClustering {
	static final String fileName = "assignment4_data.txt";
	static ArrayList<String> inputStrings;
	static Graph graph = new Graph();
	static int NClusters;
	static ArrayList<ArrayList<String>> clusters;

	public static void main(String[] args) {
		readFile();
		initGraph();
		clust();
		printReportToFile();
	}

	static void clust() {
		Instant startTime = Instant.now();
		String seed = null;
		int clustid = 0;
		while ((seed = graph.findVmaxDeg()) != null) {
			System.out.print(clustid);
			System.out.print(" ");
			
			// STEP 1: add all links
			for (String s : graph.getNode(seed).getLinks())
				graph.clust(s, clustid);
			
			//STEP 2: remove neigbors if decrease entropy
			removeNeigborsDynamic(clustid);
			// add to the cluster the seed itself
			graph.getNode(seed).addCluster(clustid);
			
			//STEP 3: add neigbors from outer boundary if decrease entropy
			addOuterBoundaryNodesDynamic(clustid);

			System.out.print(" clust_size: ");
			System.out.print(graph.getCluster(clustid).size());
			System.out.print(" clust%: ");
			System.out.println(graph.clusteredFraction());
			clustid++;
		}
		NClusters = clustid;
		Instant endTime = Instant.now();
		System.out.println(Duration.between(startTime, endTime));
	}

	static void removeNeigbors(int clustid) {
		double gentropy = graph.gEntropy(clustid);
		for (String s : graph.getCluster(clustid)){
			graph.unclust(s, clustid);
			double newgentropy = graph.gEntropy(clustid);
			if (newgentropy >= gentropy) // revert changes
				graph.clust(s, clustid);
			else
				gentropy = newgentropy;
		}
	}

	static void addOuterBoundaryNodes(int clustid) {
		double gentropy = graph.gEntropy(clustid);
		for (String s : graph.getOuterBoundaryNodes(clustid)) {
			graph.clust(s, clustid);
			double newgentropy = graph.gEntropy(clustid);
			if (newgentropy >= gentropy) // revert changes
				graph.unclust(s, clustid);
			else 
				gentropy = newgentropy;
		}
	}

	static void readFile() {
		Scanner sc;
		try {
			sc = new Scanner(new BufferedReader(new FileReader(fileName)));
			inputStrings = new ArrayList<String>();
			while (sc.hasNext())
				inputStrings.add(sc.next() + " " + sc.next());
			sc.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}

	static void initGraph() {
		Scanner sc = null;
		for (String s : inputStrings) {
			sc = new Scanner(s);
			String s1 = sc.next();
			String s2 = sc.next();
			graph.addLink(s1, s2);
			graph.addLink(s2, s1);
		}
		sc.close();
	}

	static void SortClusters() {
		clusters = new ArrayList<ArrayList<String>>();
		for (int i = 0; i < NClusters; i++) {
			clusters.add(new ArrayList<String>(graph.getCluster(i)));
		}

		// delete all that less than 3
		Iterator<ArrayList<String>> itr = clusters.iterator();
		while (itr.hasNext()) {
			ArrayList<String> i = itr.next();
			if (i.size() < 3) {
				itr.remove();
			}
		}

		// sort by size
		Collections.sort(clusters, new Comparator<ArrayList<String>>() {
			public int compare(ArrayList<String> a1, ArrayList<String> a2) {
				return a2.size() - a1.size(); // assumes you want biggest to
												// smallest
			}
		});
	}

	static void printReportToFile() {
		SortClusters();
		PrintWriter writer = null;
		try {
			writer = new PrintWriter("salo_assignment4.txt", "UTF-8");
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		writer.print(clusters.size());
		for (int i = 0; i < clusters.size(); i++) {
			writer.print("\n");
			int sz = clusters.get(i).size();
			writer.print(sz);
			Collections.sort(clusters.get(i));
			for (int j = 0; j < sz; j++)
				writer.print(" " + clusters.get(i).get(j));			
		}
		writer.close();
	}
	
	static void removeNeigborsDynamic(int clustid) {
		double gentropy = graph.gEntropy(clustid);
		int removed = 0;
		HashSet<String> cluster = new HashSet<>(graph.getCluster(clustid));
		while (!cluster.isEmpty()){
			String s = cluster.iterator().next();
			graph.unclust(s, clustid);
			double newgentropy = graph.gEntropy(clustid);
			if (newgentropy >= gentropy) // revert changes
				graph.clust(s, clustid);
			else {
				gentropy = newgentropy;
				removed ++;	
				cluster.addAll(graph.getCluster(clustid));
			}
			cluster.remove(s);
		}
		System.out.print(" removed: ");
		System.out.print(removed);
		System.out.print(" ");
	}

	static void addOuterBoundaryNodesDynamic(int clustid) {
		double gentropy = graph.gEntropy(clustid);
		HashSet<String> boundary = graph.getOuterBoundaryNodes(clustid);
		int added = 0;
		while (!boundary.isEmpty()) {
			String s = boundary.iterator().next();
			graph.clust(s, clustid);
			double newgentropy = graph.gEntropy(clustid);
			if (newgentropy >= gentropy) // revert changes
				graph.unclust(s, clustid);
			else {
				gentropy = newgentropy;
				boundary.addAll(graph.getNode(s).getLinks());
				boundary.removeAll(graph.getCluster(clustid));
				added++;
			}
			boundary.remove(s);
		}
		System.out.print(" added: ");
		System.out.print(added);
		System.out.print(" ");
	}
}
