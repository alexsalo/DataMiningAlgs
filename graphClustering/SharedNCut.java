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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Scanner;

public class SharedNCut {
	private static final String FILENAME = "assignment4_data.txt";
	private static ArrayList<String> inputStrings;
	private static final double TRESHOLD = 0.3;
	private static HashMap<String, ArrayList<String>> graph;
	private static ArrayList<HashSet<String>> finalSubgraphs = new ArrayList<HashSet<String>>();

	public static void main(String[] args) {
		Instant startTime = Instant.now();

		readFile();
		initGraph();
		findDisconnectedSubgraphs();
		printReportToFile();

		Instant endTime = Instant.now();
		System.out.println(Duration.between(startTime, endTime));
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

	static void findDisconnectedSubgraphs() {
		// initial separation of graph by connected components
		HashSet<HashSet<String>> subgraphs = new HashSet<HashSet<String>>();
		HashSet<String> preseen = new HashSet<String>();
		HashSet<String> seen = new HashSet<String>();
		while (seen.size() != graph.size()) {
			//System.out.println(graph.size() - seen.size());
			HashSet<String> chooseFrom = new HashSet<String>(graph.keySet());
			chooseFrom.removeAll(seen);
			dfs(chooseFrom.iterator().next(), seen);
			HashSet<String> subgraph = new HashSet<String>(seen);
			subgraph.removeAll(preseen);
			subgraphs.add(subgraph);
			preseen = new HashSet<String>(seen);
		}
		// for connected components
		for (HashSet<String> subgraph : subgraphs) {
			// either mark as clustered
			if (density(subgraph) > TRESHOLD) 
				finalSubgraphs.add(subgraph);
			else 
				divideAndConquer(subgraph);
			
		}
	}

	static void divideAndConquer(HashSet<String> subgraph) {
		HashSet<String> seen = connected(subgraph.iterator().next());
		// while graph stays connected - remove edges
	
		while (seen.size() == subgraph.size()){ 
			removeEdge(subgraph);
			seen = connected(subgraph.iterator().next());
		}
		
		// do the same for each of two disconnected subgraphs
		HashSet<String> unseen = new HashSet<String>(subgraph);
		unseen.removeAll(seen);
		if (unseen.size() > 1)
			if (density(unseen) > TRESHOLD)
				finalSubgraphs.add(unseen);
			else
				divideAndConquer(unseen);
		if (seen.size() > 1)
			if (density(seen) > TRESHOLD) 
				finalSubgraphs.add(seen);
			else	
				divideAndConquer(seen);
	}

	static HashSet<String> connected(String start) {
		HashSet<String> seen = new HashSet<String>();
		seen.add(start);
		dfs(start, seen);
		return seen;
	}

	static void dfs(String u, HashSet<String> seen) {
		for (String s : graph.get(u))
			if (!seen.contains(s)) {
				seen.add(s);
				dfs(s, seen);
			}
	}

	static void removeEdge(HashSet<String> subgraph) {
		double min_jaccard = 99999;
		int max_union = 0;
		String min_a = null;
		String min_b = null;
		for (String from : subgraph) {
			for (String to : graph.get(from)) {
				if (subgraph.contains(to)) {
					double jaccard = jaccard(from, to);
					if (jaccard == 0) {
						min_jaccard = 0;
						int union = union(from, to);
						if (union > max_union) {
							min_a = from;
							min_b = to;
						}
					}
					if (jaccard < min_jaccard) {
						min_jaccard = jaccard;
						min_a = from;
						min_b = to;
					}
				}
			}
		}
		//System.out.println("Removing edge: " + min_a + " -> " + min_b);
		graph.get(min_a).remove(min_b);
		graph.get(min_b).remove(min_a);
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

	static void SortClusters() {
		// delete all that less than 3
		Iterator<HashSet<String>> itr = finalSubgraphs.iterator();
		while (itr.hasNext()) {
			HashSet<String> i = itr.next();
			if (i.size() < 3) {
				itr.remove();
			}
		}

		// sort by size
		Collections.sort(finalSubgraphs, new Comparator<HashSet<String>>() {
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
			writer = new PrintWriter("salo_assignment5.txt", "UTF-8");
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		writer.print(finalSubgraphs.size());
		for (int i = 0; i < finalSubgraphs.size(); i++) {
			writer.print("\n");
			writer.print(finalSubgraphs.get(i).size());
			for (String s : finalSubgraphs.get(i))
				writer.print(" " + s);
		}
		writer.close();
	}

}
