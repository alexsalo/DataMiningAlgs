package salo_dm;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;

public class Graph {
	private Map<String, Node> G;
	private int size = 0;
	//private Map<Integer, Double> entropies;

	public Graph() {
		G = new HashMap<String, Node>();
		//entropies = new HashMap<Integer, Double>();
	}

	private void addNode(String name) {
		this.G.put(name, new Node());
		size++;
	}
	
	public void addLink(String from, String to){
		if (!G.containsKey(from))
			addNode(from);
		G.get(from).addLink(to);
	}

	public Node getNode(String name) {
		return G.get(name);
	}

	public int size() {
		return size;
	}
	
	private int howmanyclustered(){
		int i = 0;
		for (Entry<String, Node> entry : G.entrySet()) 
			if (entry.getValue().isClustered())
				i++;
		return i;
	}

	public double clusteredFraction() {
		return ((double)howmanyclustered() / size);
	}

	public void clust(String name, int clustid) {
		G.get(name).addCluster(clustid);
	}
	
	public void unclust(String name, int clustid) {
		G.get(name).removeCluster(clustid);		
	}

	public int deg(String name) {
		return G.get(name).deg();
	}	
	
	public ArrayList<String> getCluster(int clustid){
		ArrayList<String> cluster = new ArrayList<String>();
		for (Entry<String, Node> entry : G.entrySet()) {
			if (entry.getValue().isInClust(clustid))
				cluster.add(entry.getKey());
		}
		return cluster;
	}

	private double log2(double x) {
		return Math.log(x) / Math.log(2);
	}

	public String findVmaxDeg() {
		String name = "";
		int max = 0;
		for (Entry<String, Node> entry : G.entrySet()) {
			if (!entry.getValue().isClustered()) {
				int sz = entry.getValue().deg();
				if (sz > max) {
					max = sz;
					name = entry.getKey();
				}
			}
		}
		if (name.isEmpty())
			return null;
		else
			return name;
	}
	
	private double vEntropy(String name, int clustid){
		int i = 0;
		for (String s: G.get(name).getLinks())
			if (G.get(s).isInClust(clustid))
				i++;
		double pi = (double)i / G.get(name).deg();
		double po = 1 - pi;
		return (pi == 0 || po == 0) ? 0 : -pi * log2(pi) - po * log2(po);
	}
	
	public double gEntropy(int clustid){
		double gentropy = 0;
		HashSet<String> entropySet = new HashSet<String>();
		for (String s : getCluster(clustid))
			entropySet.addAll(G.get(s).getLinks());
		for (String s : entropySet) {
			gentropy += vEntropy(s, clustid);
		}
		return gentropy;		
	}
	
	public HashSet<String> getOuterBoundaryNodes(int clustid){
		HashSet<String> boundary = new HashSet<String>();
		for (String s : getCluster(clustid))
			boundary.addAll(G.get(s).getLinks());
		boundary.removeAll(getCluster(clustid));	
		return boundary;
	}
}
