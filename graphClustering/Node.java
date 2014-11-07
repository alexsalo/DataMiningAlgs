package salo_dm;

import java.util.ArrayList;

public class Node {
	private ArrayList<String> links;
	private ArrayList<Integer> clusters;
	
	public Node(){
		this.links = new ArrayList<String>();
		this.clusters = new ArrayList<Integer>();
	}
	public void addLink(String link) {
		this.links.add(link);
	}
	public int deg(){
		return links.size();
	}
	public void addCluster(Integer clustid) {
		this.clusters.add(clustid);
	}	
	public void removeCluster(Integer clustid) {
		this.clusters.remove(clustid);
	}
	public boolean isClustered(){
		return clusters.size() > 0;
	}
	public ArrayList<String> getLinks(){
		return links;
	}
	public ArrayList<Integer> getClusters(){
		return clusters;
	}
	
	public boolean isInClust(int clustid){
		return this.clusters.contains(clustid);
	}
	
}