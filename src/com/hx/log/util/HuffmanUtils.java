/**
 * file name : HuffmanUtils.java
 * created at : 下午3:19:30 2016年8月21日
 * created by 970655147
 */

package com.hx.log.util;

import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.PriorityQueue;

public final class HuffmanUtils {

	// disable constructor
	private HuffmanUtils() {
		Tools.assert0("can't instantiate !");
	}
	
	// 根据给定的优先级队列, 构造huffmanTree
	// 典型的贪心算法实现
	public static <T extends ComparableAndMergeable<T>> Node<T> constructHuffmanTree(PriorityQueue<Node<T>> queue) {
		while(queue.size() > 1) {
			Node<T> smaller = queue.remove();
			Node<T> bigger = queue.remove();			
			Node<T> merged = new Node<T>(smaller.val.merge(bigger.val), smaller, bigger);
			smaller.setParent(merged);
			bigger.setParent(merged);
			queue.add(merged);
		}
		
		return queue.remove();
	}
	
	// 根据构建出来的huffmanTree, 收集各个叶子结点
	public static <T extends ComparableAndMergeable<T>> void collectLeaveNodes(Node<T> node, List<Node<T> > leaves) {
		if(node == null) {
			return ;
		}
		
		if(node.left != null) {
			collectLeaveNodes(node.left, leaves);
		} 
		if(node.right != null) {
			collectLeaveNodes(node.right, leaves);
		} 
		if((node.left == null) && (node.right == null)) {
			leaves.add(node);
		}
	}

	// --------------------------- 辅助数据结构 --------------------------------------
	// HuffMan结点
	public static class Node<T extends ComparableAndMergeable<T>> implements Comparable<Node<T>> {
		// 左右的字符串表示[计算huffmanCode]
		public static final String ROOT = "#";
		public static final String LEFT_CHILD = "0";
		public static final String RIGHT_CHILD = "1";
		// 左边为0, 右边为1
		public static final String LEFT = "0";
		public static final String RIGHT = "1";
		
		// 当前结点的数据, 父节点, 左右子节点, 以及当前结点对应的huffmanCode
		private T val;
		Node<T> parent;
		Node<T> left, right;
		String huffManCode;
		
		// 初始化
		public Node() {
			super();
		}
		public Node(T val, Node<T> left, Node<T> right) {
			this.val = val;
			this.left = left;
			this.right = right;
		}

		// setter & getter
		public void setParent(Node<T> parent) {
			this.parent = parent;
		}
		public T getVal() {
			return val;
		}
		public String getHuffManCode() {
			if(this.huffManCode == null) {
				huffManCode = calcHuffManCode();
			}
			
			return huffManCode;
		}
		
		// for TreeSet
		public int compareTo(Node<T> o) {
			return this.val.compareTo(o.val);
		}
		
		// 计算当前Node的huffmanCode
		private String calcHuffManCode() {
			StringBuilder sb = new StringBuilder();
			Node<T> node = this;
			
			while(node.parent != null ) {
				sb.append(isLeftOrRight(node) );
				node = node.parent;
			}
			
			return sb.reverse().toString();
		}
		// 获取当前结点的字符串表示
		private String isLeftOrRight(Node<T> node) {
			if(node == null) {
				return Tools.NULL;
			}
			Node<T> parent = node.parent;
			if(parent == null) {
				return ROOT;
			}
			
			if(parent.left == node) {
				return LEFT_CHILD;
			} else {
				return RIGHT_CHILD;
			}
		}
		
		// for debug ...
		public String toString() {
			return "val : " + val + ", left : [ " + String.valueOf(left) + " ], right : [ " + String.valueOf(right) + " ]";
		}
	}
	
	// 约定的给定的数据结构需要实现的compareTo, merge接口
	public static interface  ComparableAndMergeable<T> {
		public int compareTo(T other);
		public T merge(T other);
	}
	
	// CMInteger
	public static class CMInteger implements ComparableAndMergeable<CMInteger> {
		public int val;
		public CMInteger(int val) {
			this.val = val;
		}
		@Override
		public int compareTo(CMInteger other) {
			return this.val - other.val;
		}
		@Override
		public CMInteger merge(CMInteger other) {
			return new CMInteger(this.val + other.val);
		}
		@Override
		public String toString() {
			return String.valueOf(val);
		}
	}
	// CMLong
	public static class CMLong implements ComparableAndMergeable<CMLong> {
		public long val;
		public CMLong(long val) {
			this.val = val;
		}
		@Override
		public int compareTo(CMLong other) {
			long delta = this.val - other.val;
			return (delta > 0) ? 1 : ((delta == 0) ? 0 : -1);
		}
		@Override
		public CMLong merge(CMLong other) {
			return new CMLong(this.val + other.val);
		}
		@Override
		public String toString() {
			return String.valueOf(val);
		}
	}
	// CMDouble
	public static class CMDouble implements ComparableAndMergeable<CMDouble> {
		public double val;
		public CMDouble(double val) {
			this.val = val;
		}
		@Override
		public int compareTo(CMDouble other) {
			double delta = this.val - other.val;
			return (delta > 0.0) ? 1 : (GeometryUtils.equals(delta, 0.0d) ? 0 : -1);
		}
		@Override
		public CMDouble merge(CMDouble other) {
			return new CMDouble(this.val + other.val);
		}
		@Override
		public String toString() {
			return String.valueOf(val);
		}
	}
	
}
