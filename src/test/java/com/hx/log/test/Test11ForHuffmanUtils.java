/**
 * file name : Test11ForHuffmanUtils.java
 * created at : ����3:20:06 2016��8��21��
 * created by 970655147
 */

package com.hx.log.test;

import static com.hx.log.util.Log.log;

import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;

import com.hx.log.alogrithm.huffman.comparable_mergeable.CMInteger;
import com.hx.log.util.Constants;
import com.hx.log.alogrithm.huffman.HuffmanUtils;
import com.hx.log.util.Log;

public class Test11ForHuffmanUtils {

	// HuffMan �㷨����ѹ��
	// refer : http://www.cnblogs.com/Jezze/archive/2011/12/23/2299884.html
	// saved : E:\Course Files\JAVA\Java����֪ʶ\�������������������.docx
	public static void main(String []args) {
		
		log.setLogPattern(Constants.JUST_PRINT_MSG_LOG_PATTERN);
		
		int[] arr = new int[]{1, 2, 3, 4, 5 };
		
		PriorityQueue<HuffmanUtils.Node<CMInteger>> queue = new PriorityQueue<>();
		for(int i=0; i<arr.length; i++) {
			queue.add(new HuffmanUtils.Node<CMInteger>(new CMInteger(arr[i]), null, null) );
		}
		HuffmanUtils.Node<CMInteger> root = HuffmanUtils.constructHuffmanTree(queue);
		Log.log(root.toString() );
		
		List<HuffmanUtils.Node<CMInteger>> leaves = new ArrayList<>(arr.length);
		HuffmanUtils.collectLeaveNodes(root, leaves);		
		Log.log(leaves.iterator() );
		
		for(int i=0; i<leaves.size(); i++) {
			int idx = i;
			String huffManCode = leaves.get(idx).getHuffManCode();
//				Log.log(leaves.get(idx) );
			Log.log(huffManCode);
		}
	}
	
}
