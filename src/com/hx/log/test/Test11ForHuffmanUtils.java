/**
 * file name : Test11ForHuffmanUtils.java
 * created at : 下午3:20:06 2016年8月21日
 * created by 970655147
 */

package com.hx.log.test;

import static com.hx.log.util.Log.log;

import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;

import com.hx.log.util.Constants;
import com.hx.log.util.HuffmanUtils;
import com.hx.log.util.HuffmanUtils.CMInteger;
import com.hx.log.util.HuffmanUtils.Node;
import com.hx.log.util.Log;
import com.hx.log.util.Tools;

public class Test11ForHuffmanUtils {

	// HuffMan 算法进行压缩
	// refer : http://www.cnblogs.com/Jezze/archive/2011/12/23/2299884.html
	// saved : E:\Course Files\JAVA\Java基础知识\哈夫曼树与哈夫曼编码.docx
	public static void main(String []args) {
		
		log.logPatternChain = Constants.JUST_PRINT_MSG_LOG_PATTERN;
		
		int[] arr = new int[]{1, 2, 3, 4, 5 };
		
		PriorityQueue<Node<CMInteger> > queue = new PriorityQueue<>();
		for(int i=0; i<arr.length; i++) {
			queue.add(new Node<CMInteger>(new CMInteger(arr[i]), null, null) );
		}
		Node<CMInteger> root = HuffmanUtils.constructHuffmanTree(queue);
		Log.log(root.toString() );
		
		List<Node<CMInteger> > leaves = new ArrayList<>(arr.length);
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
