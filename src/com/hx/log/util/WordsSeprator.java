/**
 * file name : WordSeprator.java
 * created at : 3:43:13 PM Mar 22, 2016
 * created by 970655147
 */

package com.hx.log.util;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

// 分割字符串的工具
public class WordsSeprator implements Iterator<String> {

	// 给定的字符串, 分隔符与下一个位置的映射, 需要跳过的符号对
	// 当前的索引
	private String str;
	private Map<String, Integer> sepToPos;
	private Map<String, String> escapeMap;
	private int idx;
	// 缓存next的值
	private String next;
	private String lastNext;
	private int lastNextIdx;
	private String last;
	// 是否获取分隔符, 当前是否应该返回分隔符
	private boolean gotSep;
	private boolean gotEmptyStr;
		private boolean nowSep;
		private String lastSep;
	
	// 初始化
	public WordsSeprator(String str, Set<String> seps, Map<String, String> escapeMap, boolean gotSep, boolean gotEmptyStr) {
		Tools.assert0(str != null, "str can't be null !");
		
		this.str = str;
		this.escapeMap = escapeMap;
		this.gotSep = gotSep;
		this.gotEmptyStr = gotEmptyStr;
		this.nowSep = false;
		this.lastSep = null;
		// update at 2016.04.21
		// update 'Map<String, Integer> sepToPos' => 'Set<String> seps', construct 'sepToPos' by this Constructor
			// incase of 'str' startsWith 'sep'
		this.sepToPos = new HashMap<>();
		if(seps != null) {
			for(String sep : seps) {
				sepToPos.put(sep, -1);
			}
		}
		
		// freshAll, got every 'sep''s right position!
		freshAll();
	}
	public WordsSeprator(String str, Set<String> seps, Map<String, String> escapeMap, boolean gotSep) {
		this(str, seps, escapeMap, gotSep, false);
	}
	public WordsSeprator(String str, Set<String> seps, Map<String, String> escapeMap) {
		this(str, seps, escapeMap, true, false);
	}

	@Override
	public boolean hasNext() {
		if(next != null) {
			return true;
		}
		
			// true 				&& 	true
		if((idx >= str.length()) && (! (nowSep && (lastSep != null))) ) {
			return false;
		}
		if(gotSep) {
			boolean isNowSep = nowSep;
			nowSep = ! nowSep;
			if(isNowSep) {
				next = lastSep;
				return hasNext();
			}
		}

		String sep = minSep();
		int pos = getPosBySep(sep);
		String res = null;
		lastNextIdx = idx;
		if(pos < 0) {
			res = str.substring(idx);
			idx = str.length();
			lastSep = null;
		} else {
			fresh(sep);
			res = str.substring(idx, pos);
			idx = pos + sep.length();
			lastSep = sep;
		}
		// because 'Constants' denpend on 'WordsSeprator', and 'Tools' denpend on 'Constants'
		// so use 'Constants.isEmpty' instead of 'Tools.isEmpty'[cause circle denpency] in case of 'InitException'
		// if 'res.trim' in 'Constants.EMPTY_CONDITIONS', skip it ! [may cause 'some space' loss]
		if((! gotEmptyStr) && Constants.isEmpty0(res) ) {
			return hasNext();
		}
		next = res;
		return hasNext();
	}
	
	@Override
	public String next() {
		last = lastNext;
		if(! hasNext() ) {
			return null;
		}
		
		lastNext = next;
		String res = next;
		next = null;
		return res;
	}
	public String seekLastNext() {
		if(! hasNext())	;
		return lastNext;
	}
	public int lastNextPos() {
		if(! hasNext())	;
		return lastNextIdx;
	}
	public int length() {
		return str.length();
	}
	public String seek() {
		if(! hasNext() ) {
			return null;
		}
		return next;
	}
	public String last() {
		if(! hasNext())	;
		return last;
	}
	public String rest() {
		if(! hasNext())	;
		return str.substring(idx - lastNext.length() );
	}
	public String rest(int pos) {
		if((pos < 0) || (pos >= str.length()) ) {
			return null;
		}
		return str.substring(pos);
	}

	@Override
	public void remove() {
		throw new RuntimeException("unsupportedOperation !");
	}
	
	// fresh所有的分隔符 / 给定的分隔符
	private void freshAll() {
		for(Entry<String, Integer> entry : sepToPos.entrySet() ) {
			fresh(entry.getKey() );
		}
	}
	private void fresh(String sep) {
		Integer pos = sepToPos.get(sep);
		if(pos != null ) {
			sepToPos.put(sep, indexOf(str, sep, pos+1) );
		}
	}
	private Integer indexOf(String str, String sep, int start) {
		int idx = start;
		whileLoop:
		while(idx < str.length() ) {
			if(escapeMap != null) {
				for(Entry<String, String> entry : escapeMap.entrySet() ) {
					if(str.startsWith(entry.getKey(), idx) ) {
						idx = str.indexOf(entry.getValue(), idx+entry.getKey().length() );
						if(idx < 0) {
							break whileLoop;
						}
						idx += entry.getValue().length();
						continue whileLoop;
					}
				}
			}
			if(str.startsWith(sep, idx) ) {
				return idx;
			}
			idx ++;
		}
		
		return -1;
	}

	// 获取距离现在最近的分隔符
	private String minSep() {
		int minPos = Integer.MAX_VALUE;
		String minSep = null;
		for(Entry<String, Integer> entry : sepToPos.entrySet() ) {
			if((entry.getValue() >= 0) && (entry.getValue() < minPos) ) {
				minPos = entry.getValue();
				minSep = entry.getKey();
			}
		}
		return minSep;
	}
	private int minPos() {
		String minSep = minSep();
		return getPosBySep(minSep);
	}
	private int getPosBySep(String sep) {
		return (sep == null) ? -1 : sepToPos.get(sep);
	}
		
}
