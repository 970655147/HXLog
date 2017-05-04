/**
 * file name : IdxManager.java
 * created at : 5:25:17 PM Jun 18, 2016
 * created by 970655147
 */

package com.hx.log.idx;

import java.util.HashMap;
import java.util.Map;

// IdxManager
public class IdxManager<IdxType> {

	/**
	 * doLoad µÄMap
	 */
	private Map<String, IdxType> doLoad = new HashMap<>();
	/**
	 * doFilter µÄMap
	 */
	private Map<String, IdxType> doFilter = new HashMap<>();

	/**
	 * setter & getter
	 */
	public void putDoLoad(String key, IdxType val) {
		doLoad.put(key, val);
	}
	public void putDoFilter(String key, IdxType val) {
		doFilter.put(key, val);
	}
	public IdxType getDoLoad(String key) {
		return doLoad.get(key);
	}
	public IdxType getDoFilter(String key) {
		return doFilter.get(key);
	}
	public Map<String, IdxType> getDoLoad() {
		return doLoad;
	}
	public Map<String, IdxType> getDoFilter() {
		return doFilter;
	}

}
