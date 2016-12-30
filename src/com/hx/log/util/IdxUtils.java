/**
 * file name : IdxUtils.java
 * created at : 23:06:17 2016-12-30
 * created by 970655147
 */

package com.hx.log.util;

import java.util.Collection;

public final class IdxUtils {
	
	// disable constructor
	private IdxUtils() {
		Tools.assert0("can't instantiate !");
	}

   // add at 2016.05.07
   // 获取索引相关
   public static int getIdx(int idx, String[] idxes) {
	   return getIdx(idx, idxes.length);
   }
   public static int getIdx(int idx, Collection<String> idxes) {
	   return getIdx(idx, idxes.size());
   }
   public static int getIdx(int idx, int maxSize) {
	   return getIdx(idx, maxSize, MapUtils.GET_INFO_FROM_JSON_DEFAULT_IDX);
   }
   public static int getIdx(int idx, String[] idxes, int defaultIdx) {
	   return getIdx(idx, idxes.length, defaultIdx);
   }
   public static int getIdx(int idx, Collection<String> idxes, int defaultIdx) {
	   return getIdx(idx, idxes.size(), defaultIdx);
   }
   public static int getIdx(int idx, int maxSize, int defaultIdx) {
	   return (idx >= maxSize) ? defaultIdx : idx;
   }


}
