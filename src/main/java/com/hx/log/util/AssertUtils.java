/**
 * file name : AssertUtils.java
 * created at : 20:32:00 2016-12-30
 * created by 970655147
 */

package com.hx.log.util;

public final class AssertUtils {
	
	// disable constructor
	private AssertUtils() {
		Tools.assert0("can't instantiate !");
	}
	
	// ------------ assert相关 ------- 2016.03.22 -------------
	// 工具方法
	// 确保boo为true, 否则 抛出异常
	public static void assert0(String msg) {
		assert0(false, msg);
	}
	public static void assert0(boolean boo, String msg) {
		if(msg == null) {
			Log.err("'msg' can't be null ");
			return ;
		}
		if(! boo) {
			throw new RuntimeException("assert0Exception : " + msg);
		}
	}
	// add at 2016.05.02
	public static void assert0(Exception e) {
		assert0(false, e);
	}
	public static void assert0(boolean boo, Exception e) {
		Tools.assert0(e != null, "'e' can't be null ");
		if(! boo) {
			throw new RuntimeException(e);
		}
	}
	// 确保val 和expected相同, 否则 抛出异常
	public static void assert0(int val, int expect, String errorMsg) {
		assert0(val, expect, true, errorMsg);
	}
	public static void assert0(int val, int expect, boolean isEquals, String errorMsg) {
		if(isEquals ^ (val == expect)) {
			String symbol = null;
			if(isEquals) {
				symbol = "!=";
			} else {
				symbol = "==";
			}
			assert0("assert0Exception : " + val + " " + symbol + ", expected : " + expect + ", MSG : " + errorMsg);
		}
	}
	public static <T> void assert0(T val, T expect, String errorMsg) {
		assert0(val, expect, true, errorMsg);
	}
	public static <T> void assert0(T val, T expect, boolean isEquals, String errorMsg) {
		if(val == null) {
			if(expect != null) {
				assert0("assert0Exception : " + val + " == null, expected : " + expect + ", MSG : " + errorMsg);
			}
			return ;
		}

		if(isEquals ^ (val.equals(expect)) ) {
			String symbol = null;
			if(isEquals) {
				symbol = "!=";
			} else {
				symbol = "==";
			}
			assert0("assert0Exception : " + String.valueOf(val) + " " + symbol + " " + String.valueOf(expect) + ", expected : " + String.valueOf(expect) + ", MSG : " + errorMsg );
		}
	}

}
