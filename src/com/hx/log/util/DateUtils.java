/**
 * file name : DateUtils.java
 * created at : 23:08:54 2016-12-30
 * created by 970655147
 */

package com.hx.log.util;

public final class DateUtils {

	// disable constructor
	private DateUtils() {
		Tools.assert0("can't instantiate !");
	}
	
	
	// ��ȡ���ڵĺ�����, �Լ�����start��ȡ������ʱ��
	public static long now() {
		return System.currentTimeMillis();
	}
	public static String nowStr() {
		return String.valueOf(now() );
	}
	public static String formatedNowStr() {
		return Constants.DATE_FORMAT.format(now() );
	}
	public static long spent(long start) {
		return now() - start;
	}
	public static String spentStr(long start) {
		return String.valueOf(spent(start) );
	}
	
}
