package com.hx.log.util;
/**
 * file name : Log.java
 * created at : 8:10:53 PM Apr 22, 2015
 * created by 970655147
 */

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

// 打印数据相关的类
public class Log {
	
	// 不允许实例化
	private Log() {
		Tools.assert0("can't instance 'Log' !");
	}
	
	// 将所有的业务委托给Log.log		add at 2016.05.30
	public static Logger log = new Logger();
	
	// --------------------------- 配置可配置变量的接口 ----------------------------------------
	public static void setOutLogFile(String logFile) throws Exception {
		log.setOutLogFile(logFile);
	}
	public static void setErrLogFile(String logFile) throws Exception {
		log.setErrLogFile(logFile);
	}
	public static void setOutToLogFile(boolean outToLogFile, String logFile) throws Exception {
		log.setOutToLogFile(outToLogFile, logFile);
	}
	public static void setOutToLogFile(boolean outToLogFile) throws Exception {
		log.setOutToLogFile(outToLogFile);
	}
	public static void setErrToLogFile(boolean errToLogFile, String logFile) throws Exception {
		log.setErrToLogFile(errToLogFile, logFile);
	}
	public static void setErrToLogFile(boolean errToLogFile) throws Exception {
		log.setErrToLogFile(errToLogFile);
	}

	// --------------------------- 业务方法 ----------------------------------------
	// 标准输出
	// 打印字符串, 对象, 按照给定的pattern填充数据
	// add at 2016.07.02
	public static void dispathLogInfo(int modeIdx, String logStr) {
		log.dispathLogInfo(modeIdx, logStr);
	}
	public static void dispathLogInfo(int modeIdx, String logStr, boolean isFormat) {
		log.dispathLogInfo(modeIdx, logStr, isFormat);
	}
	
	public static void log() {
		log.log();
	}
	public static void log(String str, boolean appendCRLF) {
		log.log(str, appendCRLF);
	}
	public static void log(String str, boolean appendCRLF, int modeIdx) {
		log.log(str, appendCRLF, modeIdx);
	}
	public static void log(boolean appendCRLF) {
		log.log(appendCRLF);
	}
	public static void log(String obj) {
		log.log(obj);
	}
	public static void log(Object obj, boolean appendCRLF) {
		log.log(obj, appendCRLF);
	}
	public static void log(Object obj) {
		log.log(obj);
	}
	public static void logf(String pattern, Object[] args, boolean appendCRLF) {
		log.logf(pattern, args, appendCRLF);
	}
	public static void logf(String pattern, Object... args) {
		log.logf(pattern, args);
	}
	
	// 格式化需要打印的数据
	public String logLogPatternFormat(String content, boolean appendCRLF, boolean isFormat, int modeIdx) {
		return log.logLogPatternFormat(content, appendCRLF, isFormat, modeIdx);
	}
	public static String logLogPatternFormat(String content, boolean appendCRLF) {
		return log.logLogPatternFormat(content, appendCRLF);
	}
	public static String logLogPatternFormat(String content) {
		return log.logLogPatternFormat(content);
	}
	
	// 打印迭代器中的数据
	public static <T> void log(Iterator<T> it, String sep, boolean appendCRLF) {
		log.log(it, sep, appendCRLF);
	}
	public static <T> void log(Iterator<T> it, boolean appendCRLF) {
		log.log(it, appendCRLF);
	}
	public static <T> void log(Iterator<T> it) {
		log.log(it);
	}
	public static <T> void log(Iterator<T> it, String sep) {
		log.log(it, sep);
	}
	public static <T> void log(Iterator<T> it, String sep, int modeIdx, boolean appendCRLF) {
		log.log(it, sep, modeIdx, appendCRLF);
	}
	
	// 打印List
	public static <T> void log(List<T> list, String sep, boolean appendCRLF) {
		log.log(list, sep, appendCRLF);
	}
	public static <T> void log(List<T> list, boolean appendCRLF) {
		log.log(list, appendCRLF);
	}
	public static <T> void log(List<T> list) {
		log.log(list);
	}
	public static <T> void log(List<T> list, String sep) {
		log.log(list, sep);
	}
	
	// 打印Set
	public static <T> void log(Set<T> set, String sep, boolean appendCRLF) {
		log.log(set, sep, appendCRLF);
	}
	public static <T> void log(Set<T> set, boolean appendCRLF) {
		log.log(set, appendCRLF);
	}
	public static <T> void log(Set<T> set) {
		log.log(set);
	}
	public static <T> void log(Set<T> set, String sep) {
		log.log(set, sep);
	}
	
	// 打印Map
	public static <K, V> void log(Map<K, V> map, String sep, int modeIdx, boolean appendCRLF) {
		log.log(map, sep, modeIdx, appendCRLF);
	}
	public static <K, V> void log(Map<K, V> map, String kvSep, String sep, boolean appendCRLF) {
		log.log(map, kvSep, sep, appendCRLF);
	}
	public static <K, V> void log(Map<K, V> map, String kvSep, String sep) {
		log.log(map, kvSep, sep);
	}
	public static <K, V> void log(Map<K, V> map, String sep, boolean appendCRLF) {
		log.log(map, sep, appendCRLF);
	}
	public static <K, V> void log(Map<K, V> map, boolean appendCRLF) {
		log.log(map, appendCRLF);
	}
	public static <K, V> void log(Map<K, V> map) {
		log.log(map);
	}
	public static <K, V> void log(Map<K, V> map, String sep) {
		log.log(map, sep);
	}
	public static <K, V> void log(Map<K, V> map, String kvSep, String sep, int modeIdx, boolean appendCRLF) {
		log.log(map, kvSep, sep, modeIdx, appendCRLF);
	}
	
	// 打印int[], long[], double[], char[], byte[], boolean[], Object[]
	public static void log(int[] ls, String sep, boolean appendCRLF) {
		log.log(ls, sep, appendCRLF);
	}
	public static void log(int[] ls, boolean appendCRLF) {
		log.log(ls, appendCRLF);
	}
	public static void log(int[] ls) {
		log.log(ls);
	}
	public static void log(int[] ls, String sep) {
		log.log(ls, sep);
	}
	public static void log(int[] arr, String sep, int modeIdx, boolean appendCRLF) {
		log.log(arr, sep, modeIdx, appendCRLF);
	}
	
	public static void log(long[] ls, String sep, boolean appendCRLF) {
		log.log(ls, sep, appendCRLF);
	}
	public static void log(long[] ls, boolean appendCRLF) {
		log.log(ls, appendCRLF);
	}
	public static void log(long[] ls) {
		log.log(ls);
	}
	public static void log(long[] ls, String sep) {
		log.log(ls, sep);
	}
	public static void log(long[] arr, String sep, int modeIdx, boolean appendCRLF) {
		log.log(arr, sep, modeIdx, appendCRLF);
	}
	
	public static void log(double[] ls, String sep, boolean appendCRLF) {
		log.log(ls, sep, appendCRLF);
	}
	public static void log(double[] ls, boolean appendCRLF) {
		log.log(ls, appendCRLF);
	}
	public static void log(double[] ls) {
		log.log(ls);
	}
	public static void log(double[] ls, String sep) {
		log.log(ls, sep);
	}
	public static void log(double[] arr, String sep, int modeIdx, boolean appendCRLF) {
		log.log(arr, sep, modeIdx, appendCRLF);
	}
	
	public static void log(char[] ls, String sep, boolean appendCRLF) {
		log.log(ls, sep, appendCRLF);
	}
	public static void log(char[] ls, boolean appendCRLF) {
		log.log(ls, appendCRLF);
	}
	public static void log(char[] ls) {
		log.log(ls);
	}
	public static void log(char[] ls, String sep) {
		log.log(ls, sep);
	}
	public static void log(char[] arr, String sep, int modeIdx, boolean appendCRLF) {
		log.log(arr, sep, modeIdx, appendCRLF);
	}
	
	public static void log(byte[] ls, String sep, boolean appendCRLF) {
		log.log(ls, sep, appendCRLF);
	}
	public static void log(byte[] ls, boolean appendCRLF) {
		log.log(ls, appendCRLF);
	}
	public static void log(byte[] ls) {
		log.log(ls);
	}
	public static void log(byte[] ls, String sep) {
		log.log(ls, sep);
	}
	public static void log(byte[] arr, String sep, int modeIdx, boolean appendCRLF) {
		log.log(arr, sep, modeIdx, appendCRLF);
	}
	
	public static void log(boolean[] ls, String sep, boolean appendCRLF) {
		log.log(ls, sep, appendCRLF);
	}
	public static void log(boolean[] ls, boolean appendCRLF) {
		log.log(ls, appendCRLF);
	}
	public static void log(boolean[] ls) {
		log.log(ls);
	}
	public static void log(boolean[] ls, String sep) {
		log.log(ls, sep);
	}
	public static void log(boolean[] arr, String sep, int modeIdx, boolean appendCRLF) {
		log.log(arr, sep, modeIdx, appendCRLF);
	}
	
	public static <T> void log(T[] ls, String sep, boolean appendCRLF) {
		log.log(ls, sep, appendCRLF);
	}
	public static <T> void log(T[] ls, boolean appendCRLF) {
		log.log(ls, appendCRLF);
	}
	public static <T> void log(T[] ls) {
		log.log(ls);
	}
	public static <T> void log(T[] ls, String sep) {
		log.log(ls, sep);
	}
	public static <T> void log(T[] arr, String sep, int modeIdx, boolean appendCRLF) {
		log.log(arr, sep, modeIdx, appendCRLF);
	}
	
	// 打印int[][], long[][], double[][], char[][], byte[][], boolean[][], Object[][]  格式如下 
	// 1 2 3 
	// 2 1 3
	// 3 2 1
	// int -> long -> char -> byte -> boolean -> T
	public static void log(int[][] arr, String sep) {
		log.log(arr, sep);
	}
	public static void log(int[][] arr) {
		log.log(arr);
	}
	public static void log(int[][] arr, String sep, int modeIdx) {
		log.log(arr, sep, modeIdx);
	}
	
	public static void log(long[][] arr, String sep) {
		log.log(arr, sep);
	}
	public static void log(long[][] arr) {
		log.log(arr);
	}
	public static void log(long[][] arr, String sep, int modeIdx) {
		log.log(arr, sep, modeIdx);
	}
	
	public static void log(double[][] arr, String sep) {
		log.log(arr, sep);
	}
	public static void log(double[][] arr) {
		log.log(arr);
	}
	public static void log(double[][] arr, String sep, int modeIdx) {
		log.log(arr, sep, modeIdx);
	}
	
	public static void log(char[][] arr, String sep) {
		log.log(arr, sep);
	}
	public static void log(char[][] arr) {
		log.log(arr);
	}
	public static void log(char[][] arr, String sep, int modeIdx) {
		log.log(arr, sep, modeIdx);
	}
	
	public static void log(byte[][] arr, String sep) {
		log.log(arr, sep);
	}
	public static void log(byte[][] arr) {
		log.log(arr);
	}
	public static void log(byte[][] arr, String sep, int modeIdx) {
		log.log(arr, sep, modeIdx);
	}
	
	public static void log(boolean[][] arr, String sep) {
		log.log(arr, sep);
	}
	public static void log(boolean[][] arr) {
		log.log(arr);
	}
	public static void log(boolean[][] arr, String sep, int modeIdx) {
		log.log(arr, sep, modeIdx);
	}
	
	// fix bug 'log(arr, sep, true)' -> 'log(arr, sep, Constants.OUT_IDX)'	add at 2016.05.14
	public static <T> void log(T[][] arr, String sep) {
		log.log(arr, sep);
	}
	public static <T> void log(T[][] arr) {
		log.log(arr);
	}
	public static <T> void log(T[][] arr, String sep, int modeIdx) {
		log.log(arr, sep, modeIdx);
	}
	
	// 按照给定的iterator迭代出来的索引, 打印arr中的元素
	// int -> long -> char -> byte -> boolean -> T
	public static void log(int[] arr, Iterator<Integer> it, String sep) {
		log.log(arr, it, sep);
	}
	public static void log(int[] arr, Iterator<Integer> it) {
		log.log(arr, it);
	}
	public static void log(int[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		log.log(arr, it, sep, modeIdx);
	}
	public static void log(long[] arr, Iterator<Integer> it, String sep) {
		log.log(arr, it, sep);
	}
	public static void log(long[] arr, Iterator<Integer> it) {
		log.log(arr, it);
	}
	public static void log(long[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		log.log(arr, it, sep, modeIdx);
	}
	public static void log(double[] arr, Iterator<Integer> it, String sep) {
		log.log(arr, it, sep);
	}
	public static void log(double[] arr, Iterator<Integer> it) {
		log.log(arr, it);
	}
	public static void log(double[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		log.log(arr, it, sep, modeIdx);
	}
	public static void log(char[] arr, Iterator<Integer> it, String sep) {
		log.log(arr, it, sep);
	}
	public static void log(char[] arr, Iterator<Integer> it) {
		log.log(arr, it);
	}
	public static void log(char[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		log.log(arr, it, sep, modeIdx);
	}
	public static void log(byte[] arr, Iterator<Integer> it, String sep) {
		log.log(arr, it, sep);
	}
	public static void log(byte[] arr, Iterator<Integer> it) {
		log.log(arr, it);
	}
	public static void log(byte[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		log.log(arr, it, sep, modeIdx);
	}
	public static void log(boolean[] arr, Iterator<Integer> it, String sep) {
		log.log(arr, it, sep);
	}
	public static void log(boolean[] arr, Iterator<Integer> it) {
		log.log(arr, it);
	}
	public static void log(boolean[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		log.log(arr, it, sep, modeIdx);
	}
	public static <T> void log(T[] arr, Iterator<Integer> it, String sep) {
		log.log(arr, it, sep);
	}
	public static <T> void log(T[] arr, Iterator<Integer> it) {
		log.log(arr, it);
	}
	public static <T> void log(T[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		log.log(arr, it, sep, modeIdx);
	}
	
	// 打印两个int, long, double, boolean, Object
	// int -> long -> char -> byte -> boolean -> T
	public static void log(int row, int col) {
		log.log(row, col);
	}
	public static void log(long row, long col) {
		log.log(row, col);
	}
	public static void log(double row, double col) {
		log.log(row, col);
	}
	public static void log(char row, char col) {
		log.log(row, col);
	}
	public static void log(byte row, byte col) {
		log.log(row, col);
	}
	public static void log(boolean bool01, boolean bool02) {
		log.log(bool01, bool02);
	}
	public static <T1, T2> void log(T1 row, T2 col) {
		log.log(row, col);
	}

	// 打印一条水平线
	public static void logHorizon(int n) {
		log.logHorizon(n);
	}
	public static void logHorizon() {
		log.logHorizon();
	}
	public static void logHorizon(int n, int modeIdx) {
		log.logHorizon(n, modeIdx);
	}
	
	// 键入一个/ n个回车
	public static void logEnter() {
		log.logEnter();
	}
	public static void logEnter(int n) {
		log.logEnter(n);
	}
	public static void logEnter(int n, int modeIdx) {
		log.logEnter(n, modeIdx);
	}
	
	// 打印自定义的主题
	public static void logForPage(String page) {
		log.logForPage(page);
	}
	public static void logForThemes(String theme) {
		log.logForThemes(theme);
	}
	public static void logForPage(Object page) {
		log.logForPage(page);
	}
	public static void logForThemes(Object theme) {
		log.logForThemes(theme);
	}
	public static void logFor(String subject, String subjectKey, String before, String after, int modeIdx) {
		log.logFor(subject, subjectKey, before, after, modeIdx);
	}
	
	// ----------------------------- seps ----------------------------------------
	
	// 错误输出
	public static void err(boolean appendCRLF) {
		log.err(appendCRLF);
	}
	public static void err() {
		log.err();
	}
	public static void err(String str, boolean appendCRLF) {
		log.err(str, appendCRLF);
	}
	public static void err(String obj) {
		log.err(obj);
	}
	public static void err(Object obj, boolean appendCRLF) {
		log.err(obj, appendCRLF);
	}
	public static void err(Object obj) {
		log.err(obj);
	}
	public static void errf(String pattern, Object[] args, boolean appendCRLF) {
		log.errf(pattern, args, appendCRLF);
	}
	public static void errf(String pattern, Object... args) {
		log.errf(pattern, args);
	}
	
	public static String errLogPatternFormat(String content, boolean appendCRLF) {
		return log.errLogPatternFormat(content, appendCRLF);
	}
	public static String errLogPatternFormat(String content) {
		return log.errLogPatternFormat(content);
	}
	
	public static <T> void err(Iterator<T> it, String sep, boolean appendCRLF) {
		log.err(it, sep, appendCRLF);
	}
	public static <T> void err(Iterator<T> it, boolean appendCRLF) {
		log.err(it, appendCRLF);
	}
	public static <T> void err(Iterator<T> it) {
		log.err(it);
	}
	public static <T> void err(Iterator<T> it, String sep) {
		log.err(it, sep);
	}
	
	public static <T> void err(List<T> list, String sep, boolean appendCRLF) {
		log.err(list, sep, appendCRLF);
	}
	public static <T> void err(List<T> list, boolean appendCRLF) {
		log.err(list, appendCRLF);
	}
	public static <T> void err(List<T> list) {
		log.err(list);
	}
	public static <T> void err(List<T> list, String sep) {
		log.err(list, sep);
	}
	public static <T> void err(Set<T> set, String sep, boolean appendCRLF) {
		log.err(set, sep, appendCRLF);
	}
	public static <T> void err(Set<T> set, boolean appendCRLF) {
		log.err(set, appendCRLF);
	}
	public static <T> void err(Set<T> set) {
		log.err(set);
	}
	public static <T> void err(Set<T> set, String sep) {
		log.err(set, sep);
	}
	public static <K, V> void err(Map<K, V> map, String kvSep, String sep, boolean appendCRLF) {
		log.err(map, kvSep, sep, appendCRLF);
	}
	public static <K, V> void err(Map<K, V> map, String kvSep, String sep) {
		log.err(map, kvSep, sep);
	}
	public static <K, V> void err(Map<K, V> map, String sep, boolean appendCRLF) {
		log.err(map, sep, appendCRLF);
	}
	public static <K, V> void err(Map<K, V> map, boolean appendCRLF) {
		log.err(map, appendCRLF);
	}
	public static <K, V> void err(Map<K, V> map) {
		log.err(map);
	}
	public static <K, V> void err(Map<K, V> map, String sep) {
		log.err(map, sep);
	}
	
	public static void err(int[] arr, String sep, boolean appendCRLF) {
		log.err(arr, sep, appendCRLF);
	}
	public static void err(int[] arr, boolean appendCRLF) {
		log.err(arr, appendCRLF);
	}
	public static void err(int[] arr) {
		log.err(arr);
	}
	public static void err(int[] arr, String sep) {
		log.err(arr, sep);
	}
	public static void err(long[] arr, String sep, boolean appendCRLF) {
		log.err(arr, sep, appendCRLF);
	}
	public static void err(long[] arr, boolean appendCRLF) {
		log.err(arr, appendCRLF);
	}
	public static void err(long[] arr) {
		log.err(arr);
	}
	public static void err(long[] arr, String sep) {
		log.err(arr, sep);
	}
	public static void err(double[] arr, String sep, boolean appendCRLF) {
		log.err(arr, sep, appendCRLF);
	}
	public static void err(double[] arr, boolean appendCRLF) {
		log.err(arr, appendCRLF);
	}
	public static void err(double[] arr) {
		log.err(arr);
	}
	public static void err(double[] arr, String sep) {
		log.err(arr, sep);
	}
	public static void err(byte[] arr, String sep, boolean appendCRLF) {
		log.err(arr, sep, appendCRLF);
	}
	public static void err(byte[] arr, boolean appendCRLF) {
		log.err(arr, appendCRLF);
	}
	public static void err(byte[] arr) {
		log.err(arr);
	}
	public static void err(byte[] arr, String sep) {
		log.err(arr, sep);
	}
	public static void err(char[] arr, String sep, boolean appendCRLF) {
		log.err(arr, sep, appendCRLF);
	}
	public static void err(char[] arr, boolean appendCRLF) {
		log.err(arr, appendCRLF);
	}
	public static void err(char[] arr) {
		log.err(arr);
	}
	public static void err(char[] arr, String sep) {
		log.err(arr, sep);
	}
	public static void err(boolean[] arr, String sep, boolean appendCRLF) {
		log.err(arr, sep, appendCRLF);
	}
	public static void err(boolean[] arr, boolean appendCRLF) {
		log.err(arr, appendCRLF);
	}
	public static void err(boolean[] arr) {
		log.err(arr);
	}
	public static void err(boolean[] arr, String sep) {
		log.err(arr, sep);
	}
	public static <T> void err(T[] arr, String sep, boolean appendCRLF) {
		log.err(arr, sep, appendCRLF);
	}
	public static <T> void err(T[] arr, boolean appendCRLF) {
		log.err(arr, appendCRLF);
	}
	public static <T> void err(T[] arr) {
		log.err(arr);
	}
	public static <T> void err(T[] arr, String sep) {
		log.err(arr, sep);
	}
	
	public static void err(int[][] arr, String sep) {
		log.err(arr, sep);
	}
	public static void err(int[][] arr) {
		log.err(arr);
	}
	public static void err(long[][] arr, String sep) {
		log.err(arr, sep);
	}
	public static void err(long[][] arr) {
		log.err(arr);
	}
	public static void err(double[][] arr, String sep) {
		log.err(arr, sep);
	}
	public static void err(double[][] arr) {
		log.err(arr);
	}
	public static void err(byte[][] arr, String sep) {
		log.err(arr, sep);
	}
	public static void err(byte[][] arr) {
		log.err(arr);
	}
	public static void err(char[][] arr, String sep) {
		log.err(arr, sep);
	}
	public static void err(char[][] arr) {
		log.err(arr);
	}
	public static void err(boolean[][] arr, String sep) {
		log.err(arr, sep);
	}
	public static void err(boolean[][] arr) {
		log.err(arr);
	}
	public static <T> void err(T[][] arr, String sep) {
		log.err(arr, sep);
	}
	public static <T> void err(T[][] arr) {
		log.err(arr);
	}
	public static void err(int[] arr, Iterator<Integer> it, String sep) {
		log.err(arr, it, sep);
	}
	public static void err(int[] arr, Iterator<Integer> it) {
		log.err(arr, it);
	}
	public static void err(long[] arr, Iterator<Integer> it, String sep) {
		log.err(arr, it, sep);
	}
	public static void err(long[] arr, Iterator<Integer> it) {
		log.err(arr, it);
	}
	public static void err(double[] arr, Iterator<Integer> it, String sep) {
		log.err(arr, it, sep);
	}
	public static void err(double[] arr, Iterator<Integer> it) {
		log.err(arr, it);
	}
	public static void err(char[] arr, Iterator<Integer> it, String sep) {
		log.err(arr, it, sep);
	}
	public static void err(char[] arr, Iterator<Integer> it) {
		log.err(arr, it);
	}
	public static void err(boolean[] arr, Iterator<Integer> it, String sep) {
		log.err(arr, it, sep);
	}
	public static void err(boolean[] arr, Iterator<Integer> it) {
		log.err(arr, it);
	}
	public static <T> void err(T[] arr, Iterator<Integer> it, String sep) {
		log.err(arr, it, sep);
	}
	public static <T> void err(T[] arr, Iterator<Integer> it) {
		log.err(arr, it);
	}
	
	public static void err(int row, int col) {
		log.err(row, col);
	}
	public static void err(long row, long col) {
		log.err(row, col);
	}
	public static void err(double row, double col) {
		log.err(row, col);
	}
	public static void err(char row, char col) {
		log.err(row, col);
	}
	public static void err(byte row, byte col) {
		log.err(row, col);
	}
	public static void err(boolean bool01, boolean bool02) {
		log.err(bool01, bool02);
	}
	public static <T1, T2> void err(T1 row, T2 col) {
		log.err(row, col);
	}
	public static void errHorizon(int n) {
		log.errHorizon(n);
	}
	public static void errHorizon() {
		log.errHorizon();
	}
	public static void errEnter() {
		log.errEnter();
	}
	public static void errEnter(int n) {
		log.errEnter(n);
	}
	public static void errForPage(String page) {
		log.errForPage(page);
	}
	public static void errForThemes(String theme) {
		log.errForThemes(theme);
	}
	public static void errForPage(Object page) {
		log.errForPage(page);
	}
	public static void errForThemes(Object theme) {
		log.errForThemes(theme);
	}
	
	// 刷出缓冲区的数据		add at 2016.04.15
	public static void flush() {
		log.flush();
	}
	
	// ------------ 待续 --------------------
	
	
}
