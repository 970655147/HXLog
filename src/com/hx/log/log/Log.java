package com.hx.log.log;
/**
 * file name : Log.java
 * created at : 8:10:53 PM Apr 22, 2015
 * created by 970655147
 */


import java.io.IOException;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import net.sf.json.JSONObject;

import com.hx.log.log.LogPattern.LogPatternChain;
import com.hx.log.log.LogPattern.LogPatternType;

// 打印数据相关的类
public class Log {
	
	// --------------------------- 可配置变量 --------------------------------------
	// 以及输出流, 错误流, 以及默认是否换行
	public static String HORIZON_LINES = Constants.HORIZON_LINES;
	public static String HORIZON_STARTS = Constants.HORIZON_STARTS;
	public static String GOT_THERE = Constants.GOT_THERE;
	public static String GOT_NOTHING = Constants.GOT_NOTHING;
	
	public static OutputStream[] outStreams = Arrays.copyOf(Constants.outStreams, Constants.outStreams.length);
	private static boolean[] outToLogFile = Arrays.copyOf(Constants.outToLogFile, Constants.outToLogFile.length);
	public final static String[] logBufNames = Arrays.copyOf(Constants.logBufNames, Constants.logBufNames.length);
	private static String[] logFiles = Arrays.copyOf(Constants.logFiles, Constants.logFiles.length);
	public static LogPatternChain logPatternChain = Constants.logPatternChain;
	
	public static String DEFAULT_SEP_WHILE_CRLF = Constants.DEFAULT_SEP_WHILE_CRLF;
	public static String DEFAULT_SEP_WHILE_NO_CRLF = Constants.DEFAULT_SEP_WHILE_NO_CRLF;
	public static String DEFAULT_SEP_WHILE_TWO_DIMEN = Constants.DEFAULT_SEP_WHILE_TWO_DIMEN;
	public static String DEFAULT_MAP_KV_SEP = Constants.DEFAULT_MAP_KV_SEP;
	
	public static boolean OUTPUT_APPEND_CRLF = Constants.OUTPUT_APPEND_CRLF;
	public static boolean ERRPUT_APPEND_CRLF = Constants.ERRPUT_APPEND_CRLF;
	public static boolean OUTPUT_APPEND_CRLF_FOR_CONTAINER = Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER;
	public static boolean ERRPUT_APPEND_CRLF_FOR_CONTAINER = Constants.ERRPUT_APPEND_CRLF_FOR_CONTAINER;
	// --------------------------- 置于最后 ----------------------------------------
	
	// 初始化
	static {
		try {
			for(int i=0; i<Constants.LOG_MODES.length; i++) {
				if(outToLogFile[i]) {
					setLogFile0(logFiles[i], i);
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	// --------------------------- 配置可配置变量的接口 ----------------------------------------
	public static void setOutLogFile(String logFile) throws IOException {
		setLogFile0(logFile, Constants.OUT_IDX);
	}
	public static void setErrLogFile(String logFile) throws IOException {
		setLogFile0(logFile, Constants.ERR_IDX);
	}
	public static void setOutToLogFile(boolean outToLogFile, String logFile) throws IOException {
		setToLogFile0(outToLogFile, logFile, Constants.OUT_IDX);
	}
	public static void setOutToLogFile(boolean outToLogFile) throws IOException {
		setOutToLogFile(outToLogFile, logFiles[Constants.OUT_IDX]);
	}
	public static void setErrToLogFile(boolean errToLogFile, String logFile) throws IOException {
		setToLogFile0(errToLogFile, logFile, Constants.ERR_IDX);
	}
	public static void setErrToLogFile(boolean errToLogFile) throws IOException {
		setErrToLogFile(errToLogFile, logFiles[Constants.ERR_IDX]);
	}
	
	// 辅助方法
	private static void setLogFile0(String logFile, int modeIdx) throws IOException {
		if(logFiles[modeIdx] != null ) {
			setLogFile00(logFile, modeIdx);
		} else {
			Log.log(Constants.LOG_MODES[modeIdx] + "'s outputFile is null, maybe not support out log to 'logFile', use 'setXXXToLogFile' insted !");
		}
	}
	private static void setLogFile00(String logFile, int modeIdx) throws IOException {
		// 和当前流输出相同的第一个流的索引[校验是否需要关流, 以及更新logBufName]
		int firstNonMeSameBuffIdx = -1;
		for(int i=0; i<Constants.LOG_MODES.length; i++) {
			if(i == modeIdx) continue ;
			if(outToLogFile[i] && (logFiles[modeIdx].equals(logFiles[i])) ) {
				firstNonMeSameBuffIdx = i;
				break ;
			}
		}
		
		// 如果是设置logFile为空, 则校验是否需要关掉缓冲, 如果需要则关掉缓冲
		if(logFile == null) {
			if((firstNonMeSameBuffIdx < 0) && Tools.bufExists(logBufNames[modeIdx]) ) {
				Tools.closeAnBuffer(logBufNames[modeIdx]);
			}
			outToLogFile[modeIdx] = false;
			logFiles[modeIdx] = null;
			return ;
		}
		
		// 和logFile相同的第一个非当前流的索引[校验是否需要创建新的缓冲]
		int sameBufIdx = -1;
		for(int i=0; i<Constants.LOG_MODES.length; i++) {
			if(i == modeIdx) continue ;
			if(logFile.equals(logFiles[i])) {
				sameBufIdx = i;
				break ;
			}
		}
		boolean needCreateNewBuf = sameBufIdx < 0;
		String oldBufName = logBufNames[modeIdx];
		// 如果需要创建新的buff, 则更新logBufNames[modeIdx]
		// 如果logBufNames[modeIdx] 和Constants.logBufNames[modeIdx]相同, 表示有其他的流关联在logBufNames[modeIdx]上面, 更新其他的流的
		if(needCreateNewBuf) {
			// 更新可能存在的多个关联在同一个缓冲上面的其他缓冲的key[散] 
			if(logBufNames[modeIdx].equals(Constants.logBufNames[modeIdx]) ) {
				if(firstNonMeSameBuffIdx >= 0) {
					for(int i=firstNonMeSameBuffIdx; i<Constants.LOG_MODES.length; i++) {
						if(logBufNames[i].equals(logBufNames[modeIdx]) ) {
							logBufNames[i] = Constants.logBufNames[firstNonMeSameBuffIdx];
						}
					}
					// 关闭原来的缓冲[由之后的createBuffer创建], 然后为关联在当前流的其他流创建新的缓冲[暂不考虑并发情况]
					Tools.closeAnBuffer(logBufNames[modeIdx]);
					Tools.createAnBuffer(Constants.logBufNames[firstNonMeSameBuffIdx], logBufNames[firstNonMeSameBuffIdx]);
				}
			}
			logBufNames[modeIdx] = Constants.logBufNames[modeIdx];
			// 更新当前缓冲为已经存在的缓冲[聚]
		} else {
			logBufNames[modeIdx] = logBufNames[sameBufIdx];
		}
		
		// 如果logFiles[modeIdx]不为空, 并且logFile 与logFiles[modeIdx]不相同, 则根据情况, 创建缓冲
		// 否则, 创建缓冲, 或者doNothing
		if((logFiles[modeIdx] != null) && (! logFiles[modeIdx].equals(logFile)) ) {
			if((firstNonMeSameBuffIdx < 0) && Tools.bufExists(logBufNames[modeIdx]) ) {
				Tools.flushBuffer(oldBufName, true);
			}
			if(needCreateNewBuf) {
				Tools.createAnBuffer(logBufNames[modeIdx], logFile);
			}
		} else {
			if(! Tools.bufExists(logBufNames[modeIdx]) ) {
				Tools.createAnBuffer(logBufNames[modeIdx], logFile);	
			} else {
				Log.log("specified : 'logFile' is current 'Log.logFile', ignore !");
			}
		}
		logFiles[modeIdx] = logFile;
	}
	private static void setToLogFile0(boolean toLogFile, String logFile, int modeIdx) throws IOException {
		Log.outToLogFile[modeIdx] = toLogFile;
		if(toLogFile) {
			setLogFile0(logFile, modeIdx);
		}
	}
	// add at 2016.05.07
	private static void dispathLogInfo(int modeIdx, String logStr) {
		// dispatch
		switch (modeIdx) {
			case Constants.OUT_IDX :
				log(logStr );
				break;
			case Constants.ERR_IDX :
				err(logStr );
				break;
			default:
				err("have no this 'modeIdx', current support " + Constants.LOG_MODES_STR + " ");
				break;
		}
	}
	
	// --------------------------- 业务方法 ----------------------------------------
	// 标准输出
	// 打印字符串, 对象, 按照给定的pattern填充数据
	public static void log(String str, boolean appendCRLF, int modeIdx) {
		Tools.assert0(str != null, "'str' is null ");
		try {
			StringBuilder sb = new StringBuilder(str.length() + 4);
			sb.append(Constants.formatLogInfo(logPatternChain, new JSONObject().element(LogPatternType.MSG.typeKey(), str).element(LogPatternType.MODE.typeKey(), Constants.LOG_MODES[Tools.getIdx(modeIdx, Constants.LOG_MODES.length, Constants.ERR_IDX)])) );
			if(appendCRLF) {
				sb.append(Constants.CRLF );
			}
			String line = sb.toString();
			
			if((modeIdx < 0) || (modeIdx >= outStreams.length) ) {
				err("have no this 'modeIdx', current support " + Constants.LOG_MODES_STR + " ");
				return ;
			}
			
			if(outStreams[modeIdx] != null) {
				outStreams[modeIdx].write(line.getBytes(Tools.DEFAULT_CHARSET) );
			}
			if(outToLogFile[modeIdx]) {
				Tools.appendBuffer(logBufNames[modeIdx], line);
			}
		} catch (IOException e) {
			Tools.assert0(Tools.errorMsg(e) );
		}
	}
	public static void log(boolean appendCRLF) {
		log(GOT_THERE, appendCRLF);
	}
	public static void log() {
		log(GOT_THERE, Constants.OUTPUT_APPEND_CRLF);
	}
	public static void log(String str, boolean appendCRLF) {
		log(str, appendCRLF, Constants.OUT_IDX);
	}
	public static void log(String obj) {
		log(obj, Constants.OUTPUT_APPEND_CRLF);
	}
	public static void log(Object obj, boolean appendCRLF) {
		log(String.valueOf(obj), appendCRLF );
	}
	public static void log(Object obj) {
		log(obj, Constants.OUTPUT_APPEND_CRLF );
	}
	public static void logf(String pattern, Object[] args, boolean appendCRLF) {
		log(String.format(pattern, args), appendCRLF);
	}
	public static void logf(String pattern, Object... args) {
		logf(pattern, args, Constants.OUTPUT_APPEND_CRLF);
	}
	
	// 打印迭代器中的数据
	public static <T> void log(Iterator<T> it, String sep, boolean appendCRLF) {
		log(it, sep, Constants.OUT_IDX, appendCRLF);
	}
	public static <T> void log(Iterator<T> it, boolean appendCRLF) {
		if(appendCRLF) {
			log(it, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			log(it, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static <T> void log(Iterator<T> it) {
		log(it, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static <T> void log(Iterator<T> it, String sep) {
		log(it, sep, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static <T> void log(Iterator<T> it, String sep, int modeIdx, boolean appendCRLF) {
		Tools.assert0(it != null, "'Iterator' is null ");
		Tools.assert0(sep != null, "'Seprator' is null ");
		
		StringBuilder sb = new StringBuilder();
		if(appendCRLF) {
			while(it.hasNext()) {
				Tools.appendCRLF(sb, (it.next().toString() + sep) );
			}
		} else {
			while(it.hasNext()) {
				Tools.append(sb, it.next().toString() + sep);
			}
		}
		// incase of 'it.hasNext == false'
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		
		// dispatch
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	// 打印List
	public static <T> void log(List<T> list, String sep, boolean appendCRLF) {
		Tools.assert0(list != null, "'list' is null ");
		log(list.iterator(), sep, appendCRLF);
	}
	public static <T> void log(List<T> list, boolean appendCRLF) {
		Tools.assert0(list != null, "'list' is null ");
		log(list.iterator(), appendCRLF);
	}
	public static <T> void log(List<T> list) {
		Tools.assert0(list != null, "'list' is null ");
		log(list.iterator(), Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static <T> void log(List<T> list, String sep) {
		Tools.assert0(list != null, "'list' is null ");
		log(list.iterator(), sep, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	
	// 打印Set
	public static <T> void log(Set<T> set, String sep, boolean appendCRLF) {
		Tools.assert0(set != null, "'set' is null ");
		log(set.iterator(), sep, appendCRLF);
	}
	public static <T> void log(Set<T> set, boolean appendCRLF) {
		Tools.assert0(set != null, "'set' is null ");
		log(set.iterator(), appendCRLF);
	}
	public static <T> void log(Set<T> set) {
		Tools.assert0(set != null, "'set' is null ");
		log(set.iterator(), Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static <T> void log(Set<T> set, String sep) {
		Tools.assert0(set != null, "'set' is null ");
		log(set.iterator(), sep, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	
	// 打印Map
	public static <K, V> void log(Map<K, V> map, String sep, int modeIdx, boolean appendCRLF) {
		log(map, DEFAULT_MAP_KV_SEP, sep, modeIdx, appendCRLF);
	}
	public static <K, V> void log(Map<K, V> map, String kvSep, String sep, boolean appendCRLF) {
		log(map, kvSep, sep, Constants.OUT_IDX, appendCRLF);
	}
	public static <K, V> void log(Map<K, V> map, String kvSep, String sep) {
		log(map, kvSep, sep, true);
	}
	public static <K, V> void log(Map<K, V> map, String sep, boolean appendCRLF) {
		log(map, DEFAULT_MAP_KV_SEP, sep, Constants.OUT_IDX, appendCRLF);
	}
	public static <K, V> void log(Map<K, V> map, boolean appendCRLF) {
		if(appendCRLF) {
			log(map, DEFAULT_MAP_KV_SEP, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			log(map, DEFAULT_MAP_KV_SEP, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static <K, V> void log(Map<K, V> map) {
		log(map, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static <K, V> void log(Map<K, V> map, String sep) {
		log(map, sep, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static <K, V> void log(Map<K, V> map, String kvSep, String sep, int modeIdx, boolean appendCRLF) {
		Tools.assert0(map != null, "'map' is null ");
		StringBuilder sb = new StringBuilder();
		if(appendCRLF) {
			for(Entry<K, V> entry : map.entrySet() ) {
				Tools.appendCRLF(sb, entry.getKey() + kvSep + entry.getValue() + sep );
			}
		} else {
			for(Entry<K, V> entry : map.entrySet() ) {
				Tools.append(sb, entry.getKey() + " -> " + entry.getValue() + sep );
			}
		}
		// incase of 'it.hasNext == false'
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		
		// dispatch
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	// 打印int[], long[], double[], char[], byte[], boolean[], Object[]
	public static void log(int[] ls, String sep, boolean appendCRLF) {
		log(ls, sep, Constants.OUT_IDX, appendCRLF);
	}
	public static void log(int[] ls, boolean appendCRLF) {
		if(appendCRLF) {
			log(ls, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			log(ls, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static void log(int[] ls) {
		log(ls, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void log(int[] ls, String sep) {
		log(ls, sep, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void log(int[] arr, String sep, int modeIdx, boolean appendCRLF) {
		Tools.assert0(arr != null, "'arr' is null ");
		StringBuilder sb = new StringBuilder();
		if(appendCRLF) {
			for(int obj : arr) {
				Tools.appendCRLF(sb, (obj + sep) );
			}
		} else {
			for(int obj : arr) {
				Tools.append(sb, obj + sep);
			}
		}
		// incase of 'it.hasNext == false'
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public static void log(long[] ls, String sep, boolean appendCRLF) {
		log(ls, sep, Constants.OUT_IDX, appendCRLF);
	}
	public static void log(long[] ls, boolean appendCRLF) {
		if(appendCRLF) {
			log(ls, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			log(ls, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static void log(long[] ls) {
		log(ls, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void log(long[] ls, String sep) {
		log(ls, sep, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void log(long[] arr, String sep, int modeIdx, boolean appendCRLF) {
		Tools.assert0(arr != null, "'arr' is null ");
		StringBuilder sb = new StringBuilder();
		if(appendCRLF) {
			for(long obj : arr) {
				Tools.appendCRLF(sb, (obj + sep) );
			}
		} else {
			for(long obj : arr) {
				Tools.append(sb, obj + sep);
			}
		}
		// incase of 'it.hasNext == false'
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		
		dispathLogInfo(modeIdx, sb.toString() );
	}

	public static void log(double[] ls, String sep, boolean appendCRLF) {
		log(ls, sep, Constants.OUT_IDX, appendCRLF);
	}
	public static void log(double[] ls, boolean appendCRLF) {
		if(appendCRLF) {
			log(ls, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			log(ls, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static void log(double[] ls) {
		log(ls, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void log(double[] ls, String sep) {
		log(ls, sep, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void log(double[] arr, String sep, int modeIdx, boolean appendCRLF) {
		Tools.assert0(arr != null, "'arr' is null ");
		StringBuilder sb = new StringBuilder();
		if(appendCRLF) {
			for(double obj : arr) {
				Tools.appendCRLF(sb, (obj + sep) );
			}
		} else {
			for(double obj : arr) {
				Tools.append(sb, obj + sep);
			}
		}
		// incase of 'it.hasNext == false'
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public static void log(char[] ls, String sep, boolean appendCRLF) {
		log(ls, sep, Constants.OUT_IDX, appendCRLF);
	}
	public static void log(char[] ls, boolean appendCRLF) {
		if(appendCRLF) {
			log(ls, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			log(ls, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static void log(char[] ls) {
		log(ls, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void log(char[] ls, String sep) {
		log(ls, sep, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void log(char[] arr, String sep, int modeIdx, boolean appendCRLF) {
		Tools.assert0(arr != null, "'arr' is null ");
		StringBuilder sb = new StringBuilder();
		if(appendCRLF) {
			for(char obj : arr) {
				Tools.appendCRLF(sb, (obj + sep) );
			}
		} else {
			for(char obj : arr) {
				Tools.append(sb, obj + sep);
			}
		}
		// incase of 'it.hasNext == false'
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public static void log(byte[] ls, String sep, boolean appendCRLF) {
		log(ls, sep, Constants.OUT_IDX, appendCRLF);
	}
	public static void log(byte[] ls, boolean appendCRLF) {
		if(appendCRLF) {
			log(ls, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			log(ls, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static void log(byte[] ls) {
		log(ls, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void log(byte[] ls, String sep) {
		log(ls, sep, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void log(byte[] arr, String sep, int modeIdx, boolean appendCRLF) {
		Tools.assert0(arr != null, "'arr' is null ");
		StringBuilder sb = new StringBuilder();
		if(appendCRLF) {
			for(byte obj : arr) {
				Tools.appendCRLF(sb, (obj + sep) );
			}
		} else {
			for(byte obj : arr) {
				Tools.append(sb, obj + sep);
			}
		}
		// incase of 'it.hasNext == false'
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public static void log(boolean[] ls, String sep, boolean appendCRLF) {
		log(ls, sep, Constants.OUT_IDX, appendCRLF);
	}
	public static void log(boolean[] ls, boolean appendCRLF) {
		if(appendCRLF) {
			log(ls, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			log(ls, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static void log(boolean[] ls) {
		log(ls, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void log(boolean[] ls, String sep) {
		log(ls, sep, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void log(boolean[] arr, String sep, int modeIdx, boolean appendCRLF) {
		Tools.assert0(arr != null, "'arr' is null ");
		StringBuilder sb = new StringBuilder();
		if(appendCRLF) {
			for(boolean obj : arr) {
				Tools.appendCRLF(sb, (obj + sep) );
			}
		} else {
			for(boolean obj : arr) {
				Tools.append(sb, obj + sep);
			}
		}
		// incase of 'it.hasNext == false'
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		
		dispathLogInfo(modeIdx, sb.toString() );
	}	
	
	public static <T> void log(T[] ls, String sep, boolean appendCRLF) {
		log(ls, sep, Constants.OUT_IDX, appendCRLF);
	}
	public static <T> void log(T[] ls, boolean appendCRLF) {
		if(appendCRLF) {
			log(ls, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			log(ls, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static <T> void log(T[] ls) {
		log(ls, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static <T> void log(T[] ls, String sep) {
		log(ls, sep, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static <T> void log(T[] arr, String sep, int modeIdx, boolean appendCRLF) {
		Tools.assert0(arr != null, "'arr' is null ");
		StringBuilder sb = new StringBuilder();
		if(appendCRLF) {
			for(Object obj : arr) {
				Tools.appendCRLF(sb, (obj.toString() + sep) );
			}
		} else {
			for(Object obj : arr) {
				Tools.append(sb, obj.toString() + sep);
			}
		}
		// incase of 'it.hasNext == false'
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	// 打印int[][], long[][], double[][], char[][], byte[][], boolean[][], Object[][]  格式如下 
	// 1 2 3 
	// 2 1 3
	// 3 2 1
	// int -> long -> char -> byte -> boolean -> T
	public static void log(int[][] arr, String sep) {
		log(arr, sep, Constants.OUT_IDX);
	}
	public static void log(int[][] arr) {
		log(arr, DEFAULT_SEP_WHILE_TWO_DIMEN);
	}
	public static void log(int[][] arr, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		for(int i=0; i<arr.length; i++) {
			int[] row = arr[i];
			log(row, sep, modeIdx, false);
		}
	}
	
	public static void log(long[][] arr, String sep) {
		log(arr, sep, Constants.OUT_IDX);
	}
	public static void log(long[][] arr) {
		log(arr, DEFAULT_SEP_WHILE_TWO_DIMEN);
	}
	public static void log(long[][] arr, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		for(int i=0; i<arr.length; i++) {
			long[] row = arr[i];
			log(row, sep, modeIdx, false);
		}
	}
	
	public static void log(double[][] arr, String sep) {
		log(arr, sep, Constants.OUT_IDX);
	}
	public static void log(double[][] arr) {
		log(arr, DEFAULT_SEP_WHILE_TWO_DIMEN);
	}
	public static void log(double[][] arr, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		for(int i=0; i<arr.length; i++) {
			double[] row = arr[i];
			log(row, sep, modeIdx, false);
		}
	}
	
	public static void log(char[][] arr, String sep) {
		log(arr, sep, Constants.OUT_IDX);
	}
	public static void log(char[][] arr) {
		log(arr, DEFAULT_SEP_WHILE_TWO_DIMEN);
	}
	public static void log(char[][] arr, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		for(int i=0; i<arr.length; i++) {
			char[] row = arr[i];
			log(row, sep, modeIdx, false);
		}
	}
	
	public static void log(byte[][] arr, String sep) {
		log(arr, sep, Constants.OUT_IDX);
	}
	public static void log(byte[][] arr) {
		log(arr, DEFAULT_SEP_WHILE_TWO_DIMEN);
	}
	public static void log(byte[][] arr, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		for(int i=0; i<arr.length; i++) {
			byte[] row = arr[i];
			log(row, sep, modeIdx, false);
		}
	}
	
	public static void log(boolean[][] arr, String sep) {
		log(arr, sep, Constants.OUT_IDX);
	}
	public static void log(boolean[][] arr) {
		log(arr, DEFAULT_SEP_WHILE_TWO_DIMEN);
	}
	public static void log(boolean[][] arr, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		for(int i=0; i<arr.length; i++) {
			boolean[] row = arr[i];
			log(row, sep, modeIdx, false);
		}
	}
	
	// fix bug 'log(arr, sep, true)' -> 'log(arr, sep, Constants.OUT_IDX)'	add at 2016.05.14
	public static <T> void log(T[][] arr, String sep) {
		log(arr, sep, Constants.OUT_IDX);
	}
	public static <T> void log(T[][] arr) {
		log(arr, DEFAULT_SEP_WHILE_TWO_DIMEN);
	}
	public static <T> void log(T[][] arr, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		for(int i=0; i<arr.length; i++) {
			Object[] row = arr[i];
			log(row, sep, modeIdx, false);
		}
	}
	
	// 按照给定的iterator迭代出来的索引, 打印arr中的元素
	// int -> long -> char -> byte -> boolean -> T
	public static void log(int[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.OUT_IDX);
	}
	public static void log(int[] arr, Iterator<Integer> it) {
		log(arr, it, DEFAULT_SEP_WHILE_NO_CRLF);
	}
	public static void log(int[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		Tools.assert0(it != null, "'Iterator' is null ");
		
		StringBuilder sb = new StringBuilder();
		while(it.hasNext()) {
			Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
		}
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		sb.append(Tools.CRLF);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public static void log(long[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.OUT_IDX);
	}
	public static void log(long[] arr, Iterator<Integer> it) {
		log(arr, it, DEFAULT_SEP_WHILE_NO_CRLF);
	}
	public static void log(long[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		Tools.assert0(it != null, "'Iterator' is null ");
		
		StringBuilder sb = new StringBuilder();
		while(it.hasNext()) {
			Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
		}
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		sb.append(Tools.CRLF);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public static void log(double[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.OUT_IDX);
	}
	public static void log(double[] arr, Iterator<Integer> it) {
		log(arr, it, DEFAULT_SEP_WHILE_NO_CRLF);
	}
	public static void log(double[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		Tools.assert0(it != null, "'Iterator' is null ");
		
		StringBuilder sb = new StringBuilder();
		while(it.hasNext()) {
			Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
		}
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		sb.append(Tools.CRLF);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}

	public static void log(char[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.OUT_IDX);
	}
	public static void log(char[] arr, Iterator<Integer> it) {
		log(arr, it, DEFAULT_SEP_WHILE_NO_CRLF);
	}
	public static void log(char[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		Tools.assert0(it != null, "'Iterator' is null ");
		
		StringBuilder sb = new StringBuilder();
		while(it.hasNext()) {
			Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
		}
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		sb.append(Tools.CRLF);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public static void log(byte[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.OUT_IDX);
	}
	public static void log(byte[] arr, Iterator<Integer> it) {
		log(arr, it, DEFAULT_SEP_WHILE_NO_CRLF);
	}
	public static void log(byte[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		Tools.assert0(it != null, "'Iterator' is null ");
		
		StringBuilder sb = new StringBuilder();
		while(it.hasNext()) {
			Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
		}
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		sb.append(Tools.CRLF);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public static void log(boolean[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.OUT_IDX);
	}
	public static void log(boolean[] arr, Iterator<Integer> it) {
		log(arr, it, DEFAULT_SEP_WHILE_NO_CRLF);
	}
	public static void log(boolean[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		Tools.assert0(it != null, "'Iterator' is null ");
		
		StringBuilder sb = new StringBuilder();
		while(it.hasNext()) {
			Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
		}
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		sb.append(Tools.CRLF);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public static <T> void log(T[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.OUT_IDX);
	}
	public static <T> void log(T[] arr, Iterator<Integer> it) {
		log(arr, it, DEFAULT_SEP_WHILE_NO_CRLF);
	}
	public static <T> void log(T[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		Tools.assert0(it != null, "'Iterator' is null ");
		
		StringBuilder sb = new StringBuilder();
		while(it.hasNext()) {
			Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
		}
		if(sb.length() > sep.length() ) {
			sb.delete(sb.length()-sep.length(), sb.length() );
		}
		sb.append(Tools.CRLF);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	// 打印两个int, long, double, boolean, Object
	// int -> long -> char -> byte -> boolean -> T
	public static void log(int row, int col) {
		log(row + DEFAULT_SEP_WHILE_NO_CRLF + col);
	}
	public static void log(long row, long col) {
		log(row + DEFAULT_SEP_WHILE_NO_CRLF + col);
	}
	public static void log(double row, double col) {
		log(row + DEFAULT_SEP_WHILE_NO_CRLF + col);
	}
	public static void log(char row, char col) {
		log(row + DEFAULT_SEP_WHILE_NO_CRLF + col);
	}
	public static void log(byte row, byte col) {
		log(row + DEFAULT_SEP_WHILE_NO_CRLF + col);
	}
	public static void log(boolean bool01, boolean bool02) {
		log(String.valueOf(bool01) + DEFAULT_SEP_WHILE_NO_CRLF + String.valueOf(bool02) );
	}
	public static <T1, T2> void log(T1 row, T2 col) {
		log(row.toString() + DEFAULT_SEP_WHILE_NO_CRLF + col.toString() );
	}
	
	// 打印一条水平线
	public static void logHorizon(int n) {
		logHorizon(n, Constants.OUT_IDX);
	}
	public static void logHorizon() {
		logHorizon(1);
	}
	public static void logHorizon(int n, int modeIdx) {
		StringBuilder sb = new StringBuilder(n * (HORIZON_LINES.length() + 2) );
		for(int i=0; i<n; i++) {
			Tools.appendCRLF(sb, HORIZON_LINES);
		}
		if(sb.length() > Tools.CRLF.length() ) {
			sb.delete(sb.length()-Tools.CRLF.length(), sb.length() );
		}
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	// 键入一个/ n个回车
	public static void logEnter() {
		logEnter(1);
	}
	public static void logEnter(int n) {
		logEnter(n, Constants.OUT_IDX);
	}
	public static void logEnter(int n, int modeIdx) {
		StringBuilder sb = new StringBuilder();
		for(int i=0; i<n; i++) {
			sb.append(Tools.CRLF);
		}
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	// 打印自定义的主题
	public static void logForPage(String page) {
		logFor(page, "page", HORIZON_LINES, HORIZON_LINES, Constants.OUT_IDX);
	}
	public static void logForThemes(String theme) {
		logFor(theme, "theme", HORIZON_STARTS, HORIZON_STARTS, Constants.OUT_IDX);
	}
	public static void logForPage(Object page) {
		logFor(String.valueOf(page), "page", HORIZON_LINES, HORIZON_LINES, Constants.OUT_IDX);
	}
	public static void logForThemes(Object theme) {
		logFor(String.valueOf(theme), "theme", HORIZON_STARTS, HORIZON_STARTS, Constants.OUT_IDX);
	}
	public static void logFor(String subject, String subjectKey, String before, String after, int modeIdx) {
		String logStr = String.valueOf(before) + " [ " + String.valueOf(subjectKey) + " : " + String.valueOf(subject) + " ] " + String.valueOf(after);
		dispathLogInfo(modeIdx, logStr);
	}
	
	// ----------------------------- seps ----------------------------------------
	
	// 错误输出
	public static void err(boolean appendCRLF) {
		err(GOT_THERE, appendCRLF);
	}
	public static void err() {
		err(GOT_THERE, ERRPUT_APPEND_CRLF);
	}
	public static void err(String str, boolean appendCRLF) {
		log(str, appendCRLF, Constants.ERR_IDX);
	}
	public static void err(String obj) {
		err(obj, ERRPUT_APPEND_CRLF);
	}
	public static void err(Object obj, boolean appendCRLF) {
		err(String.valueOf(obj), appendCRLF );
	}
	public static void err(Object obj) {
		err(obj, ERRPUT_APPEND_CRLF );
	}
	public static void errf(String pattern, Object[] args, boolean appendCRLF) {
		err(String.format(pattern, args), appendCRLF);
	}
	public static void errf(String pattern, Object... args) {
		errf(pattern, args, Constants.OUTPUT_APPEND_CRLF);
	}
	
	public static <T> void err(Iterator<T> it, String sep, boolean appendCRLF) {
		log(it, sep, Constants.ERR_IDX, appendCRLF);
	}
	public static <T> void err(Iterator<T> it, boolean appendCRLF) {
		if(appendCRLF) {
			err(it, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			err(it, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static <T> void err(Iterator<T> it) {
		err(it, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static <T> void err(Iterator<T> it, String sep) {
		err(it, sep, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	
	public static <T> void err(List<T> list, String sep, boolean appendCRLF) {
		Tools.assert0(list != null, "'list' is null ");
		err(list.iterator(), sep, appendCRLF);
	}
	public static <T> void err(List<T> list, boolean appendCRLF) {
		Tools.assert0(list != null, "'list' is null ");
		err(list.iterator(), appendCRLF);
	}
	public static <T> void err(List<T> list) {
		Tools.assert0(list != null, "'list' is null ");
		err(list.iterator(), ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static <T> void err(List<T> list, String sep) {
		Tools.assert0(list != null, "'list' is null ");
		err(list.iterator(), sep, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	
	public static <T> void err(Set<T> set, String sep, boolean appendCRLF) {
		Tools.assert0(set != null, "'set' is null ");
		err(set.iterator(), sep, appendCRLF);
	}
	public static <T> void err(Set<T> set, boolean appendCRLF) {
		Tools.assert0(set != null, "'set' is null ");
		err(set.iterator(), appendCRLF);
	}
	public static <T> void err(Set<T> set) {
		Tools.assert0(set != null, "'set' is null ");
		err(set.iterator(), ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static <T> void err(Set<T> set, String sep) {
		Tools.assert0(set != null, "'set' is null ");
		err(set.iterator(), sep, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	
	public static <K, V> void err(Map<K, V> map, String kvSep, String sep, boolean appendCRLF) {
		log(map, kvSep, sep, Constants.ERR_IDX, appendCRLF);
	}
	public static <K, V> void err(Map<K, V> map, String kvSep, String sep) {
		err(map, kvSep, sep, true);
	}
	public static <K, V> void err(Map<K, V> map, String sep, boolean appendCRLF) {
		log(map, DEFAULT_MAP_KV_SEP, sep, Constants.ERR_IDX, appendCRLF);
	}
	public static <K, V> void err(Map<K, V> map, boolean appendCRLF) {
		if(appendCRLF) {
			err(map, DEFAULT_MAP_KV_SEP, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			err(map, DEFAULT_MAP_KV_SEP, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static <K, V> void err(Map<K, V> map) {
		err(map, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static <K, V> void err(Map<K, V> map, String sep) {
		err(map, sep, Constants.OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	
	public static void err(int[] arr, String sep, boolean appendCRLF) {
		log(arr, sep, Constants.ERR_IDX, appendCRLF);
	}
	public static void err(int[] arr, boolean appendCRLF) {
		if(appendCRLF) {
			err(arr, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			err(arr, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static void err(int[] arr) {
		err(arr, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void err(int[] arr, String sep) {
		err(arr, sep, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	
	public static void err(long[] arr, String sep, boolean appendCRLF) {
		log(arr, sep, Constants.ERR_IDX, appendCRLF);
	}
	public static void err(long[] arr, boolean appendCRLF) {
		if(appendCRLF) {
			err(arr, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			err(arr, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static void err(long[] arr) {
		err(arr, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void err(long[] arr, String sep) {
		err(arr, sep, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	
	public static void err(double[] arr, String sep, boolean appendCRLF) {
		log(arr, sep, Constants.ERR_IDX, appendCRLF);
	}
	public static void err(double[] arr, boolean appendCRLF) {
		if(appendCRLF) {
			err(arr, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			err(arr, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static void err(double[] arr) {
		err(arr, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void err(double[] arr, String sep) {
		err(arr, sep, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	
	public static void err(byte[] arr, String sep, boolean appendCRLF) {
		log(arr, sep, Constants.ERR_IDX, appendCRLF);
	}
	public static void err(byte[] arr, boolean appendCRLF) {
		if(appendCRLF) {
			err(arr, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			err(arr, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static void err(byte[] arr) {
		err(arr, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void err(byte[] arr, String sep) {
		err(arr, sep, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	
	public static void err(char[] arr, String sep, boolean appendCRLF) {
		log(arr, sep, Constants.ERR_IDX, appendCRLF);
	}
	public static void err(char[] arr, boolean appendCRLF) {
		if(appendCRLF) {
			err(arr, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			err(arr, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static void err(char[] arr) {
		err(arr, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void err(char[] arr, String sep) {
		err(arr, sep, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	
	public static void err(boolean[] arr, String sep, boolean appendCRLF) {
		log(arr, sep, Constants.ERR_IDX, appendCRLF);
	}
	public static void err(boolean[] arr, boolean appendCRLF) {
		if(appendCRLF) {
			err(arr, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			err(arr, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static void err(boolean[] arr) {
		err(arr, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static void err(boolean[] arr, String sep) {
		err(arr, sep, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	
	public static <T> void err(T[] arr, String sep, boolean appendCRLF) {
		log(arr, sep, Constants.ERR_IDX, appendCRLF);
	}
	public static <T> void err(T[] arr, boolean appendCRLF) {
		if(appendCRLF) {
			err(arr, DEFAULT_SEP_WHILE_CRLF, appendCRLF);
		} else {
			err(arr, DEFAULT_SEP_WHILE_NO_CRLF, appendCRLF);
		}
	}
	public static <T> void err(T[] arr) {
		err(arr, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	public static <T> void err(T[] arr, String sep) {
		err(arr, sep, ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	}
	
	public static void err(int[][] arr, String sep) {
		log(arr, sep, Constants.ERR_IDX);
	}
	public static void err(int[][] arr) {
		err(arr, DEFAULT_SEP_WHILE_TWO_DIMEN);
	}

	public static void err(long[][] arr, String sep) {
		log(arr, sep, Constants.ERR_IDX);
	}
	public static void err(long[][] arr) {
		err(arr, DEFAULT_SEP_WHILE_TWO_DIMEN);
	}

	public static void err(double[][] arr, String sep) {
		log(arr, sep, Constants.ERR_IDX);
	}
	public static void err(double[][] arr) {
		err(arr, DEFAULT_SEP_WHILE_TWO_DIMEN);
	}
	
	public static void err(byte[][] arr, String sep) {
		log(arr, sep, Constants.ERR_IDX);
	}
	public static void err(byte[][] arr) {
		err(arr, DEFAULT_SEP_WHILE_TWO_DIMEN);
	}
	
	public static void err(char[][] arr, String sep) {
		log(arr, sep, Constants.ERR_IDX);
	}
	public static void err(char[][] arr) {
		err(arr, DEFAULT_SEP_WHILE_TWO_DIMEN);
	}
	
	public static void err(boolean[][] arr, String sep) {
		log(arr, sep, Constants.ERR_IDX);
	}
	public static void err(boolean[][] arr) {
		err(arr, DEFAULT_SEP_WHILE_TWO_DIMEN);
	}
	
	public static <T> void err(T[][] arr, String sep) {
		log(arr, sep, Constants.ERR_IDX);
	}
	public static <T> void err(T[][] arr) {
		err(arr, DEFAULT_SEP_WHILE_TWO_DIMEN);
	}
	
	public static void err(int[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.ERR_IDX);
	}
	public static void err(int[] arr, Iterator<Integer> it) {
		err(arr, it, DEFAULT_SEP_WHILE_NO_CRLF);
	}
	
	public static void err(long[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.ERR_IDX);
	}
	public static void err(long[] arr, Iterator<Integer> it) {
		err(arr, it, DEFAULT_SEP_WHILE_NO_CRLF);
	}
	
	public static void err(double[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.ERR_IDX);
	}
	public static void err(double[] arr, Iterator<Integer> it) {
		err(arr, it, DEFAULT_SEP_WHILE_NO_CRLF);
	}
	
	public static void err(char[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.ERR_IDX);
	}
	public static void err(char[] arr, Iterator<Integer> it) {
		err(arr, it, DEFAULT_SEP_WHILE_NO_CRLF);
	}
	
	public static void err(boolean[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.ERR_IDX);
	}
	public static void err(boolean[] arr, Iterator<Integer> it) {
		err(arr, it, DEFAULT_SEP_WHILE_NO_CRLF);
	}
	
	public static <T> void err(T[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.ERR_IDX);
	}
	public static <T> void err(T[] arr, Iterator<Integer> it) {
		err(arr, it, DEFAULT_SEP_WHILE_NO_CRLF);
	}

	public static void err(int row, int col) {
		err(row + DEFAULT_SEP_WHILE_NO_CRLF + col);
	}
	public static void err(long row, long col) {
		err(row + DEFAULT_SEP_WHILE_NO_CRLF + col);
	}
	public static void err(double row, double col) {
		err(row + DEFAULT_SEP_WHILE_NO_CRLF + col);
	}
	public static void err(char row, char col) {
		err(row + DEFAULT_SEP_WHILE_NO_CRLF + col);
	}
	public static void err(byte row, byte col) {
		err(row + DEFAULT_SEP_WHILE_NO_CRLF + col);
	}
	public static void err(boolean bool01, boolean bool02) {
		err(String.valueOf(bool01) + DEFAULT_SEP_WHILE_NO_CRLF + String.valueOf(bool02) );
	}
	public static <T1, T2> void err(T1 row, T2 col) {
		err(row.toString() + DEFAULT_SEP_WHILE_NO_CRLF + col.toString() );
	}
	
	public static void errHorizon(int n) {
		logHorizon(n, Constants.ERR_IDX);
	}
	public static void errHorizon() {
		errHorizon(1);
	}

	public static void errEnter() {
		errEnter(1);
	}
	public static void errEnter(int n) {
		logEnter(n, Constants.ERR_IDX);
	}
	
	public static void errForPage(String page) {
		logFor(page, "page", HORIZON_LINES, HORIZON_LINES, Constants.ERR_IDX);
	}
	public static void errForThemes(String theme) {
		logFor(theme, "theme", HORIZON_STARTS, HORIZON_STARTS, Constants.ERR_IDX);
	}
	public static void errForPage(Object page) {
		logFor(String.valueOf(page), "page", HORIZON_LINES, HORIZON_LINES, Constants.ERR_IDX);
	}
	public static void errForThemes(Object theme) {
		logFor(String.valueOf(theme), "theme", HORIZON_STARTS, HORIZON_STARTS, Constants.ERR_IDX);
	}
	
	
	// 刷出缓冲区的数据		add at 2016.04.15
	public static void flush() {
		try {
			Set<String> flushed = new HashSet<>();
			for(int i=0; i<Constants.LOG_MODES.length; i++) {
				if(outToLogFile[i] && (! flushed.contains(logBufNames[i])) ) {
					Tools.flushBuffer(logBufNames[i]);
					flushed.add(logBufNames[i]);
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	// ------------ 待续 --------------------
	
	
	

	
}
