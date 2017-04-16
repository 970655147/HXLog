/**
 * file name : Logger.java
 * created at : 9:34:52 PM May 30, 2016
 * created by 970655147
 */

package com.hx.log.log;

import java.io.OutputStream;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.hx.log.idx.IdxGenerator;

import java.util.Set;

import com.hx.log.log.log_pattern.LogPatternChain;
import com.hx.log.util.Constants;
import com.hx.log.util.Tools;
import com.hx.json.JSONObject;

public class Logger {
	
	// IdxGenerator
	private static IdxGenerator IDX_GENERATOR = new IdxGenerator();
	public static final String BUFF_NAME_PREFIX = Constants.optString(Constants._BUFF_NAME_PREFIX);
	public static final String BUFF_NAME_SEP = Constants.optString(Constants._BUFF_NAME_SEP); 
	
	// --------------------------- 可配置变量 --------------------------------------
	// 以及输出流, 错误流, 以及默认是否换行
	public String horizonLines = Constants.optString(Constants._HORIZON_LINES);
	public String horizonStars = Constants.optString(Constants._HORIZON_STARS);
	public String gotThere = Constants.optString(Constants._GOT_THERE);
	public String gotNothing = Constants.optString(Constants._GOT_NOTHING);
	
	public final int loggerId = IDX_GENERATOR.nextId();
	public final String loggerIdx = Constants.LOG_IDX_HANDLER_PARSER.handle(String.valueOf(loggerId) );
	public OutputStream[] outStreams = Arrays.copyOf(Constants.OUT_STREAMS, Constants.OUT_STREAMS.length);
	private final boolean[] outToLogFile = Arrays.copyOf(Constants.OUT_TO_LOG_FILES, Constants.OUT_TO_LOG_FILES.length );
	private final String[] logBuffNames = new String[Constants.LOG_BUFF_SIFFIXES.length];
	{
		for(int i=0; i<logBuffNames.length; i++) {
			logBuffNames[i] = genLogBuffNames(Constants.LOG_BUFF_SIFFIXES[i]);
		}
	}
	private String[] logFiles = Arrays.copyOf(Constants.LOG_FILES, Constants.LOG_FILES.length);
	private String[] logModes = Arrays.copyOf(Constants.LOG_MODES, Constants.LOG_MODES.length);
	public LogPatternChain logPatternChain = Constants.LOG_PATTERN.copyOf();
	
	public String defaultSepWhileCRLF = Constants.optString(Constants._DEFAULT_SEP_WHILE_CRLF);
	public String defaultSepWhileNotCrlf = Constants.optString(Constants._DEFAULT_SEP_WHILE_NOT_CRLF);
	public String defaultSepWhileTwoDimen = Constants.optString(Constants._DEFAULT_SEP_WHILE_TWO_DIMEN);
	public String defaultSepWhileMapKV = Constants.optString(Constants._DEFAULT_SEP_MAP_KVSEP);
	
	public boolean outputAppendCrlf = Constants.optBoolean(Constants._DEFAULT_OUTPUT_APPEND_CRLF);
	public boolean errputAppendCrlf = Constants.optBoolean(Constants._DEFAULT_ERRPUT_APPEND_CRLF);
	public boolean outputAppendCrlfForContainer = Constants.optBoolean(Constants._DEFAULT_OUTPUT_APPEND_CRLF_FOR_CONTAINER);
	public boolean errputAppendCrlfForContainer = Constants.optBoolean(Constants._DEFAULT_ERRPUT_APPEND_CRLF_FOR_CONTAINER);
	public boolean outputAppendCrlfForFormat = Constants.optBoolean(Constants._DEFAULT_OUTPUT_APPEND_CRLF_FOR_FORMAT);
	public boolean errputAppendCrlfForFormat = Constants.optBoolean(Constants._DEFAULT_ERRPUT_APPEND_CRLF_FOR_FORMAT);
	public boolean isFormat = Constants.optBoolean(Constants._DEFAULT_IS_FORMAT);
	// --------------------------- 置于最后 ----------------------------------------
	
	// 初始化
	{
		try {
			for(int i=0; i<logModes.length; i++) {
				if(outToLogFile[i]) {
					setLogFile0(logFiles[i], i);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	// --------------------------- 配置可配置变量的接口 ----------------------------------------
	public void setOutLogFile(String logFile) throws Exception {
		setLogFile0(logFile, Constants.OUT_IDX);
	}
	public void setErrLogFile(String logFile) throws Exception {
		setLogFile0(logFile, Constants.ERR_IDX);
	}
	// add at 2016.10.15
	public void setOutMode(String mode) {
		logModes[Constants.OUT_IDX] = mode;
	}
	public void setErrMode(String mode) {
		logModes[Constants.ERR_IDX] = mode;
	}
	public void setOutStream(OutputStream stream) {
		outStreams[Constants.OUT_IDX] = stream;
	}
	public void setErrStream(OutputStream stream) {
		outStreams[Constants.ERR_IDX] = stream;
	}
	public void setOutToLogFile(boolean outToLogFile, String logFile) throws Exception {
		setToLogFile0(outToLogFile, logFile, Constants.OUT_IDX);
	}
	public void setOutToLogFile(boolean outToLogFile) throws Exception {
		setOutToLogFile(outToLogFile, logFiles[Constants.OUT_IDX]);
	}
	public void setErrToLogFile(boolean errToLogFile, String logFile) throws Exception {
		setToLogFile0(errToLogFile, logFile, Constants.ERR_IDX);
	}
	public void setErrToLogFile(boolean errToLogFile) throws Exception {
		setErrToLogFile(errToLogFile, logFiles[Constants.ERR_IDX]);
	}
	
	// 辅助方法
	private void setLogFile0(String logFile, int modeIdx) throws Exception {
		if(logFiles[modeIdx] != null ) {
			setLogFile00(logFile, modeIdx);
		} else {
			log(logModes[modeIdx] + "'s outputFile is null, maybe not support out log to 'logFile', use 'setXXXToLogFile' insted !");
		}
	}
	private void setLogFile00(String logFile, int modeIdx) throws Exception {
		// 和当前流输出相同的第一个流的索引[校验是否需要关流, 以及更新logBufName]
		int firstNonMeSameBuffIdx = -1;
		for(int i=0; i<logModes.length; i++) {
			if(i == modeIdx) continue ;
			if(outToLogFile[i] && (logFiles[modeIdx].equals(logFiles[i])) ) {
				firstNonMeSameBuffIdx = i;
				break ;
			}
		}
		
		// 如果是设置logFile为空, 则校验是否需要关掉缓冲, 如果需要则关掉缓冲
		if(logFile == null) {
			if((firstNonMeSameBuffIdx < 0) && Tools.bufExists(logBuffNames[modeIdx]) ) {
				Tools.closeAnBuffer(logBuffNames[modeIdx]);
			}
			outToLogFile[modeIdx] = false;
			logFiles[modeIdx] = null;
			return ;
		}
		
		// 和logFile相同的第一个非当前流的索引[校验是否需要创建新的缓冲]
		int sameBufIdx = -1;
		for(int i=0; i<logModes.length; i++) {
			if(i == modeIdx) continue ;
			if(logFile.equals(logFiles[i])) {
				sameBufIdx = i;
				break ;
			}
		}
		boolean needCreateNewBuf = sameBufIdx < 0;
		String oldBufName = logBuffNames[modeIdx];
		// 如果需要创建新的buff, 则更新logBufNames[modeIdx]
		// 如果logBufNames[modeIdx] 和Constants.logBufNames[modeIdx]相同, 表示有其他的流关联在logBufNames[modeIdx]上面, 更新其他的流的
		if(needCreateNewBuf) {
			// 更新可能存在的多个关联在同一个缓冲上面的其他缓冲的key[散] 
			if(logBuffNames[modeIdx].equals(genLogBuffNames(Constants.LOG_BUFF_SIFFIXES[modeIdx])) ) {
				if(firstNonMeSameBuffIdx >= 0) {
					for(int i=firstNonMeSameBuffIdx; i<logModes.length; i++) {
						if(logBuffNames[i].equals(logBuffNames[modeIdx]) ) {
							logBuffNames[i] = logBuffNames[firstNonMeSameBuffIdx];
						}
					}
					// 关闭原来的缓冲[由之后的createBuffer创建], 然后为关联在当前流的其他流创建新的缓冲[暂不考虑并发情况]
					Tools.closeAnBuffer(logBuffNames[modeIdx]);
					Tools.createAnBuffer(genLogBuffNames(Constants.LOG_BUFF_SIFFIXES[firstNonMeSameBuffIdx]), logBuffNames[firstNonMeSameBuffIdx]);
				}
			}
			logBuffNames[modeIdx] = genLogBuffNames(Constants.LOG_BUFF_SIFFIXES[modeIdx]);
			// 更新当前缓冲为已经存在的缓冲[聚]
		} else {
			logBuffNames[modeIdx] = logBuffNames[sameBufIdx];
		}
		
		// 如果logFiles[modeIdx]不为空, 并且logFile 与logFiles[modeIdx]不相同, 则根据情况, 创建缓冲
		// 否则, 创建缓冲, 或者doNothing
		if((logFiles[modeIdx] != null) && (! logFiles[modeIdx].equals(logFile)) ) {
//			if((firstNonMeSameBuffIdx < 0) && Tools.bufExists(logBufNames[modeIdx]) ) {
			if((firstNonMeSameBuffIdx < 0) && Tools.bufExists(oldBufName) ) {
				Tools.flushBuffer(oldBufName, true);
			}
			if(needCreateNewBuf) {
				Tools.createAnBuffer(logBuffNames[modeIdx], logFile);
			}
		} else {
			if(! Tools.bufExists(logBuffNames[modeIdx]) ) {
				Tools.createAnBuffer(logBuffNames[modeIdx], logFile);	
			} else {
				log("specified : 'logFile' is currentStartIdx 'Log.logFile', ignore !");
			}
		}
		logFiles[modeIdx] = logFile;
	}
	private void setToLogFile0(boolean toLogFile, String logFile, int modeIdx) throws Exception {
		outToLogFile[modeIdx] = toLogFile;
		if(toLogFile) {
			setLogFile0(logFile, modeIdx);
		}
	}
	
	// add at 2016.05.07
	public void dispathLogInfo(int modeIdx, String logStr) {
		dispathLogInfo(modeIdx, logStr, isFormat);
	}
	public void dispathLogInfo(int modeIdx, String logStr, boolean isFormat) {
		// dispatch
		switch (modeIdx) {
			case Constants.OUT_IDX :
				log(logStr, outputAppendCrlf, isFormat, modeIdx);
				break;
			case Constants.ERR_IDX :
				log(logStr, errputAppendCrlf, isFormat, modeIdx);
				break;
			default:
				err("have no this 'modeIdx', currentStartIdx support " + Constants.LOG_MODES_STR + " ");
				break;
		}
	}
	private String genLogBuffNames(String logBuffSuffix) {
		return BUFF_NAME_PREFIX + BUFF_NAME_SEP + loggerId + BUFF_NAME_SEP + logBuffSuffix;
	}
	
	// --------------------------- 业务方法 ----------------------------------------
	// 标准输出
	// 打印字符串, 对象, 按照给定的pattern填充数据
	public void log(boolean appendCRLF) {
		log(String.valueOf(appendCRLF), outputAppendCrlf);
	}
	public void log() {
		log(gotThere, outputAppendCrlf);
	}
	public void log(String str, boolean appendCRLF, int modeIdx) {
		log(str, appendCRLF, true, modeIdx);
	}
	public void log(String str, boolean appendCRLF, boolean isFormat, int modeIdx) {
//		Tools.assert0(str != null, "'str' is null ");
		str = String.valueOf(str);
		
		// switch of 'isFormat'
		String line = logLogPatternFormat(str, appendCRLF, isFormat, modeIdx);

		// add 'outStreams != null' for rubustness		add at 2016.05.30
		if((outStreams != null) && ((modeIdx < 0) || (modeIdx >= outStreams.length)) ) {
			err("have no this 'modeIdx', currentStartIdx support " + Constants.LOG_MODES_STR + " ");
			return ;
		}
			
		try {
			if((outStreams != null) && (outStreams[modeIdx] != null) ) {
				outStreams[modeIdx].write(line.getBytes(Tools.DEFAULT_CHARSET) );
			}
			if(outToLogFile[modeIdx]) {
				Tools.appendBuffer(logBuffNames[modeIdx], line);
			}
		} catch (Exception e) {
			Tools.assert0(Tools.errorMsg(e) );
		}
	}
	public void log(String str, boolean appendCRLF) {
		log(str, appendCRLF, Constants.OUT_IDX);
	}
	public void log(String obj) {
		log(obj, outputAppendCrlf);
	}
	public void log(Object obj, boolean appendCRLF) {
		log(String.valueOf(obj), appendCRLF );
	}
	public void log(Object obj) {
		log(obj, outputAppendCrlf);
	}
	public void logf(String pattern, Object[] args, boolean appendCRLF) {
		log(String.format(pattern, args), appendCRLF);
	}
	public void logf(String pattern, Object... args) {
		logf(pattern, args, outputAppendCrlf);
	}
	
	// 格式化需要打印的数据
	public String logLogPatternFormat(String content, boolean appendCRLF, boolean isFormat, int modeIdx) {
		StringBuilder sb = new StringBuilder(content.length() + 4);
		if(isFormat) {
			sb.append(
					LogPatternUtils.formatLogInfo(logPatternChain, new JSONObject().element(Constants.LOG_PATTERN_MSG, content)
						.element(Constants.LOG_PATTERN_MODE, logModes[Tools.getIdx(modeIdx, logModes.length, Constants.ERR_IDX)])
						.element(Constants.LOG_PATTERN_LOG_IDX, loggerIdx)
					) );
		} else {
			sb.append(content);
		}
		
		if(appendCRLF) {
			sb.append(Constants.CRLF );
		}
		return sb.toString();
	}
	public String logLogPatternFormat(String content, boolean appendCRLF) {
		return logLogPatternFormat(content, appendCRLF, isFormat, Constants.OUT_IDX);
	}
	public String logLogPatternFormat(String content) {
		return logLogPatternFormat(content, outputAppendCrlfForFormat);
	}
	
	// 打印迭代器中的数据
	public <T> void log(Iterator<T> it, String sep, boolean appendCRLF) {
		log(it, sep, Constants.OUT_IDX, appendCRLF);
	}
	public <T> void log(Iterator<T> it, boolean appendCRLF) {
		if(appendCRLF) {
			log(it, defaultSepWhileCRLF, appendCRLF);
		} else {
			log(it, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public <T> void log(Iterator<T> it) {
		log(it, outputAppendCrlfForContainer);
	}
	public <T> void log(Iterator<T> it, String sep) {
		log(it, sep, outputAppendCrlfForContainer);
	}
	public <T> void log(Iterator<T> it, String sep, int modeIdx, boolean appendCRLF) {
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
		Tools.removeLastSep(sb, sep);
		
		// dispatch
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	// 打印List
	public <T> void log(List<T> list, String sep, boolean appendCRLF) {
		Tools.assert0(list != null, "'list' is null ");
		log(list.iterator(), sep, appendCRLF);
	}
	public <T> void log(List<T> list, boolean appendCRLF) {
		Tools.assert0(list != null, "'list' is null ");
		log(list.iterator(), appendCRLF);
	}
	public <T> void log(List<T> list) {
		Tools.assert0(list != null, "'list' is null ");
		log(list.iterator(), outputAppendCrlfForContainer);
	}
	public <T> void log(List<T> list, String sep) {
		Tools.assert0(list != null, "'list' is null ");
		log(list.iterator(), sep, outputAppendCrlfForContainer);
	}
	
	// 打印Set
	public <T> void log(Set<T> set, String sep, boolean appendCRLF) {
		Tools.assert0(set != null, "'set' is null ");
		log(set.iterator(), sep, appendCRLF);
	}
	public <T> void log(Set<T> set, boolean appendCRLF) {
		Tools.assert0(set != null, "'set' is null ");
		log(set.iterator(), appendCRLF);
	}
	public <T> void log(Set<T> set) {
		Tools.assert0(set != null, "'set' is null ");
		log(set.iterator(), outputAppendCrlfForContainer);
	}
	public <T> void log(Set<T> set, String sep) {
		Tools.assert0(set != null, "'set' is null ");
		log(set.iterator(), sep, outputAppendCrlfForContainer);
	}
	
	// 打印Map
	public <K, V> void log(Map<K, V> map, String sep, int modeIdx, boolean appendCRLF) {
		log(map, defaultSepWhileMapKV, sep, modeIdx, appendCRLF);
	}
	public <K, V> void log(Map<K, V> map, String kvSep, String sep, boolean appendCRLF) {
		log(map, kvSep, sep, Constants.OUT_IDX, appendCRLF);
	}
	public <K, V> void log(Map<K, V> map, String kvSep, String sep) {
		log(map, kvSep, sep, true);
	}
	public <K, V> void log(Map<K, V> map, String sep, boolean appendCRLF) {
		log(map, defaultSepWhileMapKV, sep, Constants.OUT_IDX, appendCRLF);
	}
	public <K, V> void log(Map<K, V> map, boolean appendCRLF) {
		if(appendCRLF) {
			log(map, defaultSepWhileMapKV, defaultSepWhileCRLF, appendCRLF);
		} else {
			log(map, defaultSepWhileMapKV, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public <K, V> void log(Map<K, V> map) {
		log(map, outputAppendCrlfForContainer);
	}
	public <K, V> void log(Map<K, V> map, String sep) {
		log(map, sep, outputAppendCrlfForContainer);
	}
	public <K, V> void log(Map<K, V> map, String kvSep, String sep, int modeIdx, boolean appendCRLF) {
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
		Tools.removeLastSep(sb, sep);
		
		// dispatch
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	// 打印int[], long[], double[], char[], byte[], boolean[], Object[]
	public void log(int[] ls, String sep, boolean appendCRLF) {
		log(ls, sep, Constants.OUT_IDX, appendCRLF);
	}
	public void log(int[] ls, boolean appendCRLF) {
		if(appendCRLF) {
			log(ls, defaultSepWhileCRLF, appendCRLF);
		} else {
			log(ls, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public void log(int[] ls) {
		log(ls, outputAppendCrlfForContainer);
	}
	public void log(int[] ls, String sep) {
		log(ls, sep, outputAppendCrlfForContainer);
	}
	public void log(int[] arr, String sep, int modeIdx, boolean appendCRLF) {
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
		Tools.removeLastSep(sb, sep);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public void log(long[] ls, String sep, boolean appendCRLF) {
		log(ls, sep, Constants.OUT_IDX, appendCRLF);
	}
	public void log(long[] ls, boolean appendCRLF) {
		if(appendCRLF) {
			log(ls, defaultSepWhileCRLF, appendCRLF);
		} else {
			log(ls, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public void log(long[] ls) {
		log(ls, outputAppendCrlfForContainer);
	}
	public void log(long[] ls, String sep) {
		log(ls, sep, outputAppendCrlfForContainer);
	}
	public void log(long[] arr, String sep, int modeIdx, boolean appendCRLF) {
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
		Tools.removeLastSep(sb, sep);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}

	public void log(double[] ls, String sep, boolean appendCRLF) {
		log(ls, sep, Constants.OUT_IDX, appendCRLF);
	}
	public void log(double[] ls, boolean appendCRLF) {
		if(appendCRLF) {
			log(ls, defaultSepWhileCRLF, appendCRLF);
		} else {
			log(ls, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public void log(double[] ls) {
		log(ls, outputAppendCrlfForContainer);
	}
	public void log(double[] ls, String sep) {
		log(ls, sep, outputAppendCrlfForContainer);
	}
	public void log(double[] arr, String sep, int modeIdx, boolean appendCRLF) {
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
		Tools.removeLastSep(sb, sep);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public void log(char[] ls, String sep, boolean appendCRLF) {
		log(ls, sep, Constants.OUT_IDX, appendCRLF);
	}
	public void log(char[] ls, boolean appendCRLF) {
		if(appendCRLF) {
			log(ls, defaultSepWhileCRLF, appendCRLF);
		} else {
			log(ls, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public void log(char[] ls) {
		log(ls, outputAppendCrlfForContainer);
	}
	public void log(char[] ls, String sep) {
		log(ls, sep, outputAppendCrlfForContainer);
	}
	public void log(char[] arr, String sep, int modeIdx, boolean appendCRLF) {
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
		Tools.removeLastSep(sb, sep);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public void log(byte[] ls, String sep, boolean appendCRLF) {
		log(ls, sep, Constants.OUT_IDX, appendCRLF);
	}
	public void log(byte[] ls, boolean appendCRLF) {
		if(appendCRLF) {
			log(ls, defaultSepWhileCRLF, appendCRLF);
		} else {
			log(ls, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public void log(byte[] ls) {
		log(ls, outputAppendCrlfForContainer);
	}
	public void log(byte[] ls, String sep) {
		log(ls, sep, outputAppendCrlfForContainer);
	}
	public void log(byte[] arr, String sep, int modeIdx, boolean appendCRLF) {
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
		Tools.removeLastSep(sb, sep);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public void log(boolean[] ls, String sep, boolean appendCRLF) {
		log(ls, sep, Constants.OUT_IDX, appendCRLF);
	}
	public void log(boolean[] ls, boolean appendCRLF) {
		if(appendCRLF) {
			log(ls, defaultSepWhileCRLF, appendCRLF);
		} else {
			log(ls, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public void log(boolean[] ls) {
		log(ls, outputAppendCrlfForContainer);
	}
	public void log(boolean[] ls, String sep) {
		log(ls, sep, outputAppendCrlfForContainer);
	}
	public void log(boolean[] arr, String sep, int modeIdx, boolean appendCRLF) {
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
		Tools.removeLastSep(sb, sep);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}	
	
	public <T> void log(T[] ls, String sep, boolean appendCRLF) {
		log(ls, sep, Constants.OUT_IDX, appendCRLF);
	}
	public <T> void log(T[] ls, boolean appendCRLF) {
		if(appendCRLF) {
			log(ls, defaultSepWhileCRLF, appendCRLF);
		} else {
			log(ls, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public <T> void log(T[] ls) {
		log(ls, outputAppendCrlfForContainer);
	}
	public <T> void log(T[] ls, String sep) {
		log(ls, sep, outputAppendCrlfForContainer);
	}
	public <T> void log(T[] arr, String sep, int modeIdx, boolean appendCRLF) {
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
		Tools.removeLastSep(sb, sep);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	// 打印int[][], long[][], double[][], char[][], byte[][], boolean[][], Object[][]  格式如下 
	// 1 2 3 
	// 2 1 3
	// 3 2 1
	// int -> long -> char -> byte -> boolean -> T
	public void log(int[][] arr, String sep) {
		log(arr, sep, Constants.OUT_IDX);
	}
	public void log(int[][] arr) {
		log(arr, defaultSepWhileTwoDimen);
	}
	public void log(int[][] arr, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		for(int i=0; i<arr.length; i++) {
			int[] row = arr[i];
			log(row, sep, modeIdx, false);
		}
	}
	
	public void log(long[][] arr, String sep) {
		log(arr, sep, Constants.OUT_IDX);
	}
	public void log(long[][] arr) {
		log(arr, defaultSepWhileTwoDimen);
	}
	public void log(long[][] arr, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		for(int i=0; i<arr.length; i++) {
			long[] row = arr[i];
			log(row, sep, modeIdx, false);
		}
	}
	
	public void log(double[][] arr, String sep) {
		log(arr, sep, Constants.OUT_IDX);
	}
	public void log(double[][] arr) {
		log(arr, defaultSepWhileTwoDimen);
	}
	public void log(double[][] arr, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		for(int i=0; i<arr.length; i++) {
			double[] row = arr[i];
			log(row, sep, modeIdx, false);
		}
	}
	
	public void log(char[][] arr, String sep) {
		log(arr, sep, Constants.OUT_IDX);
	}
	public void log(char[][] arr) {
		log(arr, defaultSepWhileTwoDimen);
	}
	public void log(char[][] arr, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		for(int i=0; i<arr.length; i++) {
			char[] row = arr[i];
			log(row, sep, modeIdx, false);
		}
	}
	
	public void log(byte[][] arr, String sep) {
		log(arr, sep, Constants.OUT_IDX);
	}
	public void log(byte[][] arr) {
		log(arr, defaultSepWhileTwoDimen);
	}
	public void log(byte[][] arr, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		for(int i=0; i<arr.length; i++) {
			byte[] row = arr[i];
			log(row, sep, modeIdx, false);
		}
	}
	
	public void log(boolean[][] arr, String sep) {
		log(arr, sep, Constants.OUT_IDX);
	}
	public void log(boolean[][] arr) {
		log(arr, defaultSepWhileTwoDimen);
	}
	public void log(boolean[][] arr, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		for(int i=0; i<arr.length; i++) {
			boolean[] row = arr[i];
			log(row, sep, modeIdx, false);
		}
	}
	
	// fix bug 'log(arr, sep, true)' -> 'log(arr, sep, Constants.OUT_IDX)'	add at 2016.05.14
	public <T> void log(T[][] arr, String sep) {
		log(arr, sep, Constants.OUT_IDX);
	}
	public <T> void log(T[][] arr) {
		log(arr, defaultSepWhileTwoDimen);
	}
	public <T> void log(T[][] arr, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		for(int i=0; i<arr.length; i++) {
			Object[] row = arr[i];
			log(row, sep, modeIdx, false);
		}
	}
	
	// 按照给定的iterator迭代出来的索引, 打印arr中的元素
	// int -> long -> char -> byte -> boolean -> T
	public void log(int[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.OUT_IDX);
	}
	public void log(int[] arr, Iterator<Integer> it) {
		log(arr, it, defaultSepWhileNotCrlf);
	}
	public void log(int[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		Tools.assert0(it != null, "'Iterator' is null ");
		
		StringBuilder sb = new StringBuilder();
		while(it.hasNext()) {
			Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
		}
		Tools.removeLastSep(sb, sep);
		sb.append(Tools.CRLF);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public void log(long[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.OUT_IDX);
	}
	public void log(long[] arr, Iterator<Integer> it) {
		log(arr, it, defaultSepWhileNotCrlf);
	}
	public void log(long[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		Tools.assert0(it != null, "'Iterator' is null ");
		
		StringBuilder sb = new StringBuilder();
		while(it.hasNext()) {
			Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
		}
		Tools.removeLastSep(sb, sep);
		sb.append(Tools.CRLF);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public void log(double[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.OUT_IDX);
	}
	public void log(double[] arr, Iterator<Integer> it) {
		log(arr, it, defaultSepWhileNotCrlf);
	}
	public void log(double[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		Tools.assert0(it != null, "'Iterator' is null ");
		
		StringBuilder sb = new StringBuilder();
		while(it.hasNext()) {
			Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
		}
		Tools.removeLastSep(sb, sep);
		sb.append(Tools.CRLF);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}

	public void log(char[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.OUT_IDX);
	}
	public void log(char[] arr, Iterator<Integer> it) {
		log(arr, it, defaultSepWhileNotCrlf);
	}
	public void log(char[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		Tools.assert0(it != null, "'Iterator' is null ");
		
		StringBuilder sb = new StringBuilder();
		while(it.hasNext()) {
			Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
		}
		Tools.removeLastSep(sb, sep);
		sb.append(Tools.CRLF);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public void log(byte[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.OUT_IDX);
	}
	public void log(byte[] arr, Iterator<Integer> it) {
		log(arr, it, defaultSepWhileNotCrlf);
	}
	public void log(byte[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		Tools.assert0(it != null, "'Iterator' is null ");
		
		StringBuilder sb = new StringBuilder();
		while(it.hasNext()) {
			Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
		}
		Tools.removeLastSep(sb, sep);
		sb.append(Tools.CRLF);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public void log(boolean[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.OUT_IDX);
	}
	public void log(boolean[] arr, Iterator<Integer> it) {
		log(arr, it, defaultSepWhileNotCrlf);
	}
	public void log(boolean[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		Tools.assert0(it != null, "'Iterator' is null ");
		
		StringBuilder sb = new StringBuilder();
		while(it.hasNext()) {
			Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
		}
		Tools.removeLastSep(sb, sep);
		sb.append(Tools.CRLF);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	public <T> void log(T[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.OUT_IDX);
	}
	public <T> void log(T[] arr, Iterator<Integer> it) {
		log(arr, it, defaultSepWhileNotCrlf);
	}
	public <T> void log(T[] arr, Iterator<Integer> it, String sep, int modeIdx) {
		Tools.assert0(arr != null, "'arr' is null ");
		Tools.assert0(it != null, "'Iterator' is null ");
		
		StringBuilder sb = new StringBuilder();
		while(it.hasNext()) {
			Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
		}
		Tools.removeLastSep(sb, sep);
		sb.append(Tools.CRLF);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	// 打印两个int, long, double, boolean, Object
	// int -> long -> char -> byte -> boolean -> T
	public void log(int row, int col) {
		log(row + defaultSepWhileNotCrlf + col);
	}
	public void log(long row, long col) {
		log(row + defaultSepWhileNotCrlf + col);
	}
	public void log(double row, double col) {
		log(row + defaultSepWhileNotCrlf + col);
	}
	public void log(char row, char col) {
		log(row + defaultSepWhileNotCrlf + col);
	}
	public void log(byte row, byte col) {
		log(row + defaultSepWhileNotCrlf + col);
	}
	public void log(boolean bool01, boolean bool02) {
		log(String.valueOf(bool01) + defaultSepWhileNotCrlf + String.valueOf(bool02) );
	}
	public <T1, T2> void log(T1 row, T2 col) {
		log(row.toString() + defaultSepWhileNotCrlf + col.toString() );
	}
	
	// 打印一条水平线
	public void logHorizon(int n) {
		logHorizon(n, Constants.OUT_IDX);
	}
	public void logHorizon() {
		logHorizon(1);
	}
	public void logHorizon(int n, int modeIdx) {
		StringBuilder sb = new StringBuilder(n * (horizonLines.length() + 2) );
		for(int i=0; i<n; i++) {
			Tools.appendCRLF(sb, horizonLines);
		}
		Tools.removeLastSep(sb, Tools.CRLF);
		
		dispathLogInfo(modeIdx, sb.toString() );
	}
	
	// 键入一个/ n个回车
	public void logEnter() {
		logEnter(1);
	}
	public void logEnter(int n) {
		logEnter(n, Constants.OUT_IDX);
	}
	public void logEnter(int n, int modeIdx) {
		StringBuilder sb = new StringBuilder();
		for(int i=0; i<n; i++) {
			sb.append(Tools.CRLF);
		}
		
		dispathLogInfo(modeIdx, sb.toString(), false);
	}
	
	// 打印自定义的主题
	public void logForPage(String page) {
		logFor(page, "page", horizonLines, horizonLines, Constants.OUT_IDX);
	}
	public void logForThemes(String theme) {
		logFor(theme, "theme", horizonStars, horizonStars, Constants.OUT_IDX);
	}
	public void logForPage(Object page) {
		logFor(String.valueOf(page), "page", horizonLines, horizonLines, Constants.OUT_IDX);
	}
	public void logForThemes(Object theme) {
		logFor(String.valueOf(theme), "theme", horizonStars, horizonStars, Constants.OUT_IDX);
	}
	public void logFor(String subject, String subjectKey, String before, String after, int modeIdx) {
		String logStr = String.valueOf(before) + " [ " + String.valueOf(subjectKey) + " : " + String.valueOf(subject) + " ] " + String.valueOf(after);
		dispathLogInfo(modeIdx, logStr);
	}
	
	// ----------------------------- seps ----------------------------------------
	
	// 错误输出
	public void err(boolean appendCRLF) {
		err(String.valueOf(appendCRLF), errputAppendCrlf);
	}
	public void err() {
		err(gotThere, errputAppendCrlf);
	}
	public void err(String str, boolean appendCRLF) {
		log(str, appendCRLF, Constants.ERR_IDX);
	}
	public void err(String obj) {
		err(obj, errputAppendCrlf);
	}
	public void err(Object obj, boolean appendCRLF) {
		err(String.valueOf(obj), appendCRLF );
	}
	public void err(Object obj) {
		err(obj, errputAppendCrlf );
	}
	public void errf(String pattern, Object[] args, boolean appendCRLF) {
		err(String.format(pattern, args), appendCRLF);
	}
	public void errf(String pattern, Object... args) {
		errf(pattern, args, errputAppendCrlf);
	}
	
	public String errLogPatternFormat(String content, boolean appendCRLF) {
		return logLogPatternFormat(content, appendCRLF, isFormat, Constants.ERR_IDX);
	}
	public String errLogPatternFormat(String content) {
		return errLogPatternFormat(content, errputAppendCrlfForFormat);
	}
	
	public <T> void err(Iterator<T> it, String sep, boolean appendCRLF) {
		log(it, sep, Constants.ERR_IDX, appendCRLF);
	}
	public <T> void err(Iterator<T> it, boolean appendCRLF) {
		if(appendCRLF) {
			err(it, defaultSepWhileCRLF, appendCRLF);
		} else {
			err(it, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public <T> void err(Iterator<T> it) {
		err(it, errputAppendCrlfForContainer);
	}
	public <T> void err(Iterator<T> it, String sep) {
		err(it, sep, errputAppendCrlfForContainer);
	}
	
	public <T> void err(List<T> list, String sep, boolean appendCRLF) {
		Tools.assert0(list != null, "'list' is null ");
		err(list.iterator(), sep, appendCRLF);
	}
	public <T> void err(List<T> list, boolean appendCRLF) {
		Tools.assert0(list != null, "'list' is null ");
		err(list.iterator(), appendCRLF);
	}
	public <T> void err(List<T> list) {
		Tools.assert0(list != null, "'list' is null ");
		err(list.iterator(), errputAppendCrlfForContainer);
	}
	public <T> void err(List<T> list, String sep) {
		Tools.assert0(list != null, "'list' is null ");
		err(list.iterator(), sep, errputAppendCrlfForContainer);
	}
	
	public <T> void err(Set<T> set, String sep, boolean appendCRLF) {
		Tools.assert0(set != null, "'set' is null ");
		err(set.iterator(), sep, appendCRLF);
	}
	public <T> void err(Set<T> set, boolean appendCRLF) {
		Tools.assert0(set != null, "'set' is null ");
		err(set.iterator(), appendCRLF);
	}
	public <T> void err(Set<T> set) {
		Tools.assert0(set != null, "'set' is null ");
		err(set.iterator(), errputAppendCrlfForContainer);
	}
	public <T> void err(Set<T> set, String sep) {
		Tools.assert0(set != null, "'set' is null ");
		err(set.iterator(), sep, errputAppendCrlfForContainer);
	}
	
	public <K, V> void err(Map<K, V> map, String kvSep, String sep, boolean appendCRLF) {
		log(map, kvSep, sep, Constants.ERR_IDX, appendCRLF);
	}
	public <K, V> void err(Map<K, V> map, String kvSep, String sep) {
		err(map, kvSep, sep, true);
	}
	public <K, V> void err(Map<K, V> map, String sep, boolean appendCRLF) {
		log(map, defaultSepWhileMapKV, sep, Constants.ERR_IDX, appendCRLF);
	}
	public <K, V> void err(Map<K, V> map, boolean appendCRLF) {
		if(appendCRLF) {
			err(map, defaultSepWhileMapKV, defaultSepWhileCRLF, appendCRLF);
		} else {
			err(map, defaultSepWhileMapKV, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public <K, V> void err(Map<K, V> map) {
		err(map, outputAppendCrlfForContainer);
	}
	public <K, V> void err(Map<K, V> map, String sep) {
		err(map, sep, outputAppendCrlfForContainer);
	}
	
	public void err(int[] arr, String sep, boolean appendCRLF) {
		log(arr, sep, Constants.ERR_IDX, appendCRLF);
	}
	public void err(int[] arr, boolean appendCRLF) {
		if(appendCRLF) {
			err(arr, defaultSepWhileCRLF, appendCRLF);
		} else {
			err(arr, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public void err(int[] arr) {
		err(arr, errputAppendCrlfForContainer);
	}
	public void err(int[] arr, String sep) {
		err(arr, sep, errputAppendCrlfForContainer);
	}
	
	public void err(long[] arr, String sep, boolean appendCRLF) {
		log(arr, sep, Constants.ERR_IDX, appendCRLF);
	}
	public void err(long[] arr, boolean appendCRLF) {
		if(appendCRLF) {
			err(arr, defaultSepWhileCRLF, appendCRLF);
		} else {
			err(arr, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public void err(long[] arr) {
		err(arr, errputAppendCrlfForContainer);
	}
	public void err(long[] arr, String sep) {
		err(arr, sep, errputAppendCrlfForContainer);
	}
	
	public void err(double[] arr, String sep, boolean appendCRLF) {
		log(arr, sep, Constants.ERR_IDX, appendCRLF);
	}
	public void err(double[] arr, boolean appendCRLF) {
		if(appendCRLF) {
			err(arr, defaultSepWhileCRLF, appendCRLF);
		} else {
			err(arr, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public void err(double[] arr) {
		err(arr, errputAppendCrlfForContainer);
	}
	public void err(double[] arr, String sep) {
		err(arr, sep, errputAppendCrlfForContainer);
	}
	
	public void err(byte[] arr, String sep, boolean appendCRLF) {
		log(arr, sep, Constants.ERR_IDX, appendCRLF);
	}
	public void err(byte[] arr, boolean appendCRLF) {
		if(appendCRLF) {
			err(arr, defaultSepWhileCRLF, appendCRLF);
		} else {
			err(arr, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public void err(byte[] arr) {
		err(arr, errputAppendCrlfForContainer);
	}
	public void err(byte[] arr, String sep) {
		err(arr, sep, errputAppendCrlfForContainer);
	}
	
	public void err(char[] arr, String sep, boolean appendCRLF) {
		log(arr, sep, Constants.ERR_IDX, appendCRLF);
	}
	public void err(char[] arr, boolean appendCRLF) {
		if(appendCRLF) {
			err(arr, defaultSepWhileCRLF, appendCRLF);
		} else {
			err(arr, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public void err(char[] arr) {
		err(arr, errputAppendCrlfForContainer);
	}
	public void err(char[] arr, String sep) {
		err(arr, sep, errputAppendCrlfForContainer);
	}
	
	public void err(boolean[] arr, String sep, boolean appendCRLF) {
		log(arr, sep, Constants.ERR_IDX, appendCRLF);
	}
	public void err(boolean[] arr, boolean appendCRLF) {
		if(appendCRLF) {
			err(arr, defaultSepWhileCRLF, appendCRLF);
		} else {
			err(arr, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public void err(boolean[] arr) {
		err(arr, errputAppendCrlfForContainer);
	}
	public void err(boolean[] arr, String sep) {
		err(arr, sep, errputAppendCrlfForContainer);
	}
	
	public <T> void err(T[] arr, String sep, boolean appendCRLF) {
		log(arr, sep, Constants.ERR_IDX, appendCRLF);
	}
	public <T> void err(T[] arr, boolean appendCRLF) {
		if(appendCRLF) {
			err(arr, defaultSepWhileCRLF, appendCRLF);
		} else {
			err(arr, defaultSepWhileNotCrlf, appendCRLF);
		}
	}
	public <T> void err(T[] arr) {
		err(arr, errputAppendCrlfForContainer);
	}
	public <T> void err(T[] arr, String sep) {
		err(arr, sep, errputAppendCrlfForContainer);
	}
	
	public void err(int[][] arr, String sep) {
		log(arr, sep, Constants.ERR_IDX);
	}
	public void err(int[][] arr) {
		err(arr, defaultSepWhileTwoDimen);
	}

	public void err(long[][] arr, String sep) {
		log(arr, sep, Constants.ERR_IDX);
	}
	public void err(long[][] arr) {
		err(arr, defaultSepWhileTwoDimen);
	}

	public void err(double[][] arr, String sep) {
		log(arr, sep, Constants.ERR_IDX);
	}
	public void err(double[][] arr) {
		err(arr, defaultSepWhileTwoDimen);
	}
	
	public void err(byte[][] arr, String sep) {
		log(arr, sep, Constants.ERR_IDX);
	}
	public void err(byte[][] arr) {
		err(arr, defaultSepWhileTwoDimen);
	}
	
	public void err(char[][] arr, String sep) {
		log(arr, sep, Constants.ERR_IDX);
	}
	public void err(char[][] arr) {
		err(arr, defaultSepWhileTwoDimen);
	}
	
	public void err(boolean[][] arr, String sep) {
		log(arr, sep, Constants.ERR_IDX);
	}
	public void err(boolean[][] arr) {
		err(arr, defaultSepWhileTwoDimen);
	}
	
	public <T> void err(T[][] arr, String sep) {
		log(arr, sep, Constants.ERR_IDX);
	}
	public <T> void err(T[][] arr) {
		err(arr, defaultSepWhileTwoDimen);
	}
	
	public void err(int[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.ERR_IDX);
	}
	public void err(int[] arr, Iterator<Integer> it) {
		err(arr, it, defaultSepWhileNotCrlf);
	}
	
	public void err(long[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.ERR_IDX);
	}
	public void err(long[] arr, Iterator<Integer> it) {
		err(arr, it, defaultSepWhileNotCrlf);
	}
	
	public void err(double[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.ERR_IDX);
	}
	public void err(double[] arr, Iterator<Integer> it) {
		err(arr, it, defaultSepWhileNotCrlf);
	}
	
	public void err(char[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.ERR_IDX);
	}
	public void err(char[] arr, Iterator<Integer> it) {
		err(arr, it, defaultSepWhileNotCrlf);
	}
	
	public void err(boolean[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.ERR_IDX);
	}
	public void err(boolean[] arr, Iterator<Integer> it) {
		err(arr, it, defaultSepWhileNotCrlf);
	}
	
	public <T> void err(T[] arr, Iterator<Integer> it, String sep) {
		log(arr, it, sep, Constants.ERR_IDX);
	}
	public <T> void err(T[] arr, Iterator<Integer> it) {
		err(arr, it, defaultSepWhileNotCrlf);
	}

	public void err(int row, int col) {
		err(row + defaultSepWhileNotCrlf + col);
	}
	public void err(long row, long col) {
		err(row + defaultSepWhileNotCrlf + col);
	}
	public void err(double row, double col) {
		err(row + defaultSepWhileNotCrlf + col);
	}
	public void err(char row, char col) {
		err(row + defaultSepWhileNotCrlf + col);
	}
	public void err(byte row, byte col) {
		err(row + defaultSepWhileNotCrlf + col);
	}
	public void err(boolean bool01, boolean bool02) {
		err(String.valueOf(bool01) + defaultSepWhileNotCrlf + String.valueOf(bool02) );
	}
	public <T1, T2> void err(T1 row, T2 col) {
		err(row.toString() + defaultSepWhileNotCrlf + col.toString() );
	}
	
	public void errHorizon(int n) {
		logHorizon(n, Constants.ERR_IDX);
	}
	public void errHorizon() {
		errHorizon(1);
	}

	public void errEnter() {
		errEnter(1);
	}
	public void errEnter(int n) {
		logEnter(n, Constants.ERR_IDX);
	}
	
	public void errForPage(String page) {
		logFor(page, "page", horizonLines, horizonLines, Constants.ERR_IDX);
	}
	public void errForThemes(String theme) {
		logFor(theme, "theme", horizonStars, horizonStars, Constants.ERR_IDX);
	}
	public void errForPage(Object page) {
		logFor(String.valueOf(page), "page", horizonLines, horizonLines, Constants.ERR_IDX);
	}
	public void errForThemes(Object theme) {
		logFor(String.valueOf(theme), "theme", horizonStars, horizonStars, Constants.ERR_IDX);
	}
	
	
	// 刷出缓冲区的数据		add at 2016.04.15
	public void flush() {
		try {
			Set<String> flushed = new HashSet<>();
			for(int i=0; i<Constants.LOG_FILES.length; i++) {
				if(outToLogFile[i] && (! flushed.contains(logBuffNames[i])) ) {
					Tools.flushBuffer(logBuffNames[i]);
					flushed.add(logBuffNames[i]);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	// ------------ 待续 --------------------
	

	
}
