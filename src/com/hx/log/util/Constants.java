/**
 * file name : Constants.java
 * created at : 8:06:27 PM Jul 24, 2015
 * created by 970655147
 */

package com.hx.log.util;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.AbstractMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import com.hx.attrHandler.attrHandler.StandardHandlerParser;
import com.hx.attrHandler.attrHandler.operation.interf.OperationAttrHandler;
import com.hx.log.log.LogPattern;
import com.hx.log.log.LogPattern.ConstantsLogPattern;
import com.hx.log.log.LogPattern.DateLogPattern;
import com.hx.log.log.LogPattern.ExceptionLogPattern;
import com.hx.log.log.LogPattern.HandlerLogPattern;
import com.hx.log.log.LogPattern.IncIndexLogPattern;
import com.hx.log.log.LogPattern.LogPatternChain;
import com.hx.log.log.LogPattern.ModeLogPattern;
import com.hx.log.log.LogPattern.MsgLogPattern;
import com.hx.log.log.LogPattern.OneStringVariableLogPattern;
import com.hx.log.log.LogPattern.ResultLogPattern;
import com.hx.log.log.LogPattern.SpentLogPattern;
import com.hx.log.log.LogPattern.StackTraceLogPattern;
import com.hx.log.log.LogPattern.TaskNameLogPattern;
import com.hx.log.log.LogPattern.ThreadLogPattern;
import com.hx.log.log.LogPattern.UrlLogPattern;
import com.hx.log.test.Test00HelloWorld;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

// 常量
public class Constants {

	// ----------------------------------- 相关业务常量 ------------------------------------------
	// Constants
	public static final String EMPTY_STR = "";
	public static final String CRLF = "\r\n";
	public static final String NULL = "null";
	public static final String DEFAULT_VALUE = NULL;
	public static final String CONSTANTS = "constants";
	public static final String HANDLER = "handler";
	public static final String TRUE = Boolean.TRUE.toString();
	public static final String FALSE = Boolean.FALSE.toString();
	
	// Constants
	public static final Character SLASH = '\\';
	public static final Character INV_SLASH = '/';
	public static final Character QUESTION = '?';
	public static final Character DOT = '.';
	public static final Character COMMA = ',';
	public static final Character COLON = ':';	
	public static final Character SPACE = ' ';
	public static final Character TAB = '\t';
	public static final Character CR = '\r';
	public static final Character LF = '\n';
	public static final Character QUOTE = '\"';
	public static final Character SINGLE_QUOTE = '\'';
	
	// 默认的字符集
	public static final String defaultCharset = Charset.defaultCharset().name();
	
	// LogPattern 相关
	public static final String VAR_START = "${";
	public static final String VAR_END = "}";
	public static final String LBRACKET = "(";
	public static final String RBRACKET = ")";
	
	// 'logPattern''s seprators 
	public static final Set<String> logPatternSeps = new HashSet<>();
	static {
		logPatternSeps.add(VAR_START);
		logPatternSeps.add(VAR_END);
		logPatternSeps.add(LBRACKET);
		logPatternSeps.add(RBRACKET);
	}

	// 增加一个输出模式[fatal] : 1. 增加索引, LOG_MODES_STR, LOG_MODES
	// 						2. 增加outStreams, outToLogFiles, logBuffNames, logFIles
	//						3. 增加Log.fatal()系列方法
	//						4. 更新Log.dispath
	//						5. 测试
	// Log.formatLogInfo(LogPatternChain, String[]) ->  Log.formatLogInfo(LogPatternChain, Map<String, String>)
//	// not concern 'ConreteValue', just ensure '*_idx' unique
//		// String mode = args[MODEL_IDX]
//	public static final int MODE_IDX = 0;
//	public static final int MSG_IDX = MODE_IDX + 1;
//	public static final int LOG_PATTERN_HANDLER_IDX = MSG_IDX + 1;
//	public static final int TASK_NAME_IDX = LOG_PATTERN_HANDLER_IDX + 1;
//	public static final int URL_IDX = TASK_NAME_IDX + 1;
//	public static final int RESULT_IDX = URL_IDX + 1;
//	public static final int SPENT_IDX = RESULT_IDX + 1;
//	public static final int EXCEPTION_IDX = SPENT_IDX + 1;
//	public static final int MAX_LOG_PATTERN_IDX = EXCEPTION_IDX + 1;
		
	public static final int OUT_IDX = 0;
	public static final int ERR_IDX = OUT_IDX + 1;
	static final JSONArray LOG_MODES_STR = new JSONArray()
											.element("Constants.OUT_IDX").element("Constants.ERR_IDX");
	public static final String[] LOG_MODES = {"LOG", "ERROR" };

	// fixed pattern
	// add at 2016.04.21
	public static final String LOG_PATTERN_CHAIN = "logPatternChain";
	public static final String LOG_PATTERN_CONSTANTS = CONSTANTS;
	public static final String LOG_PATTERN_DATE = "date";
	public static final String LOG_PATTERN_IDX = "idx";
	
	// controllable [variable]
	// add at 2016.04.22
	public static final String LOG_PATTERN_MODE = "mode";
	public static final String LOG_PATTERN_MSG = "msg";
	public static final String LOG_PATTERN_HANDLER = HANDLER;
	// add at 2016.04.29
	public static final String LOG_PATTERN_THREAD = "thread";
	public static final String LOG_PATTERN_STACK_TRACE = "stackTrace";
	
	// add at 2016.04.23
	public static final String LOG_PATTERN_TASK_NAME = "taskName";
	public static final String LOG_PATTERN_URL = "url";
	public static final String LOG_PATTERN_RESULT = "result";
	public static final String LOG_PATTERN_SPENT = "spent";
	public static final String LOG_PATTERN_EXCEPTION = "exception";
	
	// default 'VariableValue'[${var }], and supported two 'mode'
	public static final String DEFAULT_VAR_VALUE = "varNotFound";
	
	// updated at 2016.06.28
	// ----------------------------------- 相关可配置数据的初始化 ------------------------------------------
	static Map<String, String> props = null;
	// 读取配置文件
	static {
		boolean isException = false;
		Properties props = new Properties();
		try {
//			InputStream config = new FileInputStream(new File("./src/config.conf") );
			// 前者为true, 后者为false
//			Log.log(Main.class.getClass().getClassLoader() == null);
//			Log.log(new Main().getClass().getClassLoader() == null);
			InputStream config = new Test00HelloWorld().getClass().getClassLoader().getResourceAsStream("HXLogConfig.conf");
			props.load(new InputStreamReader(config, defaultCharset) );
		} catch (FileNotFoundException e) {
			isException = true;
			System.err.println("config file is not exist ...");
		} catch (IOException e) {
			isException = true;
			System.err.println("IO Exception ...");
		} catch (NullPointerException e) {
			isException = true;
			System.err.println("config file is not exist ...");
		}
		
		if(! isException) {
			Constants.props = JSONObject.fromObject(props);
		}

		props = null;		// help gc
	}
	
	// update at 2016.04.21
	// ------------------------ 业务相关常量[从Log, Tools中提取] ---------------------------------
	// 增加一个配置
	// 1. Constants中增加对应的key[配置文件中的名称]
	// 2. 在defaultProps中增加其默认值
	// 3. 在使用的地方进行使用[Constants.optXXX]
	// 相关配置常量
	public static final String horizonLines = "horizonLines";
	public static final String horizonStars = "horizonStars";
	public static final String gotThere = "gotThere";
	public static final String gotNothing = "gotNothing";
	public static final String buffNamePrefix = "buffNamePrefix";
	public static final String buffNameSep = "buffNameSep";
	
	public static final String outToConsole = "outToConsole";
	public static final String errToConsole = "errToConsole";
	public static final String outToLogFile = "outToLogFile";
	public static final String errToLogFile = "errToLogFile";
	public static final String outLogBuffName = "outLogBuffName";
	public static final String errLogBuffName = "errLogBuffName";
	public static final String outLogFilePath = "outLogFilePath";
	public static final String errLogFilePath = "errLogFilePath";
	
	public static final String defaultSepWhileCRLF = "defaultSepWhileCRLF";
	public static final String defaultSepWhileNotCRLF = "defaultSepWhileNotCRLF";
	public static final String defaultSepWhileTwoDimen = "defaultSepWhileTwoDimen";
	public static final String defaultSepWhileMapKV = "defaultMapKVSep";
	
	public static final String defaultOutputAppendCrlf = "defaultOutputAppendCrlf";
	public static final String defaultErrputAppendCrlf = "defaultErrputAppendCrlf";
	public static final String defaultOutputAppendCrlfForContainer = "defaultOutputAppendCrlfForContainer";
	public static final String defaultErrputAppendCrlfForContainer = "defaultErrputAppendCrlfForContainer";
	public static final String defaultOutputAppendCrlfForFormat = "defaultOutputAppendCrlfForFormat";
	public static final String defaultErrputAppendCrlfForFormat = "defaultErrputAppendCrlfForFormat";
	
	public static final String dateFormat = "dateFormat";
	public static final String usePattern = "usePattern";
	public static final String logPattern = "logPattern";
	
	// Tools 相关
	public static final String tmpName = "tmpName";
	public static final String tmpDir = "tmpDir";
	public static final String suffix = "suffix";
	public static final String buffSize = "buffSize";
	public static final String estimateFileLines = "estimateFileLines";
	public static final String writeAsync = "writeAsync";
	public static final String isDebugOn = "isDebugOn";
	
	public static final String checkInterval = "checkInterval";
	public static final String nThreads = "nThreads";
	public static final String emptyStrCondition = "emptyStrCondition";
	public static final String mayBeFileNameSeps = "mayBeFileNameSeps";
	
	public static final String taskBeforeLogPattern = "taskBeforeLogPattern";
	public static final String taskAfterLogPattern = "taskAfterLogPattern";
	public static final String taskExceptionLogPattern = "taskExceptionLogPattern";
	
	// JSONTransferable 相关
	public static final String jsonTUtils = "jsonTUtils";
	public static final String jsonTIdxMapManager = "jsonTIdxMapManager";
	public static final String jsonTId = "jsonTId";
	public static final String jsonTForEachEle = "jsonTForEachEle";
	public static final String jsonTBeanKey = "jsonTBeanKey";
	public static final String jsonTProtoBeanKey = "jsonTProtoBeanKey";
	public static final String jsonTArrIdxMapKey = "jsonTArrIdxMapKey";
	public static final String jsonTDefaultLoadIdx = "jsonTDefaultLoadIdx";
	public static final String jsonTDefaultFilterIdx = "jsonTDefaultFilterIdx";
	public static final String jsonTIdxSuffix = "jsonTIdxSuffix";
	public static final String jsonTObjSuffix = "jsonTObjSuffix";
	public static final String jsonTArrSuffix = "jsonTArrSuffix";
	
	// 默认的常量配置
	static Map<String, String> defaultProp = new HashMap<>();
	static {
		// Log 相关
		defaultProp.put(horizonLines, "-----------------------------------");
		defaultProp.put(horizonStars, "***********************************");
		defaultProp.put(gotThere, "get there...");
		defaultProp.put(gotNothing, "got nothing ~");
		defaultProp.put(buffNamePrefix, "Logger");
		defaultProp.put(buffNameSep, "_");
		
		defaultProp.put(outToConsole, "true");
		defaultProp.put(errToConsole, "true");
		defaultProp.put(outToLogFile, "false");
		defaultProp.put(errToLogFile, "false");
		defaultProp.put(outLogBuffName, "Log.log");
		defaultProp.put(errLogBuffName, "Log.err");
		defaultProp.put(outLogFilePath, "C:\\Users\\970655147\\Desktop\\tmp\\out.log");
		defaultProp.put(errLogFilePath, "C:\\Users\\970655147\\Desktop\\tmp\\out.log");
		
		defaultProp.put(defaultSepWhileCRLF, " ");
		defaultProp.put(defaultSepWhileNotCRLF, ", ");
		defaultProp.put(defaultSepWhileTwoDimen, " ");
		defaultProp.put(defaultSepWhileMapKV, " -> ");
		
		defaultProp.put(defaultOutputAppendCrlf, "true");
		defaultProp.put(defaultErrputAppendCrlf, "true");
		defaultProp.put(defaultOutputAppendCrlfForContainer, "false");
		defaultProp.put(defaultErrputAppendCrlfForContainer, "false");
		defaultProp.put(defaultOutputAppendCrlfForFormat, "false");
		defaultProp.put(defaultErrputAppendCrlfForFormat, "false");
		
		defaultProp.put(dateFormat, "yyyy-MM-dd HH:mm:ss:SSS");
		defaultProp.put(usePattern, "true");
		defaultProp.put(logPattern, ">>>> [${idx }] [${date }] - [${mode }] => `${msg }`  >>>>");
		
		// Tools 相关
		defaultProp.put(tmpName, "tmp");
		defaultProp.put(tmpDir, "C:\\Users\\970655147\\Desktop\\tmp");
		defaultProp.put(suffix, ".txt");
		defaultProp.put(buffSize, "2048");
		defaultProp.put(estimateFileLines, "100");
		defaultProp.put(writeAsync, "false");
		defaultProp.put(isDebugOn, "false");
		
		defaultProp.put(checkInterval, "3000");
		defaultProp.put(nThreads, "10");
		defaultProp.put(emptyStrCondition, "null;NULL");
		defaultProp.put(mayBeFileNameSeps, "?");
		
		defaultProp.put(taskBeforeLogPattern, "URL : '${url }' \r\n --------------------- [ '${taskName }' start ... ] --------------------------");
		defaultProp.put(taskAfterLogPattern, "FetchedResult : '${result }' \r\n --------------------- [ '${taskName }' end ... ] -------------------------- \r\n spent '${spent }' ms ...");
		defaultProp.put(taskExceptionLogPattern, "Exception : '${exception }' \r\n while fetch : '${taskName }', url : '${url }'");
		
		// JSONTransferable 相关
		defaultProp.put(jsonTUtils, "Tools");
		defaultProp.put(jsonTIdxMapManager, "Constants");
		defaultProp.put(jsonTId, "id");
		defaultProp.put(jsonTForEachEle, "ele");
		defaultProp.put(jsonTBeanKey, "BEAN_KEY");
		defaultProp.put(jsonTProtoBeanKey, "PROTO_BEAN");
		defaultProp.put(jsonTArrIdxMapKey, "arrIdxMap");
		defaultProp.put(jsonTDefaultLoadIdx, "CAMEL");
		defaultProp.put(jsonTDefaultFilterIdx, "ALL");
		defaultProp.put(jsonTIdxSuffix, "Idxes");
		defaultProp.put(jsonTObjSuffix, "Obj");
		defaultProp.put(jsonTArrSuffix, "Arr");

	}

	public static final Set<String> emptyStrConditiones = new HashSet<>();
	public static final Set<Character> mayBeFileNameSepsNow = new HashSet<>();
	static {
		emptyStrConditiones.add(EMPTY_STR);
		
		String[] fileNameSeps = optString(mayBeFileNameSeps).split(";");
		for(String sep : fileNameSeps) {
			if(! isEmpty0(sep)) {
				mayBeFileNameSepsNow.add(sep.charAt(0) );
			}
		}
	}
	public static final OutputStream[] outStreams = new OutputStream[] {
																System.out,
																System.err,
															};
	public static final boolean[] outToLogFiles = new boolean[] {
																optBoolean(outToLogFile),
																optBoolean(errToLogFile),																
															};
	public static final String[] logBuffSuffixes = new String[] {
																optString(outLogBuffName),
																optString(errLogBuffName),
															};
	public static final String[] logFiles = new String[] {
																optString(outLogFilePath),
																optString(errLogFilePath),
															};
	public static final OutputStream nullOutputStream = new NullOutputStream();

	public static final DateFormat dateFormatNow = new SimpleDateFormat(optString(dateFormat) );
	public static final LogPatternChain justPrintMsgLogPattern = new LogPatternChain().addLogPattern(new MsgLogPattern(Constants.DEFAULT_VALUE) );
	public static final LogPatternChain logPatternChain = optBoolean(usePattern) ? initLogPattern(optString(logPattern), props) : justPrintMsgLogPattern;
	
	
	// 获取相关默认值
	public static String optString(String key, String defaultVal) {
		String val = (props != null) ? props.get(key) : null;
		val = (val != null) ? val : defaultProp.get(key);
		
		if(val == null) {
			return defaultVal;
		}
		return val;
	}
	public static int optInt(String key, int defaultVal) {
		String val = optString(key);
		
		if(isEmpty0(val) ) {
			return defaultVal;
		}
		return Integer.parseInt(val);
	}
	public static long optLong(String key, long defaultVal) {
		String val = optString(key);
		
		if(isEmpty0(val) ) {
			return defaultVal;
		}
		return Long.parseLong(val);
	}
	public static boolean optBoolean(String key, boolean defaultVal) {
		String val = optString(key);
		
		if(isEmpty0(val) ) {
			return defaultVal;
		}
		return Boolean.parseBoolean(val);
	}
	public static double optDouble(String key, double defaultVal) {
		String val = optString(key);
		
		if(isEmpty0(val) ) {
			return defaultVal;
		}
		return Double.parseDouble(val);
	}
	
	public static String optString(String key) {
		return optString(key, null);
	}
	public static int optInt(String key) {
		return optInt(key, 0);
	}
	public static long optLong(String key) {
		return optLong(key, 0l);
	}
	public static boolean optBoolean(String key) {
		return optBoolean(key, false);
	}
	public static double optDouble(String key) {
		return optDouble(key, 0.0d);
	}
	
	// add for 'ObjectAlreadyExsists' in 'JSONTransferableUtils.encapJSON'		 add at 2016.06.19
	public static final JSONObject OBJECT_ALREADY_EXISTS = new JSONObject().element("info", "ObjectAlreadyExsists");
	// 别让Constants 依赖于Tools, 否则initDependency 又出现了,, 呵呵呵呵 			2016.06.20
//	public static final Set<String> EMPTY_INIT_OBJ_FILTER = Tools.asSet();
	public static final Set<String> EMPTY_INIT_OBJ_FILTER = new HashSet<>();
	
	// 判断给定的字符串是否为空
	static boolean isEmpty0(String str) {
		return (str == null) || emptyStrConditiones.contains(str.trim());
	}

	// ----------------------------------- 相关业务方法 ------------------------------------------
	// ------------ 格式化日期相关 ------- 2016.04.21 -------------
	// 根据给定的logPattern获取打印日志所需的LogPatternChain
	public static LogPatternChain initLogPattern(String logPattern, Map<String, String> props) {
		LogPatternChain logPatternChain = new LogPatternChain();
		WordsSeprator sep = new WordsSeprator(logPattern, Constants.logPatternSeps, null, true);
		while(sep.hasNext() ) {
			String next = sep.next();
			switch (next) {
				case Constants.VAR_START:
					assert0(sep.hasNext(), "unExpected end of 'logPattern'! ");
					String varName = sep.next().trim();
					switch (varName) {
						case Constants.LOG_PATTERN_DATE:
							logPatternChain.addLogPattern(new DateLogPattern(dateFormatNow) );
							break;
						case Constants.LOG_PATTERN_MODE:
							logPatternChain.addLogPattern(new ModeLogPattern(Constants.LOG_MODES[Constants.OUT_IDX]) );	
							break;
						case Constants.LOG_PATTERN_MSG:
							logPatternChain.addLogPattern(new MsgLogPattern(Constants.DEFAULT_VAR_VALUE) );	
							break;
						case Constants.LOG_PATTERN_IDX:
							// ${idx }
							if(! LBRACKET.equals(sep.seek()) ) {
								logPatternChain.addLogPattern(new IncIndexLogPattern(0, 1) );
								break ;
							}
							// ${idx() }
							sep.next();
							if(RBRACKET.equals(sep.seek()) ) {
								logPatternChain.addLogPattern(new IncIndexLogPattern(0, 1) );
								sep.next();
								break ;
							}
							// ${idx(2) } or $idx(2, 4)
							String initValOrAndInc = sep.next();
							int commaIdx = initValOrAndInc.indexOf(",");
							int inc = 1;
							int initVal = 0;
							// 'Integer.parseInt' may got 'NumberFormatException'
							if(commaIdx >= 0) {
								inc = Integer.parseInt(initValOrAndInc.substring(commaIdx + 1).trim() );
								initVal = Integer.parseInt(initValOrAndInc.substring(0, commaIdx).trim() );
							} else {
								initVal = Integer.parseInt(initValOrAndInc.trim() );
							}
							logPatternChain.addLogPattern(new IncIndexLogPattern(initVal, inc) );
							assert0(RBRACKET.equals(sep.next()), "expect a ')', but got an : '" + sep.seekLastNext() + "' !" );
							break;
						case LOG_PATTERN_HANDLER :
							assert0(LBRACKET.equals(sep.next()), "expect a '(', but go an : '" + sep.seekLastNext() + "' !");
							int stackCnt = 1;
							StringBuilder sb = new StringBuilder(sep.length() - sep.lastNextPos() );
							while(sep.hasNext() ) {
								String partHandlerStr = sep.next();
								if(LBRACKET.equals(partHandlerStr) ) {
									stackCnt ++;
								}
								if(RBRACKET.equals(partHandlerStr) ) {
									stackCnt --;
								}
								if(stackCnt == 0) {
									break ;
								}
								sb.append(partHandlerStr);
							}
							assert0(RBRACKET.equals(sep.seekLastNext()), "expect 'handler()' endsWith ')', but got an : '" + sep.seekLastNext() + "' !");
							String handlerStr = sb.toString();
							OperationAttrHandler operationHandler = new StandardHandlerParser().handlerParse(handlerStr, Constants.HANDLER);
							logPatternChain.addLogPattern(new HandlerLogPattern(operationHandler, Constants.DEFAULT_VAR_VALUE) );
							break ;
						case Constants.LOG_PATTERN_THREAD:
							logPatternChain.addLogPattern(new ThreadLogPattern() );
							break;
						case Constants.LOG_PATTERN_STACK_TRACE:
							logPatternChain.addLogPattern(new StackTraceLogPattern() );
							break;
						case Constants.LOG_PATTERN_TASK_NAME:
							logPatternChain.addLogPattern(new TaskNameLogPattern(Constants.DEFAULT_VAR_VALUE) );	
							break;
						case Constants.LOG_PATTERN_URL:
							logPatternChain.addLogPattern(new UrlLogPattern(Constants.DEFAULT_VAR_VALUE) );	
							break;
						case Constants.LOG_PATTERN_RESULT:
							logPatternChain.addLogPattern(new ResultLogPattern(Constants.DEFAULT_VAR_VALUE) );	
							break;
						case Constants.LOG_PATTERN_SPENT:
							logPatternChain.addLogPattern(new SpentLogPattern(Constants.DEFAULT_VAR_VALUE) );	
							break;
						case Constants.LOG_PATTERN_EXCEPTION:
							logPatternChain.addLogPattern(new ExceptionLogPattern(Constants.DEFAULT_VAR_VALUE) );	
							break;										
						default:
							String constantsValue = (props == null) ? DEFAULT_VAR_VALUE : (props.get(varName) != null) ? props.get(varName) : DEFAULT_VAR_VALUE;
							logPatternChain.addLogPattern(new ConstantsLogPattern(constantsValue) );
							break;
					}
					assert0(Constants.VAR_END.equals(sep.next() ), "expect an '" + Constants.VAR_END + "', but got an '" + sep.seekLastNext() + "' ! ");
					break;
				default:
					logPatternChain.addLogPattern(new ConstantsLogPattern(next) );
					break;
			}
		}
		
		return logPatternChain;
	}
	
	// incase of 'initDependency'[Tools.taskBeforeLogPatternChain == null]		add at 2016.05.19
	private static void assert0(boolean boo, String msg) {
		if(msg == null) {
			System.err.println("'msg' can't be null ");
			return ;
		}
		if(! boo) {
			throw new RuntimeException("assert0Exception : " + msg);
		}
	}

	// 格式化日期相关
	public static String formatLogInfo(LogPatternChain logPatternChain, JSONObject argsMap) {
		if(logPatternChain == null) {
			return argsMap.optString(Constants.LOG_PATTERN_MSG );
		}
		
		logPatternChain.setResult(null );
		for(LogPattern logPattern : logPatternChain.getChain() ) {
			switch (logPattern.type() ) {
				// use 'Mode' instedof 'LogPatternType.Mode'
				// from : http://caohongxing7604.blog.163.com/blog/static/32016974200991412040387/
				case MODE:
				case MSG:
				case HANDLER :
				case TASK_NAME :
				case URL :
				case RESULT :
				case SPENT :
				case EXCEPTION :
					((OneStringVariableLogPattern) logPattern).setArg(argsMap.optString(logPattern.type().typeKey(), Constants.DEFAULT_VAR_VALUE) );
					break ;
				case PATTERN_CHAIN :
					LogPatternChain subLogPatternChain = (LogPatternChain) logPattern;
					subLogPatternChain.setResult(formatLogInfo(subLogPatternChain, argsMap) );
					break ;
				default:
					break;
			}
		}
		
		return logPatternChain.pattern();
	}
	public static String formatLogInfo(LogPatternChain logPatternChain, AbstractMap<String, String> argsMap) {
		return formatLogInfo(logPatternChain, JSONObject.fromObject(argsMap) );
	}
	
	
}
