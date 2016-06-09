/**
 * file name : Constants.java
 * created at : 8:06:27 PM Jul 24, 2015
 * created by 970655147
 */

package com.hx.log.log;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.AbstractMap;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.atomic.AtomicInteger;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

import com.hx.attrHandler.attrHandler.StandardHandlerParser;
import com.hx.attrHandler.attrHandler.operation.interf.OperationAttrHandler;
import com.hx.attrHandler.util.WordsSeprator;
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

// 常量
public class Constants {

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

	// update at 2016.04.21
	// ------------------------ 业务相关常量[从Log, Tools中提取] ---------------------------------
	// 默认的常量配置
		// Log相关
	public static final String DEFAULT_HORIZON_LINES = "-----------------------------------";
	public static final String DEFAULT_HORIZON_STARTS = "***********************************";
	public static final String DEFAULT_GOT_THERE = "get there...";	
	public static final String DEFAULT_GOT_NOTHING = "got nothing ~";	
	
	public static final String DEFAULT_BUFF_NAME_PREFIX = "Logger"; 
	public static final String DEFAULT_BUFF_NAME_SEP = "_"; 
	public static final OutputStream[] defaultOutStreams = new OutputStream[] {
																System.out,
																System.err		
															};
	public static final boolean[] defaultOutToLogFile = new boolean[] {
																false,
																false
															};
	public static final String[] defaultLogBuffSuffix = new String[] {
																"Log.log",
																"Log.err"
															};
	public static final String[] defaultLogFiles = new String[] {
																"C:\\Users\\970655147\\Desktop\\tmp\\out.log",
																"C:\\Users\\970655147\\Desktop\\tmp\\out.log"
															};
	public static final OutputStream nullOutputStream = new NullOutputStream();
	public static final String defaultDateFormat = "yyyy-MM-dd hh:mm:ss:SSS";
	public static final String defaultLogPattern = ">>>> [${idx }] [${date }] - [${mode }] => `${msg }`  >>>>";
	public static final String defaultTaskBeforeLogPattern = "URL : '${url }' \r\n --------------------- [ '${taskName }' start ... ] --------------------------";
	public static final String defaultTaskAfterLogPattern = "FetchedResult : '${result }' \r\n --------------------- [ '${taskName }' end ... ] -------------------------- \r\n spent '${spent }' ms ... ";
	public static final String defaultTaskExceptionLogPattern = "Exception : '${exception }' \r\n while fetch : '${taskName }', url : '${url }' ";
	public static final LogPatternChain justPrintMsgLogPattern = new LogPatternChain().addLogPattern(new MsgLogPattern(Constants.DEFAULT_VALUE) );
	
	public static final String _DEFAULT_SEP_WHILE_CRLF = " ";
	public static final String _DEFAULT_SEP_WHILE_NO_CRLF = ", ";
	public static final String _DEFAULT_SEP_WHILE_TWO_DIMEN = " ";
	public static final String _DEFAULT_MAP_KV_SEP = " -> ";
	public static final boolean DEFAULT_OUTPUT_APPEND_CRLF = true;
	public static final boolean DEFAULT_ERRPUT_APPEND_CRLF = true;
	public static final boolean DEFAULT_OUTPUT_APPEND_CRLF_FOR_CONTAINER = false;
	public static final boolean DEFAULT_ERRPUT_APPEND_CRLF_FOR_CONTAINER = false;
	
	// 常量配置[首选配置文件中的配置, 否则为默认配置]
	static String HORIZON_LINES = Constants.DEFAULT_HORIZON_LINES;
	static String HORIZON_STARTS = Constants.DEFAULT_HORIZON_STARTS;
	static String GOT_THERE = Constants.DEFAULT_GOT_THERE;
	static String GOT_NOTHING = Constants.DEFAULT_GOT_NOTHING;
	
	// 增加一个输出模式[fatal] : 1. 增加索引, LOG_MODES_STR, LOG_MODES
	// 						2. 增加outStreams, outToLogFiles, logBuffNames, logFIles
	//						3. 增加Log.fatal()系列方法
	//						4. 更新Log.dispath
	//						5. 测试
	public static final int OUT_IDX = 0;
	public static final int ERR_IDX = OUT_IDX + 1;
	static final JSONArray LOG_MODES_STR = new JSONArray()
											.element("Constants.OUT_IDX").element("Constants.ERR_IDX");
	public static final String[] LOG_MODES = {"LOG", "ERROR" };
	// updated at 2015.05.05
	static String buffNamePrefix = DEFAULT_BUFF_NAME_PREFIX;
	static String buffNameSep = DEFAULT_BUFF_NAME_SEP;
	static OutputStream[] outStreams = Arrays.copyOf(defaultOutStreams, defaultOutStreams.length);
	static boolean[] outToLogFile = Arrays.copyOf(defaultOutToLogFile, defaultOutToLogFile.length);
	static String[] logBuffSuffix = Arrays.copyOf(defaultLogBuffSuffix, defaultLogBuffSuffix.length);
	static String[] logFiles = Arrays.copyOf(defaultLogFiles, defaultLogFiles.length);
	public static DateFormat dateFormat = new SimpleDateFormat(Constants.defaultDateFormat );
	static boolean usePattern = Boolean.parseBoolean(Constants.DEFAULT_USE_PATTERN);
	static String logPattern = Constants.defaultLogPattern;	
	static LogPatternChain logPatternChain = justPrintMsgLogPattern;	
	
	static String DEFAULT_SEP_WHILE_CRLF = Constants._DEFAULT_SEP_WHILE_CRLF;
	static String DEFAULT_SEP_WHILE_NO_CRLF = Constants._DEFAULT_SEP_WHILE_NO_CRLF;
	static String DEFAULT_SEP_WHILE_TWO_DIMEN = Constants._DEFAULT_SEP_WHILE_TWO_DIMEN;
	static String DEFAULT_MAP_KV_SEP = Constants._DEFAULT_MAP_KV_SEP;
	
	static boolean OUTPUT_APPEND_CRLF = Constants.DEFAULT_OUTPUT_APPEND_CRLF;
	static boolean ERRPUT_APPEND_CRLF = Constants.DEFAULT_ERRPUT_APPEND_CRLF;
	static boolean OUTPUT_APPEND_CRLF_FOR_CONTAINER = Constants.DEFAULT_OUTPUT_APPEND_CRLF_FOR_CONTAINER;
	static boolean ERRPUT_APPEND_CRLF_FOR_CONTAINER = Constants.DEFAULT_ERRPUT_APPEND_CRLF_FOR_CONTAINER;
	
	// 日志格式相关
	public static final String VAR_START = "${";
	public static final String VAR_END = "}";
	public static final String LBRACKET = "(";
	public static final String RBRACKET = ")";
	
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
	
	// 'logPattern''s seprators 
	public static final Set<String> logPatternSeps = new HashSet<>();
	static {
		logPatternSeps.add(VAR_START);
		logPatternSeps.add(VAR_END);
		logPatternSeps.add(LBRACKET);
		logPatternSeps.add(RBRACKET);
	}
	
		// Tools相关
	// 默认的相关配置变量
	private static final String DEFAULT_TMP_NAME = "tmp";
	private static final String DEFAULT_TMP_DIR = "C:\\Users\\970655147\\Desktop\\tmp";
	private static final String DEFAULT_WRITE_ASYNC = "false";
	private static final String DEFAULT_USE_PATTERN = "false";
	private static final int DEFAULT_BUFF_SIZE_ON_TRANS_STREAM = 2048;
	private static final String DEFAULT_SUFFIX = ".html";
	// 取得默认编码??
	public static final String _DEFAULT_CHARSET = Charset.defaultCharset().name();
	
	private static final int DEFAULT_CHECK_INTERVAL = 3 * 1000;
	private static final int DEFAULT_N_THREADS = 10;
	
	// 线程池相关
	static int CHECK_INTERVAL = Constants.DEFAULT_CHECK_INTERVAL;
	static int N_THREADS = Constants.DEFAULT_N_THREADS;
	static ThreadPoolExecutor threadPool = null;
	static boolean WRITE_ASYNC = Boolean.parseBoolean(Constants.DEFAULT_WRITE_ASYNC);
	
	// 临时文件相关
	static String TMP_NAME = Constants.DEFAULT_TMP_NAME;
	static String TMP_DIR = Constants.DEFAULT_TMP_DIR;
	static AtomicInteger TMP_IDX = new AtomicInteger();
	static String SUFFIX = Constants.DEFAULT_SUFFIX;
	static String DEFAULT_CHARSET = Constants._DEFAULT_CHARSET;
	static int BUFF_SIZE_ON_TRANS_STREAM = Constants.DEFAULT_BUFF_SIZE_ON_TRANS_STREAM;
	
	static Set<String> emptyStrCondition = new HashSet<>();
	static Set<Character> mayBeFileNameSeps = new HashSet<>();
	static {
		emptyStrCondition.add(EMPTY_STR);
		mayBeFileNameSeps.add(QUESTION);
	}
	
	// 打印任务日志相关
	static String taskBeforeLogPattern = Constants.defaultTaskBeforeLogPattern;
	static String taskAfterLogPattern = Constants.defaultTaskAfterLogPattern;
	static String taskExceptionLogPattern = Constants.defaultTaskExceptionLogPattern;
	public static LogPatternChain taskBeforeLogPatternChain = null;
	public static LogPatternChain taskAfterLogPatternChain = null;
	public static LogPatternChain taskExceptionLogPatternChain = null;
	
    // ------------ 初始化了所有的静态成员, 在进行更新配置 --------------------
	// 初始化相关配置
	static {
		boolean isException = false;
		Properties props = new Properties();
		try {
//			InputStream config = new FileInputStream(new File("./src/config.conf") );
			// 前者为true, 后者为false
//			Log.log(Main.class.getClass().getClassLoader() == null);
//			Log.log(new Main().getClass().getClassLoader() == null);
			InputStream config = new Test00HelloWorld().getClass().getClassLoader().getResourceAsStream("HXLogConfig.conf");
			props.load(new InputStreamReader(config, DEFAULT_CHARSET) );
		} catch (FileNotFoundException e) {
//			e.printStackTrace();
			props = null;
			isException = true;
			System.err.println("config file is not exist ...");
		} catch (IOException e) {
//			e.printStackTrace();
			props = null;
			isException = true;
			System.err.println("IO Exception ...");
		} catch (NullPointerException e) {
//			e.printStackTrace();
			props = null;
			isException = true;
			System.err.println("config file is not exist ...");
		}
		
		if(! isException) {
			initToolsByConfigFile(props);
			initLogByConfigFile(props);
		}
		JSONObject newProps = JSONObject.fromObject(props);
		taskBeforeLogPatternChain = initLogPattern(taskBeforeLogPattern, newProps);
		taskAfterLogPatternChain = initLogPattern(taskAfterLogPattern, newProps);
		taskExceptionLogPatternChain = initLogPattern(taskExceptionLogPattern, newProps);
		if(usePattern) {
			logPatternChain = initLogPattern(logPattern, newProps);
		}
		
		props = null;		// help gc
	}
	// 使用配置文件进行初始化	
		// 静态块, 初始化 可配置的数据
		// tmpName, tmpDir, buffSize, suffix, checkInterval, nThreads
		// emptyCondition, mayBeFileNameSeps
	private static void initToolsByConfigFile(Properties props) {
		TMP_NAME = props.getProperty("tmpName", DEFAULT_TMP_NAME);
		TMP_DIR = props.getProperty("tmpDir", DEFAULT_TMP_DIR);
		BUFF_SIZE_ON_TRANS_STREAM = Integer.parseInt(props.getProperty("buffSizeOnTransStream", String.valueOf(BUFF_SIZE_ON_TRANS_STREAM)) );
		SUFFIX = props.getProperty("suffix", DEFAULT_SUFFIX);
		DEFAULT_CHARSET = props.getProperty("defaultCharSet", _DEFAULT_CHARSET);
		// check for the 'charset'
			Charset.forName(DEFAULT_CHARSET);
		WRITE_ASYNC = Boolean.parseBoolean(props.getProperty("writeAsync", DEFAULT_WRITE_ASYNC) );
		CHECK_INTERVAL = Integer.parseInt(props.getProperty("checkInterval", String.valueOf(DEFAULT_CHECK_INTERVAL)) );
		N_THREADS = Integer.parseInt(props.getProperty("nThreads", String.valueOf(DEFAULT_N_THREADS)) );
		
		String[] emptyConds = props.getProperty("emptyStrCondition", Constants.EMPTY_STR).split(";");
		for(String emptyCon : emptyConds) {
			emptyStrCondition.add(emptyCon.trim() );
		}
		String[] fileNameSeps = props.getProperty("mayBeFileNameSeps", Constants.EMPTY_STR).split(";");
		for(String sep : fileNameSeps) {
			if(! isEmpty0(sep)) {
				mayBeFileNameSeps.add(sep.charAt(0) );
			}
		}
		
		taskBeforeLogPattern = props.getProperty("taskBeforeLogPattern", taskBeforeLogPattern);
		taskAfterLogPattern = props.getProperty("taskAfterLogPattern", taskAfterLogPattern);
		taskExceptionLogPattern = props.getProperty("taskExceptionLogPattern", taskExceptionLogPattern);
	}
	static boolean isEmpty0(String str) {
		return (str == null) || emptyStrCondition.contains(str.trim());
	}
	
	// 使用properties初始化log相关的常量配置
		// 使用配置文件进行初始化		add at 2016.04.15
	private static void initLogByConfigFile(Properties props) {
		if(props != null) {
			HORIZON_LINES = props.getProperty("horizonLines", DEFAULT_HORIZON_LINES);
			HORIZON_STARTS = props.getProperty("horizonStars", DEFAULT_HORIZON_STARTS);
			GOT_THERE = props.getProperty("gotThere", DEFAULT_GOT_THERE);
			GOT_NOTHING = props.getProperty("gotNothing", DEFAULT_GOT_NOTHING);
			
			buffNamePrefix = props.getProperty("buffNamePrefix", DEFAULT_BUFF_NAME_PREFIX);
			buffNameSep = props.getProperty("buffNameSep", DEFAULT_BUFF_NAME_SEP);
			String outToConsole = props.getProperty("outToConsole", Constants.TRUE);
			if(! outToConsole.equals(Constants.TRUE) ) {
				outStreams[OUT_IDX] = null;
			}
			String errToConsole = props.getProperty("errToConsole", Constants.TRUE);
			if(! errToConsole.equals(Constants.TRUE) ) {
				outStreams[ERR_IDX] = null;
			}
			outToLogFile[OUT_IDX] = Boolean.parseBoolean(props.getProperty("outToLogFile", String.valueOf(outToLogFile[OUT_IDX])) );
			outToLogFile[ERR_IDX] = Boolean.parseBoolean(props.getProperty("errToLogFile", String.valueOf(outToLogFile[ERR_IDX])) );
			logBuffSuffix[OUT_IDX] = props.getProperty("outLogBuffName", logBuffSuffix[OUT_IDX]); 
			logBuffSuffix[ERR_IDX] = props.getProperty("errLogBuffName", logBuffSuffix[ERR_IDX]); 
			logFiles[OUT_IDX] = props.getProperty("outLogFilePath", logFiles[OUT_IDX]); 
			logFiles[ERR_IDX] = props.getProperty("errLogFilePath", logFiles[ERR_IDX]); 
			dateFormat = new SimpleDateFormat(props.getProperty("dateFormat", Constants.defaultDateFormat) );
			usePattern = Boolean.parseBoolean(props.getProperty("usePattern", Constants.TRUE) );
			logPattern = props.getProperty("logPattern", logPattern);
			
			DEFAULT_SEP_WHILE_CRLF = props.getProperty("defaultSepWhileCRLF", _DEFAULT_SEP_WHILE_CRLF);
			DEFAULT_SEP_WHILE_NO_CRLF = props.getProperty("defaultSepWhileNotCRLF", _DEFAULT_SEP_WHILE_NO_CRLF);
			DEFAULT_SEP_WHILE_TWO_DIMEN = props.getProperty("defaultSepWhileTwoDimen", _DEFAULT_SEP_WHILE_TWO_DIMEN);
			DEFAULT_MAP_KV_SEP = props.getProperty("defaultMapKVSep", _DEFAULT_MAP_KV_SEP);
			
			OUTPUT_APPEND_CRLF = props.getProperty("defaultOutputAppendCrlf", Constants.TRUE).equals(Constants.TRUE);
			ERRPUT_APPEND_CRLF = props.getProperty("defaultErrputAppendCrlf", Constants.TRUE).equals(Constants.TRUE);
			OUTPUT_APPEND_CRLF_FOR_CONTAINER = props.getProperty("defaultOutputAppendCrlfForContainer", Constants.FALSE).equals(Constants.TRUE);
			ERRPUT_APPEND_CRLF_FOR_CONTAINER = props.getProperty("defaultErrputAppendCrlfForContainer", Constants.FALSE).equals(Constants.TRUE);
		}
	}	
	
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
							logPatternChain.addLogPattern(new DateLogPattern(Constants.dateFormat) );
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
