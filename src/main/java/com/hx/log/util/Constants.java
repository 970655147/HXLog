/**
 * file name : Constants.java
 * created at : 8:06:27 PM Jul 24, 2015
 * created by 970655147
 */

package com.hx.log.util;

import com.hx.attr_handler.attr_handler.operation.interf.OperationAttrHandler;
import com.hx.attr_handler.util.AttrHandlerUtils;
import com.hx.attr_handler.util.AttrHandlerConstants;
import com.hx.log.io.NullOutputStream;
import com.hx.log.log.LogPatternUtils;
import com.hx.log.log.log_pattern.LogPatternChain;
import com.hx.log.log.log_pattern.VarLogPattern;
import com.hx.log.test.Test00HelloWorld;
import com.hx.json.JSONArray;
import com.hx.json.JSONObject;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.jar.JarFile;


// 常量
public final class Constants {

	// disable constructor
	private Constants() {
		Tools.assert0("can't instantiate !");
	}
	
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
	public static final String DEFAULT_CHARSET = Charset.defaultCharset().name();
	
	// LogPattern 相关
	public static final String VAR_START = "${";
	public static final String VAR_END = "}";
	public static final String OPT_START = "$[";
	public static final String OPT_END = "]";
	public static final String LBRACKET = "(";
	public static final String RBRACKET = ")";
	
	// add at 2016.11.23
	// XX占位符
	public static final String VAR_PLACE = "{}";
	
	// 'logPattern''s seprators 
	public static final Set<String> logPatternSeps = new HashSet<>();
	static {
		logPatternSeps.add(VAR_START);
		logPatternSeps.add(VAR_END);
		logPatternSeps.add(OPT_START);
		logPatternSeps.add(OPT_END);
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
	public static final JSONArray LOG_MODES_STR = new JSONArray()
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
	public static final String LOG_PATTERN_VAR = "var";
	public static final String LOG_PATTERN_MSG = "msg";
	public static final String LOG_PATTERN_LOG_IDX = "logIdx";
	public static final String LOG_PATTERN_HANDLER = HANDLER;
	// add at 2016.04.29
	public static final String LOG_PATTERN_THREAD = "thread";
	public static final String LOG_PATTERN_STACK_TRACE = "stackTrace";
	// add at 2016.11.05
	public static final String LOG_PATTERN_LINE_INFO = "lineInfo";
	// add at 2016.09.23
	public static final String LOG_PATTERN_OPTIONAL = "optional";
	
	// add at 2016.04.23
	public static final String LOG_PATTERN_TASK_NAME = "taskName";
	public static final String LOG_PATTERN_URL = "url";
	public static final String LOG_PATTERN_RESULT = "result";
	public static final String LOG_PATTERN_SPENT = "spent";
	public static final String LOG_PATTERN_EXCEPTION = "exception";
	
	// default 'VariableValue'[${var }], and supported two 'mode'
	public static final String DEFAULT_VAR_VALUE = "VarNotFound";

	// add at 2016.11.23
	// 注释的符号, 以及最长的注释符号的长度
	public static final Set<String> COMMENT_MARKS = new HashSet<>();
	public static final int COMMENT_MAX_LEN;
	static {
		COMMENT_MARKS.add("//");
		COMMENT_MARKS.add("--");
		COMMENT_MARKS.add("#");
		COMMENT_MARKS.add(";");
		COMMENT_MARKS.add("rem");
		
		int maxLen = 1; 
		for(String mark : COMMENT_MARKS) {
			if(mark.length() > maxLen) {
				maxLen = mark.length();
			}
		}
		COMMENT_MAX_LEN = maxLen;
	}

	// add at 2017.04.03, define "HXLogConfig.conf" as constants
	public static final String CONFIG_PATH = "HXLogConfig.conf";

	// updated at 2016.06.28
	// ----------------------------------- 相关可配置数据的初始化 ------------------------------------------
	static Map<String, Object> PROPS = null;
	// 读取配置文件
	static {
		Properties props = new Properties();
		Exception e = loadPropsFromConfig(props);

		if(e != null) {
			e.printStackTrace();
		} else {
			Constants.PROPS = JSONObject.fromObject(props);
		}

		props = null;		// help gc
	}
	
	// update at 2016.04.21
	// ------------------------ 业务相关常量[从Log, Tools中提取] ---------------------------------
	// 增加一个配置
	// 1. Constants中增加对应的key[配置文件中的名称]
	// 2. 在defaultProps中增加其默认值
	// 3. 在使用的地方进行使用[Constants.optXXX]

	// 配置相关常量 
	public static final String _TMP_NAME = "tmpName";
	public static final String _TMP_DIR = "tmpDir";
	public static final String _SUFFIX = "suffix";
	public static final String _BUFF_SIZE = "buffSize";
	public static final String _ESTIMATE_FILE_LINES = "estimateFileLines";

	public static final String _WRITE_ASYNC = "writeAsync";
	public static final String _IS_DEBUG_ON = "isDebugOn";

	public static final String _CHECK_INTERVAL = "checkInterval";
	public static final String _N_THREADS = "nThreads";

	public static final String _EMPTY_STR_CONDITION = "emptyStrCondition";

	public static final String _MAY_BE_FILE_NAME_SEPS = "mayBeFileNameSeps";

	public static final String _TASK_BEFORE_LOG_PATTERN = "taskBeforeLogPattern";
	public static final String _TASK_AFTER_LOG_PATTERN = "taskAfterLogPattern";
	public static final String _TASK_EXCEPTION_LOG_PATTERN = "taskExceptionLogPattern";

	public static final String _HORIZON_LINES = "horizonLines";
	public static final String _HORIZON_STARS = "horizonStars";
	public static final String _GOT_THERE = "gotThere";
	public static final String _GOT_NOTHING = "gotNothing";

	public static final String _BUFF_NAME_PREFIX = "buffNamePrefix";
	public static final String _BUFF_NAME_SEP = "buffNameSep";

	public static final String _OUT_TO_CONSOLE = "outToConsole";
	public static final String _ERR_TO_CONSOLE = "errToConsole";
	public static final String _OUT_TO_LOG_FILE = "outToLogFile";
	public static final String _ERR_TO_LOG_FILE = "errToLogFile";
	public static final String _OUT_LOG_BUFF_NAME = "outLogBuffName";
	public static final String _ERR_LOG_BUFF_NAME = "errLogBuffName";
	public static final String _OUT_LOG_FILE_PATH = "outLogFilePath";
	public static final String _ERR_LOG_FILE_PATH = "errLogFilePath";

	public static final String _DEFAULT_SEP_WHILE_CRLF = "defaultSepWhileCRLF";
	public static final String _DEFAULT_SEP_WHILE_NOT_CRLF = "defaultSepWhileNotCRLF";
	public static final String _DEFAULT_SEP_WHILE_TWO_DIMEN = "defaultSepWhileTwoDimen";
	public static final String _DEFAULT_SEP_MAP_KVSEP = "defaultMapKVSep";

	public static final String _DEFAULT_OUTPUT_APPEND_CRLF = "defaultOutputAppendCrlf";
	public static final String _DEFAULT_ERRPUT_APPEND_CRLF = "defaultErrputAppendCrlf";
	public static final String _DEFAULT_OUTPUT_APPEND_CRLF_FOR_CONTAINER = "defaultOutputAppendCrlfForContainer";
	public static final String _DEFAULT_ERRPUT_APPEND_CRLF_FOR_CONTAINER = "defaultErrputAppendCrlfForContainer";
	public static final String _DEFAULT_OUTPUT_APPEND_CRLF_FOR_FORMAT = "defaultOutputAppendCrlfForFormat";
	public static final String _DEFAULT_ERRPUT_APPEND_CRLF_FOR_FORMAT = "defaultErrputAppendCrlfForFormat";
	public static final String _DEFAULT_IS_FORMAT = "defaultIsFormat";

	public static final String _PREFIX = "PREFIX";
	public static final String _CRLF = "CRLF";
	public static final String _DATE_FORMAT = "dateFormat";
	public static final String _USE_PATTERN = "usePattern";

	public static final String _LOG_PATTERN = "logPattern";
	public static final String _LOG_IDX_HANDLER_PARSER = "logIdxHandlerParser";

	public static final String _JSON_TUTILS = "jsonTUtils";
	public static final String _JSON_TIDX_MAP_MANAGER = "jsonTIdxMapManager";
	public static final String _JSON_TID = "jsonTId";
	public static final String _JSON_TFOR_EACH_ELE = "jsonTForEachEle";
	public static final String _JSON_TBEAN_KEY = "jsonTBeanKey";
	public static final String _JSON_TPROTO_BEAN_KEY = "jsonTProtoBeanKey";
	public static final String _JSON_TARR_IDX_MAP_KEY = "jsonTArrIdxMapKey";
	public static final String _JSON_TDEFAULT_LOAD_IDX = "jsonTDefaultLoadIdx";
	public static final String _JSON_TDEFAULT_FILTER_IDX = "jsonTDefaultFilterIdx";
	public static final String _JSON_TIDX_SUFFIX = "jsonTIdxSuffix";
	public static final String _JSON_TOBJ_SUFFIX = "jsonTObjSuffix";
	public static final String _JSON_TARR_SUFFIX = "jsonTArrSuffix";


	// 默认的配置 
	public static final Map<String, String> DEFAULT_PROPS = new HashMap<>(); 
	static {
		// Tools 相关
		DEFAULT_PROPS.put(_TMP_NAME, "tmp"); 
		DEFAULT_PROPS.put(_TMP_DIR, "F:\\tmp");
		DEFAULT_PROPS.put(_SUFFIX, ".html"); 
		DEFAULT_PROPS.put(_BUFF_SIZE, "2048"); 
		DEFAULT_PROPS.put(_ESTIMATE_FILE_LINES, "100"); 

		DEFAULT_PROPS.put(_WRITE_ASYNC, "false"); 
		DEFAULT_PROPS.put(_IS_DEBUG_ON, "true"); 

		DEFAULT_PROPS.put(_CHECK_INTERVAL, "3000"); 
		DEFAULT_PROPS.put(_N_THREADS, "10"); 
		DEFAULT_PROPS.put(_EMPTY_STR_CONDITION, "null;NULL"); 
		DEFAULT_PROPS.put(_MAY_BE_FILE_NAME_SEPS, "?"); 

		DEFAULT_PROPS.put(_TASK_BEFORE_LOG_PATTERN, "URL : '${url }' \r\n --------------------- [ '${taskName }' start ... ] --------------------------"); 
		DEFAULT_PROPS.put(_TASK_AFTER_LOG_PATTERN, "FetchedResult : '${result }' \r\n --------------------- [ '${taskName }' end ... ] -------------------------- \r\n spent '${spent }' ms ..."); 
		DEFAULT_PROPS.put(_TASK_EXCEPTION_LOG_PATTERN, "Exception : '${exception }' \r\n while fetch : '${taskName }', url : '${url }'"); 

		// Log 相关
		DEFAULT_PROPS.put(_HORIZON_LINES, "-----------------------------------"); 
		DEFAULT_PROPS.put(_HORIZON_STARS, "***********************************"); 
		DEFAULT_PROPS.put(_GOT_THERE, "get there..."); 
		DEFAULT_PROPS.put(_GOT_NOTHING, "get nothing ~"); 

		DEFAULT_PROPS.put(_BUFF_NAME_PREFIX, "Logger"); 
		DEFAULT_PROPS.put(_BUFF_NAME_SEP, "_"); 

		DEFAULT_PROPS.put(_OUT_TO_CONSOLE, "true"); 
		DEFAULT_PROPS.put(_ERR_TO_CONSOLE, "true"); 
		DEFAULT_PROPS.put(_OUT_TO_LOG_FILE, "false"); 
		DEFAULT_PROPS.put(_ERR_TO_LOG_FILE, "false"); 
		DEFAULT_PROPS.put(_OUT_LOG_BUFF_NAME, "Log.out"); 
		DEFAULT_PROPS.put(_ERR_LOG_BUFF_NAME, "Log.err"); 
		DEFAULT_PROPS.put(_OUT_LOG_FILE_PATH, "F:\\tmp\\log.log");
		DEFAULT_PROPS.put(_ERR_LOG_FILE_PATH, "F:\\tmp\\log.log");

		DEFAULT_PROPS.put(_DEFAULT_SEP_WHILE_CRLF, ""); 
		DEFAULT_PROPS.put(_DEFAULT_SEP_WHILE_NOT_CRLF, ", "); 
		DEFAULT_PROPS.put(_DEFAULT_SEP_WHILE_TWO_DIMEN, ""); 
		DEFAULT_PROPS.put(_DEFAULT_SEP_MAP_KVSEP, "->"); 

		DEFAULT_PROPS.put(_DEFAULT_OUTPUT_APPEND_CRLF, "true"); 
		DEFAULT_PROPS.put(_DEFAULT_ERRPUT_APPEND_CRLF, "true"); 
		DEFAULT_PROPS.put(_DEFAULT_OUTPUT_APPEND_CRLF_FOR_CONTAINER, "false"); 
		DEFAULT_PROPS.put(_DEFAULT_ERRPUT_APPEND_CRLF_FOR_CONTAINER, "false"); 
		DEFAULT_PROPS.put(_DEFAULT_OUTPUT_APPEND_CRLF_FOR_FORMAT, "false"); 
		DEFAULT_PROPS.put(_DEFAULT_ERRPUT_APPEND_CRLF_FOR_FORMAT, "false"); 
		DEFAULT_PROPS.put(_DEFAULT_IS_FORMAT, "true"); 

		DEFAULT_PROPS.put(_PREFIX, ""); 
		DEFAULT_PROPS.put(_CRLF, "\r\n"); 
		DEFAULT_PROPS.put(_DATE_FORMAT, "yyyy-MM-dd HH:mm:ss:SSS"); 
		DEFAULT_PROPS.put(_USE_PATTERN, "true"); 
		DEFAULT_PROPS.put(_LOG_PATTERN, ">>>> ${PREFIX } [${mode }] [${idx }] [${date }] [${thread }] [${stackTrace }]"); 
		DEFAULT_PROPS.put(_LOG_IDX_HANDLER_PARSER, "map('*Logger-' + trim)"); 

		// JSONTransferable 相关
		DEFAULT_PROPS.put(_JSON_TUTILS, "Tools"); 
		DEFAULT_PROPS.put(_JSON_TIDX_MAP_MANAGER, "Constants"); 
		DEFAULT_PROPS.put(_JSON_TID, "ID");
		DEFAULT_PROPS.put(_JSON_TFOR_EACH_ELE, "ele"); 
		DEFAULT_PROPS.put(_JSON_TBEAN_KEY, "BEAN_KEY"); 
		DEFAULT_PROPS.put(_JSON_TPROTO_BEAN_KEY, "PROTO_BEAN"); 
		DEFAULT_PROPS.put(_JSON_TARR_IDX_MAP_KEY, "arrIdxMap"); 
		DEFAULT_PROPS.put(_JSON_TDEFAULT_LOAD_IDX, "CAMEL"); 
		DEFAULT_PROPS.put(_JSON_TDEFAULT_FILTER_IDX, "ALL"); 
		DEFAULT_PROPS.put(_JSON_TIDX_SUFFIX, "Idxes"); 
		DEFAULT_PROPS.put(_JSON_TOBJ_SUFFIX, "Obj"); 
		DEFAULT_PROPS.put(_JSON_TARR_SUFFIX, "Arr"); 

	}

	public static final Set<String> EMPTY_STR_CONDITIONS = new HashSet<>();
	public static final Set<Character> MAYBE_FILE_NAME_SEPS = new HashSet<>();
	static {
		EMPTY_STR_CONDITIONS.add(EMPTY_STR);
		
		String[] fileNameSeps = optString(_MAY_BE_FILE_NAME_SEPS).split(";");
		for(String sep : fileNameSeps) {
			if(! isEmpty0(sep)) {
				MAYBE_FILE_NAME_SEPS.add(sep.charAt(0) );
			}
		}
	}
	public static final OutputStream[] OUT_STREAMS = new OutputStream[] {
																System.out,
																System.err
															};
	public static final boolean[] OUT_TO_LOG_FILES = new boolean[] {
																optBoolean(_OUT_TO_LOG_FILE),
																optBoolean(_ERR_TO_LOG_FILE),																
															};
	public static final String[] LOG_BUFF_SIFFIXES = new String[] {
																optString(_OUT_LOG_BUFF_NAME),
																optString(_ERR_LOG_BUFF_NAME),
															};
	public static final String[] LOG_FILES = new String[] {
																optString(_OUT_LOG_FILE_PATH),
																optString(_ERR_LOG_FILE_PATH),
															};
	public static final OutputStream NULL_OUTPUT_STREAM = new NullOutputStream();
	
	public static final DateFormat DATE_FORMAT = new SimpleDateFormat(optString(_DATE_FORMAT) );
	public static final LogPatternChain JUST_PRINT_MSG_LOG_PATTERN = new LogPatternChain().addLogPattern(new VarLogPattern(Constants.LOG_PATTERN_MSG) );
	public static final LogPatternChain LOG_PATTERN = optBoolean(_USE_PATTERN) ? LogPatternUtils.initLogPattern(optString(_LOG_PATTERN) ) : JUST_PRINT_MSG_LOG_PATTERN;
	public static final OperationAttrHandler LOG_IDX_HANDLER_PARSER = AttrHandlerUtils.handlerParse(optString(_LOG_IDX_HANDLER_PARSER), AttrHandlerConstants.HANDLER);
	
	// 获取相关默认值
	public static String optString(String key, String defaultVal) {
		String val = (PROPS != null) ? String.valueOf(PROPS.get(key)) : null;
		val = (val != null) ? val : DEFAULT_PROPS.get(key);
		
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
		return optLong(key, 0L);
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
	public static boolean isEmpty0(String str) {
		return (str == null) || EMPTY_STR_CONDITIONS.contains(str.trim());
	}

	// 读取配置文件的信息
	private static Exception loadPropsFromConfig(Properties props) {
		boolean isException = true;
		Exception result = null;
		try {
//			InputStream config = new FileInputStream(new File("./src/config.conf") );
			// 前者为true, 后者为false
//			Log.log(Main.class.getClass().getClassLoader() == null);
//			Log.log(new Main().getClass().getClassLoader() == null);
			InputStream config = new Test00HelloWorld().getClass().getClassLoader().getResourceAsStream(CONFIG_PATH);
			props.load(new InputStreamReader(config, DEFAULT_CHARSET) );
			isException = false;
		} catch (Exception e) {
			// ignore
			result = e;
		}
		if(! isException) {
			return null;
		}

		String bootClasspath = System.getProperty("sun.boot.class.path");
		int idxOfHXLog = bootClasspath.indexOf("HXLog");
		if(idxOfHXLog >= 0) {
			try {
				// do nothing, if have no ";" before 'HXLog', then 'hxLogHead+1' is the right position either
				int hxLogHead = bootClasspath.lastIndexOf(";", idxOfHXLog);
				int hxLogTail = bootClasspath.indexOf(";", idxOfHXLog);
				// if there are no ";" after 'HXLog', then 'hxLogTail' take the end of classpath
				if(hxLogTail < 0) {
					hxLogTail = bootClasspath.length();
				}

				String hxLogPath = bootClasspath.substring(hxLogHead+1, hxLogTail);
				JarFile jar = new JarFile(hxLogPath);
				InputStream config = jar.getInputStream(jar.getEntry(CONFIG_PATH));
				props.load(new InputStreamReader(config, DEFAULT_CHARSET));
				isException = false;
			} catch (Exception e) {
				// ignore
				result = e;
			}
			if(! isException) {
				return null;
			}
		}

		return result;
	}
	
}
