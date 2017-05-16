/**
 * file name : Constants.java
 * created at : 8:06:27 PM Jul 24, 2015
 * created by 970655147
 */

package com.hx.log.util;

import com.hx.attr_handler.attr_handler.operation.interf.OperationAttrHandler;
import com.hx.attr_handler.util.AttrHandlerConstants;
import com.hx.attr_handler.util.AttrHandlerUtils;
import com.hx.common.io.NullOutputStream;
import com.hx.common.util.InnerTools;
import com.hx.log.json.JSONTransferableUtilsConstants;
import com.hx.log.log.LogLevel;
import com.hx.log.log.LogPatternConstants;
import com.hx.log.log.LogPatternUtils;
import com.hx.log.log.LoggerConstants;
import com.hx.log.log.log_pattern.LogPatternChain;
import com.hx.log.log.log_pattern.VarLogPattern;
import com.hx.log.test.Test00HelloWorld;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.jar.JarFile;

/**
 * 常量配置
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 7:14 PM
 */
public final class Constants {

    // disable constructor
    private Constants() {
        InnerTools.assert0(false, "can't instantiate !");
    }

    // ----------------------------------- 相关业务常量 ------------------------------------------
    // Constants
    /**
     * 空白字符串
     */
    public static final String EMPTY_STR = "";
    /**
     * crlf
     */
    public static final String CRLF = "\r\n";
    /**
     * null
     */
    public static final String NULL = "null";
    /**
     * 默认的值
     */
    public static final String DEFAULT_VALUE = NULL;
    /**
     * constants
     */
    public static final String CONSTANTS = "constants";
    /**
     * handler
     */
    public static final String HANDLER = "handler";
    /**
     * true
     */
    public static final String TRUE = Boolean.TRUE.toString();
    /**
     * false
     */
    public static final String FALSE = Boolean.FALSE.toString();

    // Constants
    /**
     * \
     */
    public static final Character SLASH = '\\';
    /**
     * /
     */
    public static final Character INV_SLASH = '/';
    /**
     * ?
     */
    public static final Character QUESTION = '?';
    /**
     * .
     */
    public static final Character DOT = '.';
    /**
     * ,
     */
    public static final Character COMMA = ',';
    /**
     * :
     */
    public static final Character COLON = ':';
    /**
     *
     */
    public static final Character SPACE = ' ';
    /**
     * \t
     */
    public static final Character TAB = '\t';
    /**
     * \r
     */
    public static final Character CR = '\r';
    /**
     * \n
     */
    public static final Character LF = '\n';
    /**
     * "
     */
    public static final Character QUOTE = '\"';
    /**
     * '
     */
    public static final Character SINGLE_QUOTE = '\'';

    /**
     * 默认的字符集
     * utf-8 or gbk denpend on container
     */
    public static final String DEFAULT_CHARSET = Charset.defaultCharset().name();

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

    /**
     * out的输出索引
     */
    public static final int OUT_IDX = 0;
    /**
     * err的输出索引
     */
    public static final int ERR_IDX = OUT_IDX + 1;
    /**
     * 两个输出索引的引用方式
     */
    public static final Set<String> LOG_MODES_STR = InnerTools.asSet("Constants.OUT_IDX", "Constants.ERR_IDX");
    /**
     * 两个输出方式的字符串表示
     */
    public static final String[] LOG_MODES = {"LOG", "ERROR"};
    /**
     * 最小的日志输出级别
     */
    private static final LogLevel LOG_LEVEL_MIN = LogLevel.LOG;

    /**
     * add at 2017.04.03, define "HXLogConfig.conf" as constants
     */
    public static final String CONFIG_PATH = "HXLogConfig.conf";

    // updated at 2016.06.28
    // ----------------------------------- 相关可配置数据的初始化 ------------------------------------------

    /**
     * 从配置文件中读取的配置信息
     */
    static Map<String, String> PROPS = null;
    /**
     * 默认的配置
     */
    public static final Map<String, String> DEFAULT_PROPS = new HashMap<>();

    /**
     * 读取配置文件
     */
    static {
        Properties props = new Properties();
        Exception e = loadPropsFromConfig(props);

        if (e != null) {
            e.printStackTrace();
        } else {
            Constants.PROPS = new HashMap<>();
            for (Object _key : props.keySet()) {
                String key = String.valueOf(_key);
                Constants.PROPS.put(key, props.getProperty(key));
            }
        }

        props = null;        // help gc
    }

    // update at 2016.04.21
    // ------------------------ 业务相关常量[从Log, Tools中提取] ---------------------------------
    // 增加一个配置
    // 1. Constants中增加对应的key[配置文件中的名称]
    // 2. 在defaultProps中增加其默认值
    // 3. 在使用的地方进行使用[Constants.optXXX]

    /**
     * 配置的相关key的常量
     */
    /**
     * 空白字符串判定的条件
     */
    public static final String _EMPTY_STR_CONDITION = "emptyStrCondition";
    /**
     * 可能是文件分隔符的字符串
     */
    public static final String _MAY_BE_FILE_NAME_SEPS = "mayBeFileNameSeps";

    /**
     * PREFIX变量
     */
    public static final String _PREFIX = "PREFIX";
    /**
     * CRLF
     */
    public static final String _CRLF = "CRLF";
    /**
     * 默认的日期格式
     */
    public static final String _DATE_FORMAT = "dateFormat";

    /**
     * getter 的前缀
     */
    public static final String _REFLECT_GETTER_PREFIX = "getterPrefix";
    /**
     * setter 的前缀
     */
    public static final String _REFLECT_SETTER_PREFIX = "setterPrefix";

    /**
     * comment的相关符号
     */
    public static final String _COMMENT_MARKS = "commentMarks";

    /**
     * 添加默认配置
     */
    static {
        Constants.DEFAULT_PROPS.put(_EMPTY_STR_CONDITION, "null;NULL");
        Constants.DEFAULT_PROPS.put(_MAY_BE_FILE_NAME_SEPS, "?");
        Constants.DEFAULT_PROPS.put(_PREFIX, "");
        Constants.DEFAULT_PROPS.put(_CRLF, "\r\n");
        Constants.DEFAULT_PROPS.put(_DATE_FORMAT, "yyyy-MM-dd HH:mm:ss:SSS");

        DEFAULT_PROPS.put(_REFLECT_GETTER_PREFIX, "get,is,has");
        DEFAULT_PROPS.put(_REFLECT_SETTER_PREFIX, "set,is,has");

        DEFAULT_PROPS.put(_COMMENT_MARKS, "//,--,#,;,rem");

    }

    /**
     * put default constants
     */
    static {
        JSONTransferableUtilsConstants.loadDefaults();
        LogPatternConstants.loadDefaults();
        ToolsConstants.loadDefaults();
        LoggerConstants.loadDefaults();
    }

    /**
     * 注释的符号                                  -- add at 2016.11.23
     */
    public static final Set<String> COMMENT_MARKS = new HashSet<>();
    /**
     * 最长的注释符号的长度
     */
    public static final int COMMENT_MAX_LEN;

    /**
     * 初始化注释的符号, 以及最长的注释的长度
     */
    static {
        COMMENT_MARKS.addAll(InnerTools.asSet(optString(_COMMENT_MARKS).split(",")));

        int maxLen = 1;
        for (String mark : COMMENT_MARKS) {
            if (mark.length() > maxLen) {
                maxLen = mark.length();
            }
        }
        COMMENT_MAX_LEN = maxLen;
    }

    /**
     * 空字符串的集合
     */
    public static final Set<String> EMPTY_STR_CONDITIONS = new HashSet<>();
    /**
     * 可能是文件分隔符的集合
     */
    public static final Set<Character> MAYBE_FILE_NAME_SEPS = new HashSet<>();

    /**
     * 初始化
     */
    static {
        EMPTY_STR_CONDITIONS.add(EMPTY_STR);

        String[] fileNameSeps = optString(_MAY_BE_FILE_NAME_SEPS).split(";");
        for (String sep : fileNameSeps) {
            if (!InnerTools.isEmpty(sep)) {
                MAYBE_FILE_NAME_SEPS.add(sep.charAt(0));
            }
        }
    }

    /**
     * NullOutputStream
     */
    public static final OutputStream NULL_OUTPUT_STREAM = new NullOutputStream();
    /**
     * 默认的日期格式
     */
    public static final DateFormat DATE_FORMAT = new SimpleDateFormat(optString(Constants._DATE_FORMAT));
    /**
     * 只输出消息的LogPattern
     */
    public static final LogPatternChain JUST_PRINT_MSG_LOG_PATTERN = new LogPatternChain()
            .addLogPattern(new VarLogPattern(LogPatternConstants.LOG_PATTERN_MSG));
    /**
     * 从配置文件中读取的LogPattern
     */
    public static final LogPatternChain LOG_PATTERN = optBoolean(LoggerConstants._USE_PATTERN) ?
            LogPatternUtils.initLogPattern(optString(LoggerConstants._LOG_PATTERN)) : JUST_PRINT_MSG_LOG_PATTERN;
    /**
     * LogIdxHandler
     */
    public static final OperationAttrHandler LOG_IDX_HANDLER_PARSER = AttrHandlerUtils.handlerParse(optString(LoggerConstants._LOG_IDX_HANDLER_PARSER), AttrHandlerConstants.HANDLER);

    /**
     * add for 'ObjectAlreadyExsists' in 'JSONTransferableUtils.encapJSON'		 add at 2016.06.19
     */
    public static final String OBJECT_ALREADY_EXISTS = "{\"info\" : \"ObjectAlreadyExsists\"}";
    // 别让Constants 依赖于Tools, 否则initDependency 又出现了,, 呵呵呵呵 			2016.06.20
//	public static final Set<String> EMPTY_INIT_OBJ_FILTER = Tools.asSet();
    /**
     * 不限制任何属性的 INIT_OBJ_FILTER
     */
    public static final Set<String> EMPTY_INIT_OBJ_FILTER = new HashSet<>();

    /**
     * getter 的前缀	            	-- add at 2017.04.30
     */
    public static final Set<String> BEAN_GETTER_PREFIXES = InnerTools.asSet(optString(_REFLECT_GETTER_PREFIX).split(","));
    /**
     * setter 的前缀
     */
    public static final Set<String> BEAN_SETTER_PREFIXES = InnerTools.asSet(optString(_REFLECT_SETTER_PREFIX).split(","));


    // ----------------- 获取配置的的部分方法 -----------------------

    /**
     * getter for String, Int, Long, Float, Double
     *
     * @param key        key
     * @param defaultVal 默认值
     * @return
     */
    public static String optString(String key, String defaultVal) {
        String val = (PROPS != null) ? PROPS.get(key) : null;
        val = (val != null) ? val : DEFAULT_PROPS.get(key);

        if (val == null) {
            return defaultVal;
        }
        return val;
    }

    public static int optInt(String key, int defaultVal) {
        String val = optString(key);

        if (InnerTools.isEmpty(val)) {
            return defaultVal;
        }
        return Integer.parseInt(val);
    }

    public static long optLong(String key, long defaultVal) {
        String val = optString(key);

        if (InnerTools.isEmpty(val)) {
            return defaultVal;
        }
        return Long.parseLong(val);
    }

    public static boolean optBoolean(String key, boolean defaultVal) {
        String val = optString(key);

        if (InnerTools.isEmpty(val)) {
            return defaultVal;
        }
        return Boolean.parseBoolean(val);
    }

    public static double optDouble(String key, double defaultVal) {
        String val = optString(key);

        if (InnerTools.isEmpty(val)) {
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

    // ----------------- 辅助方法 -----------------------

    /**
     * 读取配置文件的信息, 首先尝试从classloader中加载, 然后尝试从jar中加载
     *
     * @param props
     * @return java.lang.Exception
     * @author Jerry.X.He
     * @date 5/5/2017 7:12 PM
     * @since 1.0
     */
    private static Exception loadPropsFromConfig(Properties props) {
        boolean isException = true;
        Exception result = null;
        try {
//			InputStream config = new FileInputStream(new File("./src/config.conf") );
            // 前者为true, 后者为false
//			Log.log(Main.class.getClass().getClassLoader() == null);
//			Log.log(new Main().getClass().getClassLoader() == null);
            InputStream config = new Test00HelloWorld().getClass().getClassLoader().getResourceAsStream(CONFIG_PATH);
            props.load(new InputStreamReader(config, DEFAULT_CHARSET));
            isException = false;
        } catch (Exception e) {
            // ignore
            result = e;
        }
        if (!isException) {
            return null;
        }

        String bootClasspath = System.getProperty("sun.boot.class.path");
        int idxOfHXLog = bootClasspath.indexOf("HXLog");
        if (idxOfHXLog >= 0) {
            try {
                // do nothing, if have no ";" before 'HXLog', then 'hxLogHead+1' is the right position either
                int hxLogHead = bootClasspath.lastIndexOf(";", idxOfHXLog);
                int hxLogTail = bootClasspath.indexOf(";", idxOfHXLog);
                // if there are no ";" after 'HXLog', then 'hxLogTail' take the end of classpath
                if (hxLogTail < 0) {
                    hxLogTail = bootClasspath.length();
                }

                String hxLogPath = bootClasspath.substring(hxLogHead + 1, hxLogTail);
                JarFile jar = new JarFile(hxLogPath);
                InputStream config = jar.getInputStream(jar.getEntry(CONFIG_PATH));
                props.load(new InputStreamReader(config, DEFAULT_CHARSET));
                isException = false;
            } catch (Exception e) {
                // ignore
                result = e;
            }
            if (!isException) {
                return null;
            }
        }

        return result;
    }


}
