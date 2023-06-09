package com.hx.log.log;

import com.hx.log.util.Constants;
import com.hx.common.util.InnerTools;

import java.io.OutputStream;

/**
 * LoggerConstants
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/1/2017 11:48 AM
 */
public final class LoggerConstants {

    // disable constructor
    private LoggerConstants() {
        InnerTools.assert0(false, "can't instantiate !");
    }

    /**
     * Logger相关的可配置变量的key
     */
    /**
     * bufferName的前缀
     */
    public static final String _BUFF_NAME_PREFIX = "buffNamePrefix";
    /**
     * bufferName的的分隔符[分割idx]
     */
    public static final String _BUFF_NAME_SEP = "buffNameSep";

    /**
     * out 是否输出到控制台
     */
    public static final String _OUT_TO_CONSOLE = "outToConsole";
    /**
     * err 是否输出到控制台
     */
    public static final String _ERR_TO_CONSOLE = "errToConsole";
    /**
     * out 是否输出到文件
     */
    public static final String _OUT_TO_LOG_FILE = "outToLogFile";
    /**
     * err 是否输出到文件
     */
    public static final String _ERR_TO_LOG_FILE = "errToLogFile";
    /**
     * out 的buffName
     */
    public static final String _OUT_LOG_BUFF_NAME = "outLogBuffName";
    /**
     * err 的buffName
     */
    public static final String _ERR_LOG_BUFF_NAME = "errLogBuffName";
    /**
     * out 输出 的文件路径
     */
    public static final String _OUT_LOG_FILE_PATH = "outLogFilePath";
    /**
     * err 输出 的文件路径
     */
    public static final String _ERR_LOG_FILE_PATH = "errLogFilePath";

    /**
     * 默认的有crlf输出元素的分隔符
     */
    public static final String _DEFAULT_SEP_WHILE_CRLF = "defaultSepWhileCRLF";
    /**
     * 默认的无crlf输出元素的分隔符
     */
    public static final String _DEFAULT_SEP_WHILE_NOT_CRLF = "defaultSepWhileNotCRLF";
    /**
     * 默认的二维数组输出的分隔符
     */
    public static final String _DEFAULT_SEP_WHILE_TWO_DIMEN = "defaultSepWhileTwoDimen";
    /**
     * 默认的kv分隔符
     */
    public static final String _DEFAULT_SEP_MAP_KVSEP = "defaultMapKVSep";

    /**
     * out默认输出 是否输出crlf
     */
    public static final String _DEFAULT_OUTPUT_APPEND_CRLF = "defaultOutputAppendCrlf";
    /**
     * err默认输出 是否输出crlf
     */
    public static final String _DEFAULT_ERRPUT_APPEND_CRLF = "defaultErrputAppendCrlf";
    /**
     * out默认集合输出 是否输出crlf
     */
    public static final String _DEFAULT_OUTPUT_APPEND_CRLF_FOR_CONTAINER = "defaultOutputAppendCrlfForContainer";
    /**
     * err默认集合输出 是否输出crlf
     */
    public static final String _DEFAULT_ERRPUT_APPEND_CRLF_FOR_CONTAINER = "defaultErrputAppendCrlfForContainer";
    /**
     * out的logPattern输出 是否输出crlf
     */
    public static final String _DEFAULT_OUTPUT_APPEND_CRLF_FOR_FORMAT = "defaultOutputAppendCrlfForFormat";
    /**
     * err的logPattern输出 是否输出crlf
     */
    public static final String _DEFAULT_ERRPUT_APPEND_CRLF_FOR_FORMAT = "defaultErrputAppendCrlfForFormat";
    /**
     * 输出[out, err]的时候 是否格式化
     */
    public static final String _DEFAULT_IS_FORMAT = "defaultIsFormat";

    /**
     * 是否使用logPattern
     */
    public static final String _USE_PATTERN = "usePattern";
    /**
     * 输出的 logPattern配置
     */
    public static final String _LOG_PATTERN = "logPattern";
    /**
     * 创建Logger的时候, logIdx的处理的handler
     */
    public static final String _LOG_IDX_HANDLER_PARSER = "logIdxHandlerParser";

    /**
     * 水平线
     */
    public static final String _HORIZON_LINES = "horizonLines";
    /**
     * 水平星
     */
    public static final String _HORIZON_STARS = "horizonStars";
    /**
     * got there
     */
    public static final String _GOT_THERE = "gotThere";
    /**
     * got nothing
     */
    public static final String _GOT_NOTHING = "gotNothing";

    static {
        /**
         * Log 相关
         */
        Constants.DEFAULT_PROPS.put(_BUFF_NAME_PREFIX, "Logger");
        Constants.DEFAULT_PROPS.put(_BUFF_NAME_SEP, "_");

        Constants.DEFAULT_PROPS.put(_OUT_TO_CONSOLE, "true");
        Constants.DEFAULT_PROPS.put(_ERR_TO_CONSOLE, "true");
        Constants.DEFAULT_PROPS.put(_OUT_TO_LOG_FILE, "false");
        Constants.DEFAULT_PROPS.put(_ERR_TO_LOG_FILE, "false");
        Constants.DEFAULT_PROPS.put(_OUT_LOG_BUFF_NAME, "Log.out");
        Constants.DEFAULT_PROPS.put(_ERR_LOG_BUFF_NAME, "Log.err");
        Constants.DEFAULT_PROPS.put(_OUT_LOG_FILE_PATH, "F:\\tmp\\log.log");
        Constants.DEFAULT_PROPS.put(_ERR_LOG_FILE_PATH, "F:\\tmp\\log.log");

        Constants.DEFAULT_PROPS.put(_DEFAULT_SEP_WHILE_CRLF, "");
        Constants.DEFAULT_PROPS.put(_DEFAULT_SEP_WHILE_NOT_CRLF, ", ");
        Constants.DEFAULT_PROPS.put(_DEFAULT_SEP_WHILE_TWO_DIMEN, "");
        Constants.DEFAULT_PROPS.put(_DEFAULT_SEP_MAP_KVSEP, "->");

        Constants.DEFAULT_PROPS.put(_DEFAULT_OUTPUT_APPEND_CRLF, "true");
        Constants.DEFAULT_PROPS.put(_DEFAULT_ERRPUT_APPEND_CRLF, "true");
        Constants.DEFAULT_PROPS.put(_DEFAULT_OUTPUT_APPEND_CRLF_FOR_CONTAINER, "false");
        Constants.DEFAULT_PROPS.put(_DEFAULT_ERRPUT_APPEND_CRLF_FOR_CONTAINER, "false");
        Constants.DEFAULT_PROPS.put(_DEFAULT_OUTPUT_APPEND_CRLF_FOR_FORMAT, "false");
        Constants.DEFAULT_PROPS.put(_DEFAULT_ERRPUT_APPEND_CRLF_FOR_FORMAT, "false");
        Constants.DEFAULT_PROPS.put(_DEFAULT_IS_FORMAT, "true");

        Constants.DEFAULT_PROPS.put(_USE_PATTERN, "true");
        Constants.DEFAULT_PROPS.put(_LOG_PATTERN, ">>>> ${PREFIX } [${mode }] [${idx }] [${date }] [${thread }] [${stackTrace }] => ${msg}");
        Constants.DEFAULT_PROPS.put(_LOG_IDX_HANDLER_PARSER, "map('*Logger-' + trim)");

        Constants.DEFAULT_PROPS.put(_HORIZON_LINES, "-----------------------------------");
        Constants.DEFAULT_PROPS.put(_HORIZON_STARS, "***********************************");
        Constants.DEFAULT_PROPS.put(_GOT_THERE, "get there...");
        Constants.DEFAULT_PROPS.put(_GOT_NOTHING, "get nothing ~");
    }

    /**
     * 加载Constants中的默认常量
     *
     * @return void
     * @author Jerry.X.He
     * @date 5/1/2017 12:14 PM
     * @since 1.0
     */
    public static void loadDefaults() {
        // invoke classloader load current class
    }

    public static final OutputStream[] OUT_STREAMS = new OutputStream[]{
            System.out,
            System.err
    };
    public static final boolean[] OUT_TO_LOG_FILES = new boolean[]{
            Constants.optBoolean(LoggerConstants._OUT_TO_LOG_FILE),
            Constants.optBoolean(LoggerConstants._ERR_TO_LOG_FILE),
    };
    public static final String[] LOG_BUFF_SIFFIXES = new String[]{
            Constants.optString(LoggerConstants._OUT_LOG_BUFF_NAME),
            Constants.optString(LoggerConstants._ERR_LOG_BUFF_NAME),
    };
    public static final String[] LOG_FILES = new String[]{
            Constants.optString(LoggerConstants._OUT_LOG_FILE_PATH),
            Constants.optString(LoggerConstants._ERR_LOG_FILE_PATH),
    };


}
