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
     * Logger��صĿ����ñ�����key
     */
    /**
     * bufferName��ǰ׺
     */
    public static final String _BUFF_NAME_PREFIX = "buffNamePrefix";
    /**
     * bufferName�ĵķָ���[�ָ�idx]
     */
    public static final String _BUFF_NAME_SEP = "buffNameSep";

    /**
     * out �Ƿ����������̨
     */
    public static final String _OUT_TO_CONSOLE = "outToConsole";
    /**
     * err �Ƿ����������̨
     */
    public static final String _ERR_TO_CONSOLE = "errToConsole";
    /**
     * out �Ƿ�������ļ�
     */
    public static final String _OUT_TO_LOG_FILE = "outToLogFile";
    /**
     * err �Ƿ�������ļ�
     */
    public static final String _ERR_TO_LOG_FILE = "errToLogFile";
    /**
     * out ��buffName
     */
    public static final String _OUT_LOG_BUFF_NAME = "outLogBuffName";
    /**
     * err ��buffName
     */
    public static final String _ERR_LOG_BUFF_NAME = "errLogBuffName";
    /**
     * out ��� ���ļ�·��
     */
    public static final String _OUT_LOG_FILE_PATH = "outLogFilePath";
    /**
     * err ��� ���ļ�·��
     */
    public static final String _ERR_LOG_FILE_PATH = "errLogFilePath";

    /**
     * Ĭ�ϵ���crlf���Ԫ�صķָ���
     */
    public static final String _DEFAULT_SEP_WHILE_CRLF = "defaultSepWhileCRLF";
    /**
     * Ĭ�ϵ���crlf���Ԫ�صķָ���
     */
    public static final String _DEFAULT_SEP_WHILE_NOT_CRLF = "defaultSepWhileNotCRLF";
    /**
     * Ĭ�ϵĶ�ά��������ķָ���
     */
    public static final String _DEFAULT_SEP_WHILE_TWO_DIMEN = "defaultSepWhileTwoDimen";
    /**
     * Ĭ�ϵ�kv�ָ���
     */
    public static final String _DEFAULT_SEP_MAP_KVSEP = "defaultMapKVSep";

    /**
     * outĬ����� �Ƿ����crlf
     */
    public static final String _DEFAULT_OUTPUT_APPEND_CRLF = "defaultOutputAppendCrlf";
    /**
     * errĬ����� �Ƿ����crlf
     */
    public static final String _DEFAULT_ERRPUT_APPEND_CRLF = "defaultErrputAppendCrlf";
    /**
     * outĬ�ϼ������ �Ƿ����crlf
     */
    public static final String _DEFAULT_OUTPUT_APPEND_CRLF_FOR_CONTAINER = "defaultOutputAppendCrlfForContainer";
    /**
     * errĬ�ϼ������ �Ƿ����crlf
     */
    public static final String _DEFAULT_ERRPUT_APPEND_CRLF_FOR_CONTAINER = "defaultErrputAppendCrlfForContainer";
    /**
     * out��logPattern��� �Ƿ����crlf
     */
    public static final String _DEFAULT_OUTPUT_APPEND_CRLF_FOR_FORMAT = "defaultOutputAppendCrlfForFormat";
    /**
     * err��logPattern��� �Ƿ����crlf
     */
    public static final String _DEFAULT_ERRPUT_APPEND_CRLF_FOR_FORMAT = "defaultErrputAppendCrlfForFormat";
    /**
     * ���[out, err]��ʱ�� �Ƿ��ʽ��
     */
    public static final String _DEFAULT_IS_FORMAT = "defaultIsFormat";

    /**
     * �Ƿ�ʹ��logPattern
     */
    public static final String _USE_PATTERN = "usePattern";
    /**
     * ����� logPattern����
     */
    public static final String _LOG_PATTERN = "logPattern";
    /**
     * ����Logger��ʱ��, logIdx�Ĵ����handler
     */
    public static final String _LOG_IDX_HANDLER_PARSER = "logIdxHandlerParser";

    /**
     * ˮƽ��
     */
    public static final String _HORIZON_LINES = "horizonLines";
    /**
     * ˮƽ��
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
         * Log ���
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
     * ����Constants�е�Ĭ�ϳ���
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
