package com.hx.log.util;

import com.hx.common.util.InnerTools;

/**
 * ToolsConstants
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/1/2017 12:00 PM
 */
public final class ToolsConstants {

    // disable constructor
    private ToolsConstants() {
        InnerTools.assert0(false, "can't instantiate !");
    }

    /**
     * Tools���key�ĳ���
     */
    /**
     * ��ʱ�ļ�������
     */
    public static final String _TMP_NAME = "tmpName";
    /**
     * ��ʱ�ļ���
     */
    public static final String _TMP_DIR = "tmpDir";
    /**
     * Ĭ�ϵĺ�׺
     */
    public static final String _SUFFIX = "suffix";
    /**
     * Ĭ�ϵ�buffSize
     */
    public static final String _BUFF_SIZE = "buffSize";
    /**
     * �����ļ�������
     */
    public static final String _ESTIMATE_FILE_LINES = "estimateFileLines";

    /**
     * �Ƿ��첽����д����
     */
    public static final String _WRITE_ASYNC = "writeAsync";
    /**
     * Tools.assert0 �Ƿ���Ч
     */
    public static final String _IS_DEBUG_ON = "isDebugOn";

    /**
     * ����̳߳ص�����
     */
    public static final String _CHECK_INTERVAL = "checkInterval";
    /**
     * �̳߳ص��̵߳�ˢ��
     */
    public static final String _N_THREADS = "nThreads";

    /**
     * ��������ʼǰ��Ҫ��ӡ�����ݵ�LogPattern
     */
    public static final String _TASK_BEFORE_LOG_PATTERN = "taskBeforeLogPattern";
    /**
     * �����������֮����Ҫ��ӡ�����ݵ�LogPattern
     */
    public static final String _TASK_AFTER_LOG_PATTERN = "taskAfterLogPattern";
    /**
     * ���������쳣�������Ҫ��ӡ�����ݵ�LogPattern
     */
    public static final String _TASK_EXCEPTION_LOG_PATTERN = "taskExceptionLogPattern";

    static {
        /**
         * Tools ���
         */
        Constants.DEFAULT_PROPS.put(_TMP_NAME, "tmp");
        Constants.DEFAULT_PROPS.put(_TMP_DIR, "F:\\tmp");
        Constants.DEFAULT_PROPS.put(_SUFFIX, ".html");
        Constants.DEFAULT_PROPS.put(_BUFF_SIZE, "2048");
        Constants.DEFAULT_PROPS.put(_ESTIMATE_FILE_LINES, "100");

        Constants.DEFAULT_PROPS.put(_WRITE_ASYNC, "false");
        Constants.DEFAULT_PROPS.put(_IS_DEBUG_ON, "true");

        Constants.DEFAULT_PROPS.put(_CHECK_INTERVAL, "3000");
        Constants.DEFAULT_PROPS.put(_N_THREADS, "10");

        Constants.DEFAULT_PROPS.put(_TASK_BEFORE_LOG_PATTERN, "URL : '${url }' \r\n --------------------- [ '${taskName }' start ... ] --------------------------");
        Constants.DEFAULT_PROPS.put(_TASK_AFTER_LOG_PATTERN, "FetchedResult : '${result }' \r\n --------------------- [ '${taskName }' end ... ] -------------------------- \r\n spent '${spent }' ms ...");
        Constants.DEFAULT_PROPS.put(_TASK_EXCEPTION_LOG_PATTERN, "Exception : '${exception }' \r\n while fetch : '${taskName }', url : '${url }'");
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

}
