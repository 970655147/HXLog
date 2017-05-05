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
     * Tools相关key的常量
     */
    /**
     * 临时文件的名字
     */
    public static final String _TMP_NAME = "tmpName";
    /**
     * 临时文件夹
     */
    public static final String _TMP_DIR = "tmpDir";
    /**
     * 默认的后缀
     */
    public static final String _SUFFIX = "suffix";
    /**
     * 默认的buffSize
     */
    public static final String _BUFF_SIZE = "buffSize";
    /**
     * 估计文件的行数
     */
    public static final String _ESTIMATE_FILE_LINES = "estimateFileLines";

    /**
     * 是否异步处理写操作
     */
    public static final String _WRITE_ASYNC = "writeAsync";
    /**
     * Tools.assert0 是否生效
     */
    public static final String _IS_DEBUG_ON = "isDebugOn";

    /**
     * 检查线程池的周期
     */
    public static final String _CHECK_INTERVAL = "checkInterval";
    /**
     * 线程池的线程的刷领
     */
    public static final String _N_THREADS = "nThreads";

    /**
     * 爬虫任务开始前需要打印的数据的LogPattern
     */
    public static final String _TASK_BEFORE_LOG_PATTERN = "taskBeforeLogPattern";
    /**
     * 爬虫任务结束之后需要打印的数据的LogPattern
     */
    public static final String _TASK_AFTER_LOG_PATTERN = "taskAfterLogPattern";
    /**
     * 爬虫任务异常情况下需要打印的数据的LogPattern
     */
    public static final String _TASK_EXCEPTION_LOG_PATTERN = "taskExceptionLogPattern";

    static {
        /**
         * Tools 相关
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

}
