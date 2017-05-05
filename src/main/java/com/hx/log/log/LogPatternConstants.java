package com.hx.log.log;

import com.hx.log.util.Constants;
import com.hx.common.util.InnerTools;

import java.util.Set;

/**
 * LogPatternConstants
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/1/2017 11:36 AM
 */
public final class LogPatternConstants {

    // disable constructor
    private LogPatternConstants() {
        InnerTools.assert0(false, "can't instantiate !");
    }

    // LogPattern 相关
    /**
     * 变量开始的分隔符
     */
    public static final String VAR_START = "${";
    /**
     * 变量结束的分隔符
     */
    public static final String VAR_END = "}";
    /**
     * OptionalLogPattern开始的分隔符
     */
    public static final String OPT_START = "$[";
    /**
     * OptionalLogPattern结束的分隔符
     */
    public static final String OPT_END = "]";
    /**
     * 左括号 [IdxLogPattern, HandlerPattern需要]
     */
    public static final String LBRACKET = "(";
    /**
     * 右括号 [IdxLogPattern, HandlerPattern需要]
     */
    public static final String RBRACKET = ")";

    // add at 2016.11.23
    /**
     * 顺序输出占位符
     */
    public static final String VAR_PLACE = "{}";
    /**
     * 'logPattern''s seprators                            // use Constants.asSet incase of recursely dependency
     */
    public static final Set<String> LOG_PATTERN_SEPS = InnerTools.asSet(VAR_START, VAR_END, OPT_START, OPT_END, LBRACKET, RBRACKET);

    // fixed pattern
    // add at 2016.04.21
    /**
     * logPatternChain 的typeKey
     */
    public static final String LOG_PATTERN_CHAIN = "logPatternChain";
    /**
     * constants 的typeKey
     */
    public static final String LOG_PATTERN_CONSTANTS = Constants.CONSTANTS;
    /**
     * date 的typeKey
     */
    public static final String LOG_PATTERN_DATE = "date";
    /**
     * idx 的typeKey
     */
    public static final String LOG_PATTERN_IDX = "idx";

    // controllable [variable]
    // add at 2016.04.22
    /**
     * mode 的typeKey
     */
    public static final String LOG_PATTERN_MODE = "mode";
    /**
     * var 的typeKey
     */
    public static final String LOG_PATTERN_VAR = "var";
    /**
     * msg 的typeKey
     */
    public static final String LOG_PATTERN_MSG = "msg";
    /**
     * logIdx 的typeKey
     */
    public static final String LOG_PATTERN_LOG_IDX = "logIdx";
    /**
     * handler 的typeKey
     */
    public static final String LOG_PATTERN_HANDLER = Constants.HANDLER;
    // add at 2016.04.29
    /**
     * thread 的typeKey
     */
    public static final String LOG_PATTERN_THREAD = "thread";
    /**
     * stackTrace 的typeKey
     */
    public static final String LOG_PATTERN_STACK_TRACE = "stackTrace";
    // add at 2016.11.05
    /**
     * lineInfo 的typeKey
     */
    public static final String LOG_PATTERN_LINE_INFO = "lineInfo";
    // add at 2016.09.23
    /**
     * optional 的typeKey
     */
    public static final String LOG_PATTERN_OPTIONAL = "optional";

    // add at 2016.04.23
    /**
     * 一些业务相关的常量[被VarLogPattern替代]
     */
    /**
     * taskName
     */
    public static final String LOG_PATTERN_TASK_NAME = "taskName";
    /**
     * url
     */
    public static final String LOG_PATTERN_URL = "url";
    /**
     * result
     */
    public static final String LOG_PATTERN_RESULT = "result";
    /**
     * spepnt
     */
    public static final String LOG_PATTERN_SPENT = "spent";
    /**
     * exception
     */
    public static final String LOG_PATTERN_EXCEPTION = "exception";

    /**
     * default 'VariableValue'[${var }], and supported two 'mode'
     */
    public static final String DEFAULT_VAR_VALUE = "VarNotFound";

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
