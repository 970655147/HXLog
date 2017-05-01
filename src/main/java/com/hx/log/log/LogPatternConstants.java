package com.hx.log.log;

import com.hx.log.util.Constants;
import com.hx.log.util.InnerTools;
import com.hx.log.util.Tools;

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
    public static final String VAR_START = "${";
    public static final String VAR_END = "}";
    public static final String OPT_START = "$[";
    public static final String OPT_END = "]";
    public static final String LBRACKET = "(";
    public static final String RBRACKET = ")";

    // add at 2016.11.23
    // XX占位符
    public static final String VAR_PLACE = "{}";
    // 'logPattern''s seprators                            // use Constants.asSet incase of recursely dependency
    public static final Set<String> LOG_PATTERN_SEPS = InnerTools.asSet(VAR_START, VAR_END, OPT_START, OPT_END, LBRACKET, RBRACKET);

    // fixed pattern
    // add at 2016.04.21
    public static final String LOG_PATTERN_CHAIN = "logPatternChain";
    public static final String LOG_PATTERN_CONSTANTS = Constants.CONSTANTS;
    public static final String LOG_PATTERN_DATE = "date";
    public static final String LOG_PATTERN_IDX = "idx";

    // controllable [variable]
    // add at 2016.04.22
    public static final String LOG_PATTERN_MODE = "mode";
    public static final String LOG_PATTERN_VAR = "var";
    public static final String LOG_PATTERN_MSG = "msg";
    public static final String LOG_PATTERN_LOG_IDX = "logIdx";
    public static final String LOG_PATTERN_HANDLER = Constants.HANDLER;
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
