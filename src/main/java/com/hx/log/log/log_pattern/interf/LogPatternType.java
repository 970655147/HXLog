package com.hx.log.log.log_pattern.interf;

import com.hx.log.log.LogPatternConstants;

/**
 * LogPattern的类型
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:13 PM
 */
public enum LogPatternType {

    // LogPattern链[通常来说是最顶级的LogPattern]
    // Log.log相关LogPattern
    // Tools.logBefore / logAfter相关的LogPattern
    /**
     * logPatterChain
     */
    PATTERN_CHAIN(LogPatternConstants.LOG_PATTERN_CHAIN),
    /**
     * date($dateFormat)
     */
    DATE(LogPatternConstants.LOG_PATTERN_DATE),
    /**
     * constants($constants)
     */
    CONSTANTS(LogPatternConstants.LOG_PATTERN_CONSTANTS),
    /**
     * var($var)
     */
    VAR(LogPatternConstants.LOG_PATTERN_VAR),
    /**
     * idx(start, inc)
     */
    INC_IDX(LogPatternConstants.LOG_PATTERN_IDX),
    /**
     * handler('map(trim)')
     */
    HANDLER(LogPatternConstants.LOG_PATTERN_HANDLER),
    /**
     * thread
     */
    THREAD(LogPatternConstants.LOG_PATTERN_THREAD),
    /**
     * stackTrace
     */
    STACK_TRACE(LogPatternConstants.LOG_PATTERN_STACK_TRACE),
    /**
     * lineInfo
     */
    LINE_INFO(LogPatternConstants.LOG_PATTERN_LINE_INFO),
    /**
     * $[$logPatternChain ]
     */
    OPTIONAL(LogPatternConstants.LOG_PATTERN_OPTIONAL);

    // typeKey
    private String typeKey;

    private LogPatternType(String typeKey) {
        this.typeKey = typeKey;
    }

    /**
     * 获取当前LogPatternType的key
     *
     * @return return type key identify currentStartIdx LogPatternType
     * @author Jerry.X.He
     * @date 4/12/2017 10:18 PM
     * @since 1.0
     */
    public String typeKey() {
        return typeKey;
    }

}
