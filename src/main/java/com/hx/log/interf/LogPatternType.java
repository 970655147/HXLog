package com.hx.log.interf;

import com.hx.log.util.Constants;

/**
 * LogPattern������
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:13 PM
 */
public enum LogPatternType {

    // LogPattern��[ͨ����˵�������LogPattern]
    // Log.log���LogPattern
    // Tools.logBefore / logAfter��ص�LogPattern
    PATTERN_CHAIN(Constants.LOG_PATTERN_CHAIN),
    DATE(Constants.LOG_PATTERN_DATE), CONSTANTS(Constants.LOG_PATTERN_CONSTANTS), VAR(Constants.LOG_PATTERN_VAR), INC_IDX(Constants.LOG_PATTERN_IDX), HANDLER(Constants.LOG_PATTERN_HANDLER),
    THREAD(Constants.LOG_PATTERN_THREAD), STACK_TRACE(Constants.LOG_PATTERN_STACK_TRACE), LINE_INFO(Constants.LOG_PATTERN_LINE_INFO),
    OPTIONAL(Constants.LOG_PATTERN_OPTIONAL);

    // typeKey
    private String typeKey;

    private LogPatternType(String typeKey) {
        this.typeKey = typeKey;
    }

    /**
     * ��ȡ��ǰLogPatternType��key
     *
     * @return return type key identify current LogPatternType
     * @author Jerry.X.He
     * @date 4/12/2017 10:18 PM
     * @since 1.0
     */
    public String typeKey() {
        return typeKey;
    }

}
