package com.hx.log.log.log_pattern;

import com.hx.log.log.log_pattern.interf.LogPattern;
import com.hx.log.log.log_pattern.interf.LogPatternType;
import com.hx.log.util.Constants;

/**
 * 获取当前调用的代码的行信息
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:24 PM
 */
public class LineInfoLogPattern implements LogPattern {

    @Override
    public String pattern() {
        StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();
        int idx = StackTraceLogPattern.positionLogStackTraceIdx(stackTraceElements);
        if(idx < 0) {
            // can't be there
            return Constants.EMPTY_STR;
        }
        return stackTraceElements[idx].getFileName() + " - " + stackTraceElements[idx].getLineNumber();
    }

    @Override
    public LogPatternType type() {
        return LogPatternType.LINE_INFO;
    }

    @Override
    public LogPattern copyOf() {
        return this;
    }

}
