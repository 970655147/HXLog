package com.hx.log.log.log_pattern;

import com.hx.log.log.log_pattern.interf.LogPattern;
import com.hx.log.log.LogPatternType;
import com.hx.log.log.LogPatternUtils;
import com.hx.log.log.Logger;
import com.hx.log.util.Constants;
import com.hx.log.util.Log;
import com.hx.log.util.Tools;

import java.util.Set;

/**
 * 获取当前StackTrace相关的LogPattern [执行的代码的信息]
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:23 PM
 */
public class StackTraceLogPattern implements LogPattern {

    // intercept first method that called "Log / Logger / LoggerPatternUtils"
    static Set<String> loggerClazzNames = Tools.asSet(Logger.class.getName(),
            Log.class.getName(), LogPatternUtils.class.getName() );

    @Override
    public String pattern() {
        StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();
        int idx = positionLogStackTraceIdx(stackTraceElements);
        if(idx < 0) {
            // can't be there
            return Constants.EMPTY_STR;
        }

        String className = stackTraceElements[idx].getClassName();
        int lastIdxOfDot = className.lastIndexOf(".");
        String trimedClassName = (lastIdxOfDot > 0) ? className.substring(className.lastIndexOf(".") + 1) : className;
        return trimedClassName + "." + stackTraceElements[idx].getMethodName();
    }

    @Override
    public LogPatternType type() {
        return LogPatternType.STACK_TRACE;
    }

    @Override
    public LogPattern copyOf() {
        return this;
    }

    static int positionLogStackTraceIdx(StackTraceElement[] stackTraceElements) {
        int idx = -1;
        for(int i=stackTraceElements.length-1; i>=0; i--) {
            StackTraceElement stackTraceElement = stackTraceElements[i];
            if(loggerClazzNames.contains(stackTraceElement.getClassName()) ) {
                idx = i + 1;
                break ;
            }
        }
        return idx;
    }

}
