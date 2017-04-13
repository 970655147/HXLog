package com.hx.log.log.log_pattern;

import com.hx.log.interf.LogPattern;
import com.hx.log.log.LogPatternType;

/**
 * 获取当前线程名称的LogPattern
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:23 PM
 */
public class ThreadLogPattern implements LogPattern {

    @Override
    public String pattern() {
        return Thread.currentThread().getName();
    }

    @Override
    public LogPatternType type() {
        return LogPatternType.THREAD;
    }

    @Override
    public LogPattern copyOf() {
        return this;
    }

}
