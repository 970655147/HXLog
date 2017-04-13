package com.hx.log.log.log_pattern;

import com.hx.log.interf.LogPattern;
import com.hx.log.log.LogPatternType;

/**
 * ·µ»Ø³£ÁÁµÄLogPattern
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:16 PM
 */
public class ConstantsLogPattern implements LogPattern {

    protected String res;

    public ConstantsLogPattern(String res) {
        this.res = res;
    }

    @Override
    public String pattern() {
        return res;
    }

    @Override
    public LogPatternType type() {
        return LogPatternType.CONSTANTS;
    }

    @Override
    public LogPattern copyOf() {
        return this;
    }

}
