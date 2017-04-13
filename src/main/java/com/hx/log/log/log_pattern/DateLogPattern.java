package com.hx.log.log.log_pattern;

import com.hx.log.interf.LogPattern;
import com.hx.log.log.LogPatternType;

import java.text.DateFormat;
import java.util.Date;

/**
 * 日期相关的LogPattern
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:16 PM
 */
public class DateLogPattern implements LogPattern {

    private DateFormat dateFormat;

    public DateLogPattern(DateFormat dateFormat) {
        this.dateFormat = dateFormat;
    }

    @Override
    public String pattern() {
        return dateFormat.format(new Date() );
    }

    @Override
    public LogPatternType type() {
        return LogPatternType.DATE;
    }

    @Override
    public LogPattern copyOf() {
        return this;
    }

}
