package com.hx.log.log.log_pattern;

import com.hx.log.log.log_pattern.interf.LogPattern;
import com.hx.log.log.LogPatternType;

import java.text.DateFormat;
import java.util.Date;

/**
 * ������ص�LogPattern
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:16 PM
 */
public class DateLogPattern implements LogPattern {

    /**
     * ��ʽ�����ڵ� format
     */
    private DateFormat dateFormat;

    /**
     * ��ʼ��
     *
     * @param dateFormat ��ʽ�����ڵ� format
     * @since 1.0
     */
    public DateLogPattern(DateFormat dateFormat) {
        this.dateFormat = dateFormat;
    }

    @Override
    public String pattern() {
        return dateFormat.format(new Date());
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
