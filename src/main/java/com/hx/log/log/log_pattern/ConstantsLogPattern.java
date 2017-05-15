package com.hx.log.log.log_pattern;

import com.hx.log.log.log_pattern.interf.LogPattern;
import com.hx.log.log.log_pattern.interf.LogPatternType;

/**
 * 返回常亮的LogPattern
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:16 PM
 */
public class ConstantsLogPattern implements LogPattern {

    /**
     * pattern需要返回的常量
     */
    protected String res;

    /**
     * 初始化
     *
     * @param res pattern需要返回的常量
     * @since 1.0
     */
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
