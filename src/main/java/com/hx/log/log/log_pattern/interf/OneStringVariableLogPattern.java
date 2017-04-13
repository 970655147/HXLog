package com.hx.log.log.log_pattern.interf;

import com.hx.log.interf.LogPattern;

/**
 * 一个参数变量的LogPattern
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 11:23 AM
 */
public abstract class OneStringVariableLogPattern implements LogPattern {

    protected String arg;

    public OneStringVariableLogPattern(String arg) {
        setArg(arg);
    }

    /**
     * 配置参数, 以及默认的pattern实现
     *
     * @return
     * @author Jerry.X.He
     * @date 4/12/2017 10:20 PM
     * @since 1.0
     */
    public void setArg(String arg) {
        this.arg = arg;
    }

    @Override
    public String pattern() {
        return arg;
    }

    @Override
    public LogPattern copyOf() {
        return this;
    }

}
