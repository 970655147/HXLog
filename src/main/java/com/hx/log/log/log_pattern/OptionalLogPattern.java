package com.hx.log.log.log_pattern;

import com.hx.log.interf.LogPattern;
import com.hx.log.interf.LogPatternType;

/**
 * 可选的LogPattern, 如果某一个param不存在, 则省略掉当前LogPattern
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:25 PM
 */
public class OptionalLogPattern extends LogPattern.OneStringVariableLogPattern {

    public final LogPatternChain chain;
    private String result = null;

    public OptionalLogPattern(LogPatternChain chain, String arg) {
        super(arg);
        this.chain = chain;
    }

    @Override
    public String pattern() {
        if(result != null) {
            return result;
        }

        result = chain.pattern();
        return result;
    }

    @Override
    public LogPatternType type() {
        return LogPatternType.OPTIONAL;
    }

    public void setResult(String result) {
        this.result = result;
    }

}
