package com.hx.log.log.log_pattern;

import com.hx.log.log.LogPatternType;
import com.hx.log.log.log_pattern.interf.OneStringVariableLogPattern;

/**
 * 可选的LogPattern, 如果某一个param不存在, 则省略掉当前LogPattern
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:25 PM
 */
public class OptionalLogPattern extends OneStringVariableLogPattern {

    /**
     * 处理业务的LogPatternChain
     */
    public final LogPatternChain chain;
    /**
     * 缓存的结果
     */
    private String result = null;

    /**
     * 初始化
     *
     * @param chain 委托的LogPatternChain
     * @param arg   参数
     * @since 1.0
     */
    public OptionalLogPattern(LogPatternChain chain, String arg) {
        super(arg);
        this.chain = chain;
    }

    @Override
    public String pattern() {
        if (result != null) {
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
