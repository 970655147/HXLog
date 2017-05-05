package com.hx.log.log.log_pattern;

import com.hx.log.log.log_pattern.interf.LogPattern;
import com.hx.log.log.LogPatternType;

import java.util.ArrayList;
import java.util.List;

/**
 * 一个复合的LogPattern
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:15 PM
 */
public class LogPatternChain implements LogPattern {

    /**
     * 处理业务所需的 一系列LogPattern
     */
    private List<LogPattern> chain = new ArrayList<>();
    /**
     * 缓存的结果
     */
    private String result = null;

    /**
     * 初始化
     *
     * @param chain 给定的一系列的LogPatternChain
     * @since 1.0
     */
    public LogPatternChain(List<LogPattern> chain) {
        this.chain = chain;
    }

    public LogPatternChain() {
    }

    public String pattern() {
        if (result != null) {
            return result;
        }

        StringBuilder sb = new StringBuilder();
        for (LogPattern logPat : chain) {
            sb.append(logPat.pattern());
        }
        result = sb.toString();
        return result;
    }

    @Override
    public LogPatternType type() {
        return LogPatternType.PATTERN_CHAIN;
    }

    @Override
    public LogPatternChain copyOf() {
        List<LogPattern> newChain = new ArrayList<>(this.chain.size());
        for (LogPattern logPat : this.chain) {
            newChain.add(logPat.copyOf());
        }
        return new LogPatternChain(newChain);
    }

    public LogPatternChain addLogPattern(LogPattern logPattern) {
        this.chain.add(logPattern);
        return this;
    }

    public void setResult(String result) {
        this.result = result;
    }

    public List<LogPattern> getChain() {
        return this.chain;
    }

}
