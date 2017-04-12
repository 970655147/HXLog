package com.hx.log.log.log_pattern;

import com.hx.log.interf.LogPattern;
import com.hx.log.interf.LogPatternType;

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

    private List<LogPattern> chain = new ArrayList<>();
    private String result = null;

    // 初始化
    public LogPatternChain() {
    }
    public LogPatternChain(List<LogPattern> chain) {
        this.chain = chain;
    }
    public String pattern() {
        if(result != null) {
            return result;
        }

        StringBuilder sb = new StringBuilder();
        for(LogPattern logPat : chain) {
            sb.append(logPat.pattern() );
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
        List<LogPattern> newChain = new ArrayList<>(this.chain.size() );
        for(LogPattern logPat : this.chain) {
            newChain.add(logPat.copyOf() );
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
