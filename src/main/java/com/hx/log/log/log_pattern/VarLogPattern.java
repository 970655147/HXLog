package com.hx.log.log.log_pattern;

import com.hx.log.log.log_pattern.interf.LogPatternType;
import com.hx.log.log.log_pattern.interf.OneStringVariableLogPattern;

/**
 * 字符串对应于一个变量, 计算的时候, 从context中获取变量的值的LogPattern
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:17 PM
 */
public class VarLogPattern extends OneStringVariableLogPattern {

    /**
     * 当前var对应的key
     */
    public final String key;

    // 注意 这里配置的是key...
    public VarLogPattern(String key) {
        super(key);
        this.key = key;
    }

    @Override
    public LogPatternType type() {
        return LogPatternType.VAR;
    }

}
