package com.hx.log.log.log_pattern;

import com.hx.log.log.LogPatternType;
import com.hx.log.log.log_pattern.interf.OneStringVariableLogPattern;

/**
 * ��ѡ��LogPattern, ���ĳһ��param������, ��ʡ�Ե���ǰLogPattern
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:25 PM
 */
public class OptionalLogPattern extends OneStringVariableLogPattern {

    /**
     * ����ҵ���LogPatternChain
     */
    public final LogPatternChain chain;
    /**
     * ����Ľ��
     */
    private String result = null;

    /**
     * ��ʼ��
     *
     * @param chain ί�е�LogPatternChain
     * @param arg   ����
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
