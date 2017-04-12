package com.hx.log.log.log_pattern;

import com.hx.log.interf.LogPattern;
import com.hx.log.interf.LogPatternType;

/**
 * �ַ�����Ӧ��һ������, �����ʱ��, ��context�л�ȡ������ֵ��LogPattern
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:17 PM
 */
public class VarLogPattern extends LogPattern.OneStringVariableLogPattern {

    public final String key;

    // ע�� �������õ���key...
    public VarLogPattern(String key) {
        super(key);
        this.key = key;
    }

    @Override
    public LogPatternType type() {
        return LogPatternType.VAR;
    }

}
