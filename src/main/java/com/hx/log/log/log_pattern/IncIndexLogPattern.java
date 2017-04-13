package com.hx.log.log.log_pattern;

import com.hx.log.idx.IdxGenerator;
import com.hx.log.interf.LogPattern;
import com.hx.log.log.LogPatternType;

/**
 * �����������µ�LogPattern, ���ص�������
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:20 PM
 */
public class IncIndexLogPattern implements LogPattern {

    private IdxGenerator idxGenerator;

    public IncIndexLogPattern(int initVal, int inc) {
        idxGenerator = new IdxGenerator(initVal, inc);
    }

    @Override
    public String pattern() {
        return String.valueOf(idxGenerator.nextId());
    }

    @Override
    public LogPatternType type() {
        return LogPatternType.INC_IDX;
    }

    @Override
    public LogPattern copyOf() {
        return new IncIndexLogPattern(idxGenerator.idx(), idxGenerator.step());
    }

}
