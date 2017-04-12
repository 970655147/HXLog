package com.hx.log.idx.idx_filter;

import com.hx.log.interf.IdxFilter;

/**
 * ������������� <[=] min, ������˵�
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:15 PM
 */
public class LowerBoundsIdxFilter implements IdxFilter {

    private int min;
    private boolean containsEq;

    public LowerBoundsIdxFilter(int min, boolean containsEq) {
        this.min = min;
        this.containsEq = containsEq;
    }

    @Override
    public boolean filter(int idx) {
        return containsEq ? (idx >= min) : (idx > min);
    }

}
