package com.hx.log.idx.idx_filter;

import com.hx.log.interf.IdxFilter;

/**
 * 如果给定的索引 >[=] max, 则将其过滤掉
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:14 PM
 */
public class UpperBoundsIdxFilter implements IdxFilter {

    private int max;
    private boolean containsEq;

    public UpperBoundsIdxFilter(int max, boolean containsEq) {
        this.max = max;
        this.containsEq = containsEq;
    }

    @Override
    public boolean filter(int idx) {
        return containsEq ? (idx <= max) : (idx < max);
    }

}
