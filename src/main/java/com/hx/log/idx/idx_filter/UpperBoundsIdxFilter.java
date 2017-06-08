package com.hx.log.idx.idx_filter;

import com.hx.common.interf.idx.IdxFilter;

/**
 * 如果给定的索引 >[=] max, 则将其过滤掉
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:14 PM
 */
public class UpperBoundsIdxFilter implements IdxFilter {

    /**
     * 上限
     */
    private int max;
    /**
     * 是否包含上限
     */
    private boolean containsEq;

    /**
     * 初始化
     *
     * @param max        上限
     * @param containsEq 是否包含上限
     * @since 1.0
     */
    public UpperBoundsIdxFilter(int max, boolean containsEq) {
        this.max = max;
        this.containsEq = containsEq;
    }

    @Override
    public boolean filter(int idx) {
        return containsEq ? (idx <= max) : (idx < max);
    }

    @Override
    public IdxFilter copy() {
        return new UpperBoundsIdxFilter(max, containsEq);
    }
}
