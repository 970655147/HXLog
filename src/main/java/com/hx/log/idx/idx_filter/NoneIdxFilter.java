package com.hx.log.idx.idx_filter;

import com.hx.common.interf.idx.IdxFilter;
import com.hx.log.idx.idx_iterator.NoneIdxIterator;

/**
 * 什么都不过滤的 idxFilter
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 6/8/2017 7:16 PM
 */
public class NoneIdxFilter implements IdxFilter {

    /**
     * 单例
     */
    private static NoneIdxFilter INSTANCE;

    private NoneIdxFilter() {
    }

    /**
     * 获取一个NoneIdxIterator
     *
     * @return com.hx.log.idx.idx_iterator.NoneIdxIterator
     * @author Jerry.X.He
     * @date 5/5/2017 12:04 AM
     * @since 1.0
     */
    public static NoneIdxFilter getInstance() {
        if(INSTANCE == null) {
            synchronized (NoneIdxFilter.class) {
                if(INSTANCE == null) {
                    INSTANCE = new NoneIdxFilter();
                }
            }
        }

        return INSTANCE;
    }

    @Override
    public boolean filter(int idx) {
        return false;
    }

    @Override
    public IdxFilter copy() {
        return getInstance();
    }
}
