package com.hx.log.idx.idx_iterator;

import com.hx.common.interf.idx.IdxFilter;
import com.hx.common.interf.idx.IdxIterator;

/**
 * 过滤掉某些索引的IdxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:03 PM
 */
public class FilteredIdxIterator implements IdxIterator {

    /**
     * 组合的idxIteraotr
     */
    private IdxIterator ite;
    /**
     * 组合的 idxFilter
     */
    private IdxFilter filter;
    /**
     * 缓冲的下一个索引
     */
    private int next;

    /**
     * 初始化
     *
     * @param ite idxIteraotr
     * @param filter idxFilter
     * @since 1.0
     */
    public FilteredIdxIterator(IdxIterator ite, IdxFilter filter) {
        this.ite = ite;
        this.filter = filter;
        this.next = -1;
    }

    @Override
    public boolean hasNext() {
        if (next >= 0) {
            return true;
        }
        while (ite.hasNext()) {
            int _next = ite.next();
            if (filter.filter(_next)) {
                next = _next;
                break;
            }
        }

        return (next >= 0);
    }

    @Override
    public int next() {
        if (!hasNext()) {
            throw new RuntimeException("have no next !");
        }
        int result = next;
        next = -1;
        return result;
    }

}
