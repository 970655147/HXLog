package com.hx.log.idx.idx_iterator;

import com.hx.log.idx.interf.IdxIterator;

/**
 * 对应于一个范围的IdxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:00 PM
 */
public class RangeIdxIterator implements IdxIterator {

//		private int start;
    /**
     * 上限
     */
    private int end;
    /**
     * 当前的索引
     */
    private int cur;

    /**
     * 初始化
     *
     * @param start 起始索引
     * @param end 终止索引
     * @since 1.0
     */
    public RangeIdxIterator(int start, int end) {
//			this.start = start;
        this.end = end;
        this.cur = start;
    }

    @Override
    public boolean hasNext() {
        return cur < end;
    }

    @Override
    public int next() {
        if (!hasNext()) {
            throw new RuntimeException("have no next !");
        }

        return cur++;
    }

}
