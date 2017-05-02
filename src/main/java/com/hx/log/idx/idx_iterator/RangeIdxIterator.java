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
    private int end;
    private int cur;

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
        if(! hasNext() ) {
            throw new RuntimeException("have no next !");
        }

        return cur ++;
    }

}
