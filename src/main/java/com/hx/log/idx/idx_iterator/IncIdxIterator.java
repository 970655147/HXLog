package com.hx.log.idx.idx_iterator;

import com.hx.log.idx.interf.IdxIterator;

/**
 * 一个累增的的IdxIterator '[[start ... end) += step]'
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:01 PM
 */
public class IncIdxIterator implements IdxIterator {

    private int step;
    private int cnt;
    private int cur;
    private int executed;

    public IncIdxIterator(int start, int step, int cnt) {
        this.step = step;
        this.cnt = cnt;
        this.cur = start;
        executed = 0;
    }

    // 创建对应的Inc
    public static IncIdxIterator newEndInc(int start, int step, int end) {
        int cnt = ((end-1) - start) / step + 1;
        return new IncIdxIterator(start, step, cnt);
    }
    public static IncIdxIterator newCntInc(int start, int step, int cnt) {
        return new IncIdxIterator(start, step, cnt);
    }

    @Override
    public boolean hasNext() {
        return executed < cnt;
    }

    @Override
    public int next() {
        if(! hasNext() ) {
            throw new RuntimeException("have no next !");
        }

        int result = cur;
        cur += step;
        executed ++;
        return result;
    }

}
