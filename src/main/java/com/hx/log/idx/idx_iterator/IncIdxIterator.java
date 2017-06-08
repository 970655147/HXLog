package com.hx.log.idx.idx_iterator;

import com.hx.common.interf.idx.IdxIterator;

/**
 * 一个累增的的IdxIterator '[[start ... end) += step]'
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:01 PM
 */
public class IncIdxIterator implements IdxIterator {

    /**
     * 步长
     */
    private int step;
    /**
     * 可以增长的次数
     */
    private int cnt;
    /**
     * 当前索引
     */
    private int cur;
    /**
     * 已经执行了的次数
     */
    private int executed;

    /**
     * 初始化
     *
     * @param start 起始的索引
     * @param step  步长
     * @param cnt   可以增长的次数
     * @since 1.0
     */
    public IncIdxIterator(int start, int step, int cnt) {
        this.step = step;
        this.cnt = cnt;
        this.cur = start;
        executed = 0;
    }

    // ----------------- 工具方法 -----------------------

    /**
     * 根据, start; inc; end 创建IncIdxIterator
     *
     * @param start 起始的索引
     * @param step  步长
     * @param end   终止的索引
     * @return com.hx.log.idx.idx_iterator.IncIdxIterator
     * @author Jerry.X.He
     * @date 5/5/2017 12:01 AM
     * @since 1.0
     */
    public static IncIdxIterator newEndInc(int start, int step, int end) {
        int cnt = ((end - 1) - start) / step + 1;
        return new IncIdxIterator(start, step, cnt);
    }

    /**
     * 根据, start; inc; cnt 创建IncIdxIterator
     *
     * @param start 起始的索引
     * @param step  步长
     * @param cnt   需要增长的次数
     * @return com.hx.log.idx.idx_iterator.IncIdxIterator
     * @author Jerry.X.He
     * @date 5/5/2017 12:01 AM
     * @since 1.0
     */
    public static IncIdxIterator newCntInc(int start, int step, int cnt) {
        return new IncIdxIterator(start, step, cnt);
    }

    @Override
    public boolean hasNext() {
        return executed < cnt;
    }

    @Override
    public int next() {
        if (!hasNext()) {
            throw new RuntimeException("have no next !");
        }

        int result = cur;
        cur += step;
        executed++;
        return result;
    }

    @Override
    public IdxIterator copy() {
        IncIdxIterator result = new IncIdxIterator(1, 2, 2);
        result.cur = this.cur;
        result.step = this.step;
        result.cnt = this.cnt;
        result.executed = this.executed;
        return result;
    }
}
