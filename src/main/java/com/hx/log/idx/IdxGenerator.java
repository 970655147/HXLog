/**
 * file name : IdxGenerator.java
 * created at : 12:19:14 PM May 30, 2016
 * created by 970655147
 */

package com.hx.log.idx;

import com.hx.log.idx.interf.IdxIterator;

import java.util.concurrent.atomic.AtomicInteger;

// 索引生成工具
public class IdxGenerator implements IdxIterator {

    /**
     * 默认起始值
     */
    public static final int DEFAULT_INIT = 0;
    /**
     * 默认的step
     */
    public static final int DEFAULT_STEP = 1;

    /**
     * 生成索引的AtomicInteger
     */
    private AtomicInteger idxGenerator = new AtomicInteger();
    /**
     * 当前IdxGenerator的步长
     */
    private int step;

    /**
     * 初始化
     *
     * @param init 初始值
     * @param step 递增步长
     * @return none
     * @author Jerry.X.He
     * @date 5/2/2017 8:53 PM
     * @since 1.0
     */
    public IdxGenerator(int init, int step) {
        idx(init);
        this.step = step;
    }

    public IdxGenerator(int init) {
        this(init, DEFAULT_STEP);
    }

    public IdxGenerator() {
        this(DEFAULT_INIT, DEFAULT_STEP);
    }

    /**
     * 配置当前索引的值
     *
     * @param idx 给定的值
     * @return void
     * @author Jerry.X.He
     * @date 5/2/2017 8:53 PM
     * @since 1.0
     */
    public void idx(int idx) {
        idxGenerator.set(idx);
    }

    /**
     * 配置当前步长的值
     *
     * @param step 给定的步长
     * @return void
     * @author Jerry.X.He
     * @date 5/2/2017 8:53 PM
     * @since 1.0
     */
    public void step(int step) {
        this.step = step;
    }

    /**
     * 获取当前索引的值
     *
     * @return void
     * @author Jerry.X.He
     * @date 5/2/2017 8:53 PM
     * @since 1.0
     */
    public int idx() {
        return idxGenerator.get();
    }

    /**
     * 获取当前步长
     *
     * @return void
     * @author Jerry.X.He
     * @date 5/2/2017 8:53 PM
     * @since 1.0
     */
    public int step() {
        return step;
    }

    @Override
    public boolean hasNext() {
        return true;
    }

    @Override
    public int next() {
        return idxGenerator.getAndAdd(step);
    }
}
