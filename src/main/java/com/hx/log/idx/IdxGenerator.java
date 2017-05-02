/**
 * file name : IdxGenerator.java
 * created at : 12:19:14 PM May 30, 2016
 * created by 970655147
 */

package com.hx.log.idx;

import com.hx.log.idx.interf.IdxIterator;

import java.util.concurrent.atomic.AtomicInteger;

// �������ɹ���
public class IdxGenerator implements IdxIterator {

    /**
     * Ĭ����ʼֵ
     */
    public static final int DEFAULT_INIT = 0;
    /**
     * Ĭ�ϵ�step
     */
    public static final int DEFAULT_STEP = 1;

    /**
     * ����������AtomicInteger
     */
    private AtomicInteger idxGenerator = new AtomicInteger();
    /**
     * ��ǰIdxGenerator�Ĳ���
     */
    private int step;

    /**
     * ��ʼ��
     *
     * @param init ��ʼֵ
     * @param step ��������
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
     * ���õ�ǰ������ֵ
     *
     * @param idx ������ֵ
     * @return void
     * @author Jerry.X.He
     * @date 5/2/2017 8:53 PM
     * @since 1.0
     */
    public void idx(int idx) {
        idxGenerator.set(idx);
    }

    /**
     * ���õ�ǰ������ֵ
     *
     * @param step �����Ĳ���
     * @return void
     * @author Jerry.X.He
     * @date 5/2/2017 8:53 PM
     * @since 1.0
     */
    public void step(int step) {
        this.step = step;
    }

    /**
     * ��ȡ��ǰ������ֵ
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
     * ��ȡ��ǰ����
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
