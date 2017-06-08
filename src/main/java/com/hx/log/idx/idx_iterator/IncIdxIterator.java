package com.hx.log.idx.idx_iterator;

import com.hx.common.interf.idx.IdxIterator;

/**
 * һ�������ĵ�IdxIterator '[[start ... end) += step]'
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:01 PM
 */
public class IncIdxIterator implements IdxIterator {

    /**
     * ����
     */
    private int step;
    /**
     * ���������Ĵ���
     */
    private int cnt;
    /**
     * ��ǰ����
     */
    private int cur;
    /**
     * �Ѿ�ִ���˵Ĵ���
     */
    private int executed;

    /**
     * ��ʼ��
     *
     * @param start ��ʼ������
     * @param step  ����
     * @param cnt   ���������Ĵ���
     * @since 1.0
     */
    public IncIdxIterator(int start, int step, int cnt) {
        this.step = step;
        this.cnt = cnt;
        this.cur = start;
        executed = 0;
    }

    // ----------------- ���߷��� -----------------------

    /**
     * ����, start; inc; end ����IncIdxIterator
     *
     * @param start ��ʼ������
     * @param step  ����
     * @param end   ��ֹ������
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
     * ����, start; inc; cnt ����IncIdxIterator
     *
     * @param start ��ʼ������
     * @param step  ����
     * @param cnt   ��Ҫ�����Ĵ���
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
