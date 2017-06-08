package com.hx.log.idx.idx_iterator;

import com.hx.log.idx.idx_filter.UpperBoundsIdxFilter;
import com.hx.common.interf.idx.IdxIterator;

import static com.hx.log.util.Tools.assert0;

/**
 * filter '> max' ��IdxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:13 PM
 */
public class UpperBoundsIdxIterator implements IdxIterator {

    /**
     * ����Ŀ��iterator, ���������֮�� ��ϵ�idxIterator
     */
    private FilteredIdxIterator ite;

    /**
     * ��ʼ��
     *
     * @param ite        ������idxIterator
     * @param max        ����
     * @param containsEq �Ƿ��������
     */
    public UpperBoundsIdxIterator(IdxIterator ite, int max, boolean containsEq) {
        assert0(ite != null, "ite can't be null !");
        this.ite = new FilteredIdxIterator(ite, new UpperBoundsIdxFilter(max, containsEq));
    }

    public UpperBoundsIdxIterator(IdxIterator ite, int max) {
        this(ite, max, true);
    }

    @Override
    public boolean hasNext() {
        return ite.hasNext();
    }

    @Override
    public int next() {
        return ite.next();
    }

    @Override
    public IdxIterator copy() {
        return ite.copy();
    }
}
