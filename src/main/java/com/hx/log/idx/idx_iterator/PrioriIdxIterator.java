package com.hx.log.idx.idx_iterator;

import com.hx.common.interf.idx.IdxIterator;
import com.hx.log.collection.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

import static com.hx.log.util.Tools.assert0;

/**
 * һ�����ϵ�IdxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:11 PM
 */
public class PrioriIdxIterator implements IdxIterator {

    /**
     * ��ϵĶ�һϵ�е� idxIteraotr
     */
    public List<IdxIterator> chain;

    /**
     * ��ʼ��
     *
     * @param chain ������idxIteratorCha * @param chaine 1.0
     */
    public PrioriIdxIterator(List<IdxIterator> chain) {
        assert0(chain != null, "chain can't be null !");
        assert0(!CollectionUtils.isAnyNull(chain), "some of 'idxIterator' is null, please check that !");
        this.chain = chain;
    }

    public PrioriIdxIterator() {
        this(new ArrayList<IdxIterator>());
    }

    public PrioriIdxIterator add(IdxIterator idxIterator) {
        this.chain.add(idxIterator);
        return this;
    }

    @Override
    public boolean hasNext() {
        return locateHasNext() >= 0;
    }

    @Override
    public int next() {
        int idx = locateHasNext();
        if (idx < 0) {
            throw new RuntimeException("have no next !");
        }

        return chain.get(idx).next();
    }

    @Override
    public IdxIterator copy() {
        PrioriIdxIterator result = new PrioriIdxIterator();
        for (IdxIterator idxIterator : this.chain) {
            result.add(idxIterator.copy());
        }
        return result;
    }

    /**
     * ��ȡ��һ�� ����һ��Ԫ�� ��idxIterator ������
     *
     * @return int
     * @author Jerry.X.He
     * @date 6/8/2017 7:32 PM
     * @since 1.0
     */
    private int locateHasNext() {
        int idx = 0;
        for (IdxIterator idxIterator : chain) {
            if (idxIterator.hasNext()) {
                return idx;
            }
            idx++;
        }

        return -1;
    }

}
