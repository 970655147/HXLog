package com.hx.log.idx.idx_iterator;

import com.hx.log.collection.CollectionUtils;
import com.hx.common.interf.idx.IdxIterator;

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
public class IdxIteratorChain implements IdxIterator {

    /**
     * ��ϵĶ�һϵ�е� idxIteraotr
     */
    public List<IdxIterator> chain;
    /**
     * ��ǰ���ڶ�ȡ[��Ч]��idxIterator����
     */
    public int curIdx;

    /**
     * ��ʼ��
     *
     * @param chain ������idxIteratorCha * @param chaine 1.0
     */
    public IdxIteratorChain(List<IdxIterator> chain) {
        assert0(chain != null, "chain can't be null !");
        assert0(!CollectionUtils.isAnyNull(chain), "some of 'idxIterator' is null, please check that !");
        this.chain = chain;
        this.curIdx = 0;
    }

    public IdxIteratorChain() {
        this(new ArrayList<IdxIterator>());
    }

    public IdxIteratorChain add(IdxIterator idxIterator) {
        this.chain.add(idxIterator);
        return this;
    }

    @Override
    public boolean hasNext() {
        if (chain == null) {
            return false;
        }
        if (curIdx >= chain.size()) {
            return false;
        }
        if (chain.get(curIdx).hasNext()) {
            return true;
        }

        while (((++curIdx) < chain.size()) && chain.get(curIdx).hasNext()) {
            return true;
        }
        return false;
    }

    @Override
    public int next() {
        if (!hasNext()) {
            throw new RuntimeException("have no next !");
        }
        return chain.get(curIdx).next();
    }

}
