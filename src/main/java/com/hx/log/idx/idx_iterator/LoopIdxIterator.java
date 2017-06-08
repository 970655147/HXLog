package com.hx.log.idx.idx_iterator;

import com.hx.common.interf.idx.IdxIterator;
import com.hx.log.idx.idx_filter.NoneIdxFilter;
import com.hx.log.util.Tools;

/**
 * ѭ��ʹ�ø����� IdxIterator �� idxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 6/8/2017 7:24 PM
 */
public class LoopIdxIterator implements IdxIterator {

    /**
     * Ŀ�� IdxIterator
     */
    private IdxIterator idxIterator;
    /**
     * idxIterator �ĸ���
     */
    private IdxIterator copyOf;

    public LoopIdxIterator(IdxIterator idxIterator) {
        Tools.assert0(idxIterator != null, "'idxIterator' can't be null !");

        this.idxIterator = idxIterator;
        this.copyOf = this.idxIterator.copy();
    }

    @Override
    public boolean hasNext() {
        boolean resultOfCur = idxIterator.hasNext();
        if(resultOfCur) {
            return true;
        }

        boolean resultOfCopy = copyOf.hasNext();
        if(resultOfCopy) {
            idxIterator = copyOf;
            copyOf = idxIterator.copy();
            return true;
        }
        return false;
    }

    @Override
    public int next() {
        if(! hasNext()) {
            throw new RuntimeException("have no next !");
        }

        return idxIterator.next();
    }

    @Override
    public IdxIterator copy() {
        LoopIdxIterator result = new LoopIdxIterator(NoneIdxIterator.getInstance());
        result.idxIterator = this.idxIterator.copy();
        result.copyOf = this.copyOf.copy();
        return result;
    }
}
