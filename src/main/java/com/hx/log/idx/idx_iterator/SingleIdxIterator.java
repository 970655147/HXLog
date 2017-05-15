package com.hx.log.idx.idx_iterator;

import com.hx.common.interf.idx.IdxIterator;

/**
 * ֻ��һ��������IdxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:00 PM
 */
public class SingleIdxIterator implements IdxIterator {

    /**
     * Ψһ��һ��Ԫ��
     */
    private int single;
    /**
     * �Ƿ��������
     */
    private boolean iterated;

    /**
     * ��ʼ��
     *
     * @param single Ψһ��Ԫ��
     * @since 1.0
     */
    public SingleIdxIterator(int single) {
        this.single = single;
    }

    @Override
    public boolean hasNext() {
        return ! iterated;
    }

    @Override
    public int next() {
        if(! hasNext() ) {
            throw new RuntimeException("have no next !");
        }

        iterated = true;
        return single;
    }

}
