package com.hx.log.idx.idx_iterator;

import com.hx.log.interf.IdxIterator;

/**
 * û���κ�������IdxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:59 PM
 */
public class NoneIdxIterator implements IdxIterator {

    private static NoneIdxIterator instance;
    private NoneIdxIterator() {

    }

    // ���߷���
    public static NoneIdxIterator getInstance() {
        if(instance == null) {
            synchronized (NoneIdxIterator.class) {
                if(instance == null) {
                    instance = new NoneIdxIterator();
                }
            }
        }

        return instance;
    }

    @Override
    public boolean hasNext() {
        return false;
    }

    @Override
    public int next() {
        if(! hasNext() ) {
            throw new RuntimeException("have no next !");
        }
        return 0;
    }

}
