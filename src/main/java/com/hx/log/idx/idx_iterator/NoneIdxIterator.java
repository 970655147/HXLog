package com.hx.log.idx.idx_iterator;

import com.hx.log.idx.interf.IdxIterator;

/**
 * û���κ�������IdxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:59 PM
 */
public class NoneIdxIterator implements IdxIterator {

    /**
     * ����
     */
    private static NoneIdxIterator INSTANCE;

    /**
     * ��ʼ��
     *
     * @since 1.0
     */
    private NoneIdxIterator() {

    }

    /**
     * ��ȡһ��NoneIdxIterator
     *
     * @return com.hx.log.idx.idx_iterator.NoneIdxIterator
     * @author Jerry.X.He
     * @date 5/5/2017 12:04 AM
     * @since 1.0
     */
    public static NoneIdxIterator getInstance() {
        if(INSTANCE == null) {
            synchronized (NoneIdxIterator.class) {
                if(INSTANCE == null) {
                    INSTANCE = new NoneIdxIterator();
                }
            }
        }

        return INSTANCE;
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
