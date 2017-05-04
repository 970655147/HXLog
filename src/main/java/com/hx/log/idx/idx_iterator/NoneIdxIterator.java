package com.hx.log.idx.idx_iterator;

import com.hx.log.idx.interf.IdxIterator;

/**
 * 没有任何索引的IdxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:59 PM
 */
public class NoneIdxIterator implements IdxIterator {

    /**
     * 单例
     */
    private static NoneIdxIterator INSTANCE;

    /**
     * 初始化
     *
     * @since 1.0
     */
    private NoneIdxIterator() {

    }

    /**
     * 获取一个NoneIdxIterator
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
