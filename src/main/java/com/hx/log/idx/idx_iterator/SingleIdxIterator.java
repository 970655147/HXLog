package com.hx.log.idx.idx_iterator;

import com.hx.common.interf.idx.IdxIterator;

/**
 * 只有一个索引的IdxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:00 PM
 */
public class SingleIdxIterator implements IdxIterator {

    /**
     * 唯一的一个元素
     */
    private int single;
    /**
     * 是否迭代过了
     */
    private boolean iterated;

    /**
     * 初始化
     *
     * @param single 唯一的元素
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
