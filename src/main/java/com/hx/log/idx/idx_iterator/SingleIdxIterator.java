package com.hx.log.idx.idx_iterator;

import com.hx.log.idx.interf.IdxIterator;

/**
 * 只有一个索引的IdxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:00 PM
 */
public class SingleIdxIterator implements IdxIterator {

    private int single;
    private boolean iterated;

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
