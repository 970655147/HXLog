package com.hx.log.idx.idx_iterator;

import com.hx.log.idx.interf.IdxIterator;
import com.hx.log.util.Tools;

import java.util.ArrayList;
import java.util.List;

/**
 * 一个复合的IdxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:11 PM
 */
public class IdxIteratorChain implements IdxIterator {

    public List<IdxIterator> chain;
    public int curIdx;

    public IdxIteratorChain() {
        this(new ArrayList<IdxIterator>() );
    }
    public IdxIteratorChain(List<IdxIterator> chain) {
        Tools.assert0(chain != null, "chain can't be null !");
        this.chain = chain;
        this.curIdx = 0;
    }
    public IdxIteratorChain add(IdxIterator idxIterator) {
        this.chain.add(idxIterator);
        return this;
    }

    @Override
    public boolean hasNext() {
        if(chain == null) {
            return false;
        }
        if(curIdx >= chain.size() ) {
            return false;
        }
        if(chain.get(curIdx).hasNext() ) {
            return true;
        }
        while(((++ curIdx) < chain.size() ) && chain.get(curIdx).hasNext() ) {
            return true;
        }

        return false;
    }

    @Override
    public int next() {
        if(! hasNext() ) {
            throw new RuntimeException("have no next !");
        }
        return chain.get(curIdx).next();
    }

}
