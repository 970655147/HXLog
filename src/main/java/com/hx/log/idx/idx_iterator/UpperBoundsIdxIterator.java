package com.hx.log.idx.idx_iterator;

import com.hx.log.idx.idx_filter.UpperBoundsIdxFilter;
import com.hx.log.interf.IdxIterator;
import com.hx.log.util.Tools;

/**
 * filter '> max' µÄIdxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:13 PM
 */
public class UpperBoundsIdxIterator implements IdxIterator {

    private FilteredIdxIterator ite;

    public UpperBoundsIdxIterator(IdxIterator ite, int max, boolean containsEq) {
        Tools.assert0(ite != null, "ite can't be null !");
        this.ite = new FilteredIdxIterator(ite, new UpperBoundsIdxFilter(max, containsEq) );
    }
    public UpperBoundsIdxIterator(IdxIterator ite, int max) {
        this(ite, max, true);
    }

    @Override
    public boolean hasNext() {
        return ite.hasNext();
    }

    @Override
    public int next() {
        return ite.next();
    }

}
