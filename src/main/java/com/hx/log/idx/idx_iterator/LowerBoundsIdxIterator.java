package com.hx.log.idx.idx_iterator;

import com.hx.log.idx.idx_filter.LowerBoundsIdxFilter;
import com.hx.log.idx.interf.IdxIterator;
import com.hx.log.util.Tools;

/**
 * filter '< min' µÄIdxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:13 PM
 */
public class LowerBoundsIdxIterator implements IdxIterator {

    private FilteredIdxIterator ite;

    public LowerBoundsIdxIterator(IdxIterator ite, int min, boolean containsEq) {
        Tools.assert0(ite != null, "ite can't be null !");
        this.ite = new FilteredIdxIterator(ite, new LowerBoundsIdxFilter(min, containsEq) );
    }
    public LowerBoundsIdxIterator(IdxIterator ite, int min) {
        this(ite, min, true);
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
