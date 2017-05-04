package com.hx.log.idx.idx_iterator;

import com.hx.log.idx.idx_filter.LowerBoundsIdxFilter;
import com.hx.log.idx.interf.IdxIterator;
import com.hx.log.util.Tools;

import static com.hx.log.util.Tools.assert0;

/**
 * filter '< min' 的IdxIterator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:13 PM
 */
public class LowerBoundsIdxIterator implements IdxIterator {

    /**
     * 根据目标iterator, 添加上下限之后 组合的idxIterator
     */
    private FilteredIdxIterator ite;

    /**
     * 初始化
     *
     * @param ite        给定的idxIterator
     * @param min        下限
     * @para * @param ite
 * @param min
 * @param containsEqr
     * @date
     * @since 1.0
     */
    public LowerBoundsIdxIterator(IdxIterator ite, int min, boolean containsEq) {
        assert0(ite != null, "ite can't be null !");
        this.ite = new FilteredIdxIterator(ite, new LowerBoundsIdxFilter(min, containsEq));
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
