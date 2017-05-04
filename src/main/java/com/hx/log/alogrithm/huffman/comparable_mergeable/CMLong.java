package com.hx.log.alogrithm.huffman.comparable_mergeable;

import com.hx.json.JSONObject;
import com.hx.log.alogrithm.huffman.interf.ComparableAndMergeable;

/**
 * CMLong
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 10:03 PM
 */
public class CMLong implements ComparableAndMergeable<CMLong> {

    /**
     * °ü×°µÄval
     */
    public long val;

    public CMLong(long val) {
        this.val = val;
    }

    @Override
    public int compareTo(CMLong other) {
        long delta = this.val - other.val;
        return (delta > 0) ? 1 : ((delta == 0) ? 0 : -1);
    }

    @Override
    public CMLong merge(CMLong other) {
        return new CMLong(this.val + other.val);
    }

    @Override
    public String toString() {
        return new JSONObject().element("val", val)
                .toString();
    }

}
