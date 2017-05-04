package com.hx.log.alogrithm.huffman.comparable_mergeable;

import com.hx.common.math.GeometryUtils;
import com.hx.json.JSONObject;
import com.hx.log.alogrithm.huffman.interf.ComparableAndMergeable;

/**
 * CMDouble
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 10:04 PM
 */
public class CMDouble implements ComparableAndMergeable<CMDouble> {

    /**
     * °ü×°µÄval
     */
    public double val;

    public CMDouble(double val) {
        this.val = val;
    }

    @Override
    public int compareTo(CMDouble other) {
        double delta = this.val - other.val;
        return (delta > 0.0) ? 1 : (GeometryUtils.equals(delta, 0.0d) ? 0 : -1);
    }

    @Override
    public CMDouble merge(CMDouble other) {
        return new CMDouble(this.val + other.val);
    }

    @Override
    public String toString() {
        return new JSONObject().element("val", val)
                .toString();
    }

}
