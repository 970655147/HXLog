package com.hx.log.alogrithm.huffman.comparable_mergeable;

import com.hx.json.JSONObject;
import com.hx.log.alogrithm.huffman.HuffmanUtils;
import com.hx.log.alogrithm.huffman.interf.ComparableAndMergeable;

/**
 * CMInteger
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 10:02 PM
 */
public class CMInteger implements ComparableAndMergeable<CMInteger> {

    /**
     * °ü×°µÄval
     */
    public int val;

    public CMInteger(int val) {
        this.val = val;
    }

    @Override
    public int compareTo(CMInteger other) {
        return this.val - other.val;
    }

    @Override
    public CMInteger merge(CMInteger other) {
        return new CMInteger(this.val + other.val);
    }

    @Override
    public String toString() {
        return new JSONObject().element("val", val)
                .toString();
    }

}
