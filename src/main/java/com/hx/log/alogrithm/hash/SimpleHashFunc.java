package com.hx.log.alogrithm.hash;

import com.hx.log.alogrithm.hash.interf.HashFunc;

/**
 * 一个简单的HashFunc的实现, * 种子, 然后取模
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:35 PM
 */
public class SimpleHashFunc<T> implements HashFunc<T> {
    /**
     * hash 取模的阈值
     */
    private int cap;
    /**
     * 计算hash的种子
     */
    private int seed;
    /**
     * cap 是否为 "整数"
     */
    private boolean isInteger;

    /**
     * 初始化
     *
     * @param cap  hash 取模的阈值
     * @param seed 计算hash的种子
     * @since 1.0
     */
    public SimpleHashFunc(int cap, int seed) {
        this.cap = cap;
        isInteger = isInteger(cap);
        this.seed = seed;
    }

    /**
     * 计算给定的val的hash, 简单的累加hash函数
     *
     * @param value 给定的对象
     * @return int the hash current HashFunc caclulated
     * @author Jerry.X.He
     * @date 5/4/2017 9:47 PM
     * @since 1.0
     */
    @Override
    public int hash(T value) {
        if (value == null) {
            return 0;
        }

        int result = 0;
        String valStr = String.valueOf(value);
        int len = valStr.length();
        for (int i = 0; i < len; i++) {
            result = seed * result + valStr.charAt(i);
        }

        if (isInteger) {
            return result & (cap - 1);
        }

        return result % cap;
    }

    // ----------------- 辅助方法 -----------------------

    /**
     * 判断给定的num是否为'二进制整数'
     *
     * @param num 给定的数字
     * @return boolean
     * @author Jerry.X.He
     * @date 5/4/2017 9:48 PM
     * @since 1.0
     */
    private static boolean isInteger(int num) {
        return ((num & (num - 1)) == 0);
//			return (Integer.bitCount(cap) == 1);
    }

}