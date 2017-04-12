package com.hx.log.alogrithm.hash;

import com.hx.log.interf.HashFunc;

/**
 * 一个简单的HashFunc的实现, * 种子, 然后取模
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:35 PM
 */
// HashFunc的简单实现
public class SimpleHashFunc implements HashFunc {
    // 容量, hash种子
    private int cap;
    private int seed;
    private boolean isInteger;

    // 初始化
    public SimpleHashFunc(int cap, int seed) {
        this.cap = cap;
        isInteger = isInteger(cap);
        this.seed = seed;
    }

    // 计算给定的val的hash
    // 简单的累加hash函数
    public int hash(Object value) {
        if(value == null) {
            return 0;
        }

        int result =0;
        String valStr = String.valueOf(value);
        int len = valStr.length();
        for (int i =0; i < len; i++) {
            result = seed * result + valStr.charAt(i);
        }

        if(isInteger) {
            return result & (cap -1);
        }

        return result % cap;
    }

    // 判断给定的num是否为'二进制整数'
    private static boolean isInteger(int num) {
        return ((num & (num-1)) == 0);
//			return (Integer.bitCount(cap) == 1);
    }

}