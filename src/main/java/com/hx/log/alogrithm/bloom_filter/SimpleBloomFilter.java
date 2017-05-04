package com.hx.log.alogrithm.bloom_filter;

import com.hx.log.alogrithm.bloom_filter.interf.BloomFilter;
import com.hx.log.alogrithm.hash.SimpleHashFunc;
import com.hx.log.alogrithm.hash.interf.HashFunc;
import com.hx.log.collection.CollectionUtils;

import java.util.BitSet;

import static com.hx.log.util.Tools.assert0;

/**
 * 一个基于BitSet的简易BloomFilter
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:38 PM
 */
public class SimpleBloomFilter implements BloomFilter {

    /**
     * 默认的BitSet容器的大小
     */
    private static final int DEFAULT_SIZE = 1 << 25;
    /**
     * 各个hash函数的的种子
     */
    private static final int[] SEEDS = new int[]{5, 7, 11, 13, 31, 37, 61, 71};
    /**
     * 默认的hashFuncs
     */
    private static final HashFunc[] DEFAULT_HASH_FUNCS = new HashFunc[SEEDS.length];

    static {
        for (int i = 0; i < SEEDS.length; i++) {
            DEFAULT_HASH_FUNCS[i] = new SimpleHashFunc(DEFAULT_SIZE, SEEDS[i]);
        }
    }

    /**
     * bitSet 容器
     */
    private BitSet bits;
    /**
     * 计算给定的字符串的hash的各个hansh函数
     */
    private HashFunc[] hashFuncs;

    /**
     * 令cap为大于等于cap的最小的的'二进制整数'
     * 构造bitSet, hashFuncs
     *
     * @param cap       给定的BitSet的容量
     * @param hashFuncs 给定的hashFuncs
     * @return
     * @author
     * @date
     * @since 1.0
     */
    public SimpleBloomFilter(int cap, HashFunc[] hashFuncs) {
        assert0(cap > 0, "'cap' must gt 0 !");
        assert0(hashFuncs != null, "'hashFuncs' can't be null !");
        assert0(! CollectionUtils.isAnyNull(hashFuncs), "some of 'hashFunc' is null, please check that !");

        cap = getCapsInteger(cap);
        bits = new BitSet(cap);
        this.hashFuncs = hashFuncs;
    }

    public SimpleBloomFilter() {
        this(DEFAULT_SIZE, DEFAULT_HASH_FUNCS);
    }

    @Override
    public boolean add(String str) {
        for (HashFunc f : hashFuncs) {
            bits.set(f.hash(str), true);
        }
        return true;
    }

    @Override
    public boolean contains(String str) {
        boolean isContain = true;
        for (HashFunc f : hashFuncs) {
            isContain = isContain && bits.get(f.hash(str));

            if (!isContain) {
                break;
            }
        }

        return isContain;
    }

    // ----------------- 辅助方法 -----------------------

    /**
     * 判断给定的num是否为'二进制整数'
     *
     * @param num 给定的数字
     * @return boolean
     * @author Jerry.X.He
     * @date 5/4/2017 9:38 PM
     * @since 1.0
     */
    private static boolean isInteger(int num) {
        return ((num & (num - 1)) == 0);
//			return (Integer.bitCount(cap) == 1);
    }

    /**
     * 计算比num大的最小的'二进制整数'
     *
     * @param num 给定的数字
     * @return int
     * @author Jerry.X.He
     * @date 5/4/2017 9:37 PM
     * @since 1.0
     */
    private static int getCapsInteger(int num) {
        if (isInteger(num)) {
            return num;
        }

        return Integer.highestOneBit(num) << 1;
    }

}
