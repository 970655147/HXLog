package com.hx.log.alogrithm.bloom_filter;

import com.hx.log.alogrithm.hash.SimpleHashFunc;
import com.hx.log.alogrithm.bloom_filter.interf.BloomFilter;
import com.hx.log.alogrithm.hash.interf.HashFunc;
import com.hx.log.util.Tools;

import java.util.BitSet;

/**
 * һ������BitSet�ļ���BloomFilter
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:38 PM
 */
public class SimpleBloomFilter implements BloomFilter {

    // Ĭ�ϵ�BitSet�����Ĵ�С
    // ����hash�����ĵ�����
    private static final int DEFAULT_SIZE = 1<<25;
    private static final int[] SEEDS = new int[] {5, 7, 11, 13, 31, 37, 61, 71 };
    private static final HashFunc[] DEFAULT_HASH_FUNCS = new HashFunc[SEEDS.length];
    static {
        for (int i = 0; i < SEEDS.length; i++) {
            DEFAULT_HASH_FUNCS[i] = new SimpleHashFunc(DEFAULT_SIZE, SEEDS[i]);
        }
    }

    // bitSet ����
    // ����������ַ�����hash�ĸ���hansh����
    private BitSet bits;
    private HashFunc[] hashFuncs;

    // ��ʼ��
    // ��capΪ���ڵ���cap����С�ĵ�'����������'
    // ����bitSet, hashFuncs
    public SimpleBloomFilter(int cap, HashFunc[] hashFuncs) {
        Tools.assert0(cap > 0, "'cap' must gt 0 !");
        Tools.assert0(hashFuncs != null, "'hashFuncs' can't be null !");
        for(HashFunc hashFunc : hashFuncs) {
            Tools.assert0(hashFunc != null, "some of 'hashFunc' is null, please check that !");
        }

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
//			if (value == null) {
//				return false;
//			}
        boolean isContain =true;
        for (HashFunc f : hashFuncs) {
            isContain = isContain && bits.get(f.hash(str));

            if(! isContain) {
                break ;
            }
        }

        return isContain;
    }

    // �жϸ�����num�Ƿ�Ϊ'����������'
    private static boolean isInteger(int num) {
        return ((num & (num-1)) == 0);
//			return (Integer.bitCount(cap) == 1);
    }
    // �����num�����С��'����������'
    private static int getCapsInteger(int num) {
        if(isInteger(num) ) {
            return num;
        }

        return Integer.highestOneBit(num) << 1;
    }

}
