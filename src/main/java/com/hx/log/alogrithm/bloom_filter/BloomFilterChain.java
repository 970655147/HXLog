package com.hx.log.alogrithm.bloom_filter;

import com.hx.log.alogrithm.bloom_filter.interf.BloomFilter;
import com.hx.log.collection.CollectionUtils;
import com.hx.log.util.Tools;

import static com.hx.log.util.Tools.assert0;

/**
 * 一个复合的BloomFilter
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:46 PM
 */
public class BloomFilterChain implements BloomFilter {

    private BloomFilter[] chain;

    /**
     * 使用给定的一系列的Bloom * @param chain*
     * @param chain
     * @return
     * @author
     * @date
     * @since 1.0
     */
    public BloomFilterChain(BloomFilter[] chain) {
        assert0(chain != null, "'chain' can't be null !");
        assert0(! CollectionUtils.isAnyNull(chain), "some of 'filter' is null, please check that !");

        this.chain = chain;
    }

    @Override
    public boolean add(String str) {
        for(BloomFilter filter : chain) {
            filter.add(str);
        }
        return true;
    }

    @Override
    public boolean contains(String str) {
        for(BloomFilter filter : chain) {
            if(! filter.contains(str)) {
                return false;
            }
        }

        return true;
    }
}
