package com.hx.log.test;

import com.hx.log.alogrithm.bloom_filter.BloomFilterChain;
import com.hx.log.alogrithm.bloom_filter.SimpleBloomFilter;
import com.hx.log.alogrithm.hash.SimpleHashFunc;
import com.hx.log.alogrithm.bloom_filter.interf.BloomFilter;
import com.hx.log.alogrithm.hash.interf.HashFunc;
import org.junit.Test;

import static com.hx.log.util.Log.info;

/**
 * tests for BloomFilter
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:57 PM
 */
public class Test13BloomFilter {

//    // tests for BloomFilter
//    public static void main(String[] args) {
//
//        test01SimpleBloomFilter();
//
//    }

    @Test
    public void test01SimpleBloomFilter() {

        BloomFilter filter = new SimpleBloomFilter();
        for(int i=55; i<77; i++) {
            filter.add(String.valueOf(i));
        }

        info(filter.contains("66"));
        info(filter.contains("22"));

    }

    @Test
    public void test02ChainOfBloomFIlter() {

//        BloomFilter filter01 = new SimpleBloomFilter();
        // hash conflicted
        BloomFilter filter01 = new SimpleBloomFilter(4, new HashFunc[]{
                new SimpleHashFunc(4, 31),
                new SimpleHashFunc(4, 37)
        });
        BloomFilter filter02 = new SimpleBloomFilter(8, new HashFunc[]{
                new SimpleHashFunc(8, 31),
                new SimpleHashFunc(8, 37)
        });
        BloomFilterChain filter = new BloomFilterChain(new BloomFilter[]{filter01, filter02 });

        for(int i=55; i<77; i++) {
            filter.add(String.valueOf(i));
        }

        info(filter.contains("66"));
        info(filter.contains("22"));

    }


}
