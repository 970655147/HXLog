package com.hx.log.alogrithm.bloom_filter.interf;

/**
 * 布隆过滤器
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:07 PM
 */
public interface BloomFilter {

    /**
     * 将给定的字符串添加到filter中
     *
     * @param str 需要添加到filter的字符串
     * @return true if success, or else false
     * @author Jerry.X.He
     * @date 4/12/2017 9:09 PM
     * @since 1.0
     */
    boolean add(String str);

    /**
     * 判断当前filter中是否存在str对应的字符串[可能存在一定概率的误差]
     *
     * @param str 需要检查的字符串
     * @return true if contains, or else false
     * @author Jerry.X.He
     * @date 4/12/2017 9:09 PM
     * @since 1.0
     */
    boolean contains(String str);

}
