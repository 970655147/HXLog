package com.hx.log.cache.interf;

import java.util.Collection;
import java.util.List;

/**
 * 缓存接口
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 11:00 AM
 */
public interface Cache<K, V> {

    /**
     * 根据给定的key, 从缓存中拿对应的value
     *
     * @param key 给定的key
     * @return the value store in cache, or else null will be return
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    V get(K key);

    /**
     * 根据给定的key, 从缓存中拿对应的entry
     *
     * @param key 给定的key
     * @return the cacheEntry store in cache, or else null will be return
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    CacheEntryFacade<K, V> getEntry(K key);

    /**
     * 获取缓存中所有的key
     *
     * @return all key in cache
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    List<K> keys();

    /**
     * 获取当前缓存中缓存的元素个数
     *
     * @return element size of current cache
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    int size();

    /**
     * 获取当前缓存中缓存的元素个数上线
     *
     * @return the max element size of current cache, -1 represents no limitation
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    int capacity();

    /**
     * 缓存命中的次数
     *
     * @return the number of cache hitted
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    long hitCount();

    /**
     * 用户访问缓存的次数
     *
     * @return the number of cache visited
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    long visitCount();

    /**
     * 向缓存中添加kv对
     *
     * @return true if put entry success, or else
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean put(K key, V value);

    /**
     * 向缓存中添加kv对, 并配置过期时间
     *
     * @return true if put entry success, or else
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean put(K key, V value, long expire);

    /**
     * 更新缓存中的kv对
     *
     * @return true if update entry success, or else
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean update(K key, V value);

    /**
     * 更新缓存中的kv对, 并更新过期时间
     *
     * @return true if update entry success, or else
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean update(K key, V value, long expire);

    /**
     * 清理缓存的key对应的条目
     *
     * @return true if evict entry success, or else
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean evict(K key);

    /**
     * 清理缓存的keys对应的条目
     *
     * @return true if evict entries success, or else
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean evict(Collection<K> keys);

    /**
     * 配置当前缓存的状态[读写等等控制]
     *
     * @return true if config state success
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean state(int state);

    /**
     * 判断当前Cache是否可读
     *
     * @return true if config state success
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean readable();

    /**
     * 判断当前Cache是否可写
     *
     * @return true if config state success
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean writeable();

    /**
     * 配置ttl是固定的ttl[相对于createAt], 还是相对于上一次access的ttl
     *
     * @param isFixed ttl是否固定
     * @return config the ttl type
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    void fixedTtl(boolean isFixed);

    /**
     * 获取ttl的类型
     *
     * @return get the ttl type
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean fixedTtl();

    /**
     * 清理所有的key
     *
     * @return the element count that removed
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    int clear();

    /**
     * 销毁当前cache
     *
     * @return true if destroy success
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean destroy();

}
