package com.hx.log.cache.interf;

import java.util.Date;

/**
 * 缓存的条目
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 11:28 AM
 */
public interface CacheEntry<K, V> {

    /**
     * live forever, escape be evicted or scheduled out
     */
    long LONG_LIVE = -1;
    /**
     * do not update cacheEntry's ttl
     */
    long NOT_UPDATE_TTL = -2;

    /**
     * 获取当前Entry对应的key
     *
     * @return return key associated at this cacheEntry
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    K key();

    /**
     * 获取当前Entry对应的value
     *
     * @return return value associated at this cacheEntry
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    V value();

    /**
     * 获取当前Entry对应访问的次数
     *
     * @return return accessCount of this cacheEntry
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    long accessCount();

    /**
     * 获取当前Entry的生存时间
     *
     * @return return time to live, -1 represents long live
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    long ttl();

    /**
     * 获取当前Entry的创建时间
     *
     * @return return time this cacheEntry created
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    Date createdAt();

    /**
     * 获取当前Entry的上一次访问时间
     *
     * @return return last access time this cacheEntry
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    Date lastAccessed();

    /**
     * 获取当前Entry的上一次更新时间
     *
     * @return return last update time this cacheEntry
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    Date lastUpdated();

    /**
     * 获取当前Entry的被移除缓存的时间, 被调度出去的也算
     *
     * @return return the time this cacheEntry be eivcted
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    Date evictedAt();

    /**
     * set the value of this cacheEntry
     *
     * @return void
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    void value(V value);

    /**
     * increment the accessCount of this cacheEntry
     *
     * @return void
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    void incAccessCount(long inc);

    /**
     * set the ttl of this cacheEntry
     *
     * @return void
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    void ttl(long ttl);

    /**
     * set the createAt of this cacheEntry
     *
     * @return void
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    void createdAt(Date date);

    /**
     * set the lastAccessed of this cacheEntry
     *
     * @return void
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    void lastAccessed(Date date);

    /**
     * set the lastUpdated of this cacheEntry
     *
     * @return void
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    void lastUpdated(Date date);

    /**
     * set the evictedAt of this cacheEntry
     *
     * @return void
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    void evictedAt(Date date);

}
