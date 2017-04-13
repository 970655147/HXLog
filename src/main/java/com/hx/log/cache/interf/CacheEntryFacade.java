package com.hx.log.cache.interf;

import java.util.Date;

/**
 * 给用户的CacheEntry
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 3:29 PM
 */
public interface CacheEntryFacade<K, V> {

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

}
