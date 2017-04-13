package com.hx.log.cache.interf;

import java.util.Date;

/**
 * ���û���CacheEntry
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 3:29 PM
 */
public interface CacheEntryFacade<K, V> {

    /**
     * ��ȡ��ǰEntry��Ӧ��key
     *
     * @return return key associated at this cacheEntry
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    K key();

    /**
     * ��ȡ��ǰEntry��Ӧ��value
     *
     * @return return value associated at this cacheEntry
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    V value();

    /**
     * ��ȡ��ǰEntry��Ӧ���ʵĴ���
     *
     * @return return accessCount of this cacheEntry
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    long accessCount();

    /**
     * ��ȡ��ǰEntry������ʱ��
     *
     * @return return time to live, -1 represents long live
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    long ttl();

    /**
     * ��ȡ��ǰEntry�Ĵ���ʱ��
     *
     * @return return time this cacheEntry created
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    Date createdAt();

    /**
     * ��ȡ��ǰEntry����һ�η���ʱ��
     *
     * @return return last access time this cacheEntry
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    Date lastAccessed();

    /**
     * ��ȡ��ǰEntry����һ�θ���ʱ��
     *
     * @return return last update time this cacheEntry
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    Date lastUpdated();

    /**
     * ��ȡ��ǰEntry�ı��Ƴ������ʱ��, �����ȳ�ȥ��Ҳ��
     *
     * @return return the time this cacheEntry be eivcted
     * @author Jerry.X.He
     * @date 4/13/2017 11:30 AM
     * @since 1.0
     */
    Date evictedAt();

}
