package com.hx.log.cache;

import com.hx.common.interf.cache.CacheEntryFacade;
import com.hx.common.interf.cache.CacheEntry;

import java.util.Date;

import static com.hx.log.util.Tools.assert0;

/**
 * 一个CacheEntryFacade的简单实现
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 3:31 PM
 */
public class SimpleCacheEntryFacade<K, V> implements CacheEntryFacade<K, V> {

    private CacheEntry<K, V> entry;

    /**
     * initialize
     *
      * @param entry CacheEntry
     * @return
     * @author
     * @date
     * @since 1.0
     */
    public SimpleCacheEntryFacade(CacheEntry<K, V> entry) {
        assert0(entry != null, "'entry' can't be null !");

        this.entry = entry;
    }

    @Override
    public K key() {
        return entry.key();
    }

    @Override
    public V value() {
        return entry.value();
    }

    @Override
    public long accessCount() {
        return entry.accessCount();
    }

    @Override
    public long ttl() {
        return entry.ttl();
    }

    @Override
    public Date createdAt() {
        return entry.createdAt();
    }

    @Override
    public Date lastAccessed() {
        return entry.lastAccessed();
    }

    @Override
    public Date lastUpdated() {
        return entry.lastUpdated();
    }

    @Override
    public Date evictedAt() {
        return entry.evictedAt();
    }
}
