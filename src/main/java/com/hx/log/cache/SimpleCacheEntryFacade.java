package com.hx.log.cache;

import com.hx.log.cache.interf.CacheEntryFacade;
import com.hx.log.interf.CacheEntry;
import com.hx.log.util.Tools;

import java.util.Date;

/**
 * 一个CacheEntryFacade的简单实现
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 3:31 PM
 */
public class SimpleCacheEntryFacade<K, V> implements CacheEntryFacade<K, V> {

    private CacheEntry<K, V> entry;

    public SimpleCacheEntryFacade(CacheEntry<K, V> entry) {
        Tools.assert0(entry != null, "'entry' can't be null !");

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
