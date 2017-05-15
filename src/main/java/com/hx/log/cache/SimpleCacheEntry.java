package com.hx.log.cache;

import com.hx.common.interf.cache.CacheEntry;

import java.util.Date;
import java.util.concurrent.atomic.AtomicLong;

/**
 * CacheEntry的简单实现
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 11:41 AM
 */
public class SimpleCacheEntry<K, V> implements CacheEntry<K, V> {

    /**
     * the key associated at this cacheEntry
     */
    private K key;
    /**
     * the value associated at this cacheEntry
     */
    private V value;
    /**
     * the 'access count' of this cacheEntry
     */
    private AtomicLong accessCount;
    /**
     * the 'time to live' of this cacheEntry
     */
    private long ttl;

    /**
     * the createTime of this cacheEntry
     */
    private Date createdAt;
    /**
     * the lastAccessTime, lastUpdateTime of this cacheEntry
     */
    private Date lastAccessed;
    /**
     * the lastUpdateTime of this cacheEntry
     */
    private Date lastUpdated;
    /**
     * the evictedTime of this cacheEntry
     */
    private Date evictedAt;

    /**
     * initialize
     *
     * @param key   the key
     * @param value * @param key
 * @param value
 * @param ttllive
     * @since 1.0
     */
    public SimpleCacheEntry(K key, V value, long ttl) {
        this.key = key;
        this.value = value;
        this.ttl = ttl;
        this.accessCount = new AtomicLong(0);

        this.createdAt = new Date();
    }

    public SimpleCacheEntry(K key, V value) {
        this(key, value, CacheEntry.LONG_LIVE);
    }

    @Override
    public K key() {
        return key;
    }

    @Override
    public V value() {
        return value;
    }

    @Override
    public long accessCount() {
        return accessCount.get();
    }

    @Override
    public long ttl() {
        return ttl;
    }

    @Override
    public Date createdAt() {
        return createdAt;
    }

    @Override
    public Date lastAccessed() {
        return lastAccessed;
    }

    @Override
    public Date lastUpdated() {
        return lastUpdated;
    }

    @Override
    public Date evictedAt() {
        return evictedAt;
    }

    @Override
    public void value(V value) {
        this.value = value;
    }

    @Override
    public void incAccessCount(long inc) {
        this.accessCount.addAndGet(inc);
    }

    @Override
    public void ttl(long ttl) {
        this.ttl = ttl;
    }

    @Override
    public void createdAt(Date date) {
        this.createdAt = date;
    }

    @Override
    public void lastAccessed(Date date) {
        this.lastAccessed = date;
    }

    @Override
    public void lastUpdated(Date date) {
        this.lastUpdated = date;
    }

    @Override
    public void evictedAt(Date date) {
        this.evictedAt = date;
    }
}
