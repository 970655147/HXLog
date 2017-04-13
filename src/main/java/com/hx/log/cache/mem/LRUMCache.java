package com.hx.log.cache.mem;

import com.hx.log.cache.interf.CacheEntryFactory;
import com.hx.log.interf.CacheEntry;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * latest recently used cache [base on memory]
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 5:13 PM
 */
public class LRUMCache<K, V> extends MCache<K, V> {

    /**
     * °´ÕÕaccessCountÅÅÐòµÄ accessCnt -> key
     */
    protected Set<K> recentlyUsedQueue;

    public LRUMCache(int estimateSize, int capacity, int state, CacheEntryFactory cacheEntryFactory) {
        super(capacity, state, cacheEntryFactory);
        // true for access-order, false for insertion-order.
        cache = new LinkedHashMap<>(estimateSize);
        recentlyUsedQueue = new LinkedHashSet<>();
    }

    public LRUMCache(int capacity, int state, CacheEntryFactory cacheEntryFactory) {
        this(DEFAULT_ESTIMATE_SIZE, capacity, state, cacheEntryFactory);
    }

    public LRUMCache(int capacity, CacheEntryFactory cacheEntryFactory) {
        this(capacity, STATE_ALL, cacheEntryFactory);
    }

    public LRUMCache(int estimateSize, int capacity) {
        this(estimateSize, capacity, STATE_ALL, DEFAULT_CACHE_ENTRY_FACTORY);
    }

    public LRUMCache(int capacity) {
        this(DEFAULT_ESTIMATE_SIZE, capacity);
    }

    @Override
    protected V getAfterGetEntry(K key, CacheEntry<K, V> entry) {
        if(recentlyUsedQueue.contains(key)) {
            recentlyUsedQueue.remove(key);
        }
        recentlyUsedQueue.add(key);
        return entry.value();
    }

    @Override
    protected boolean putAfterGetEntry(K key, CacheEntry<K, V> entry) {
        recentlyUsedQueue.add(key);
        if(size() > capacity) {
            K least = recentlyUsedQueue.iterator().next();
            evict(least);
        }

        return true;
    }

    @Override
    protected boolean updateAfterGetEntry(K key, CacheEntry<K, V> entry) {
        return true;
    }

    @Override
    protected boolean evictAfterGetEntry(K key, CacheEntry<K, V> entry) {
        recentlyUsedQueue.remove(key);
        return true;
    }

    @Override
    protected boolean afterStateUpdated(int state, boolean succ) {
        return succ;
    }

    @Override
    protected int afterClear(int cleaned) {
        recentlyUsedQueue.clear();
        return cleaned;
    }

    @Override
    protected boolean afterDestroyed(boolean succ) {
        return succ;
    }
}
