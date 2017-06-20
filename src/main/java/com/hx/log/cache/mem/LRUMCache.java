package com.hx.log.cache.mem;

import com.hx.common.interf.cache.CacheEntry;
import com.hx.common.interf.cache.CacheEntryFactory;
import com.hx.log.cache.mem.interf.MCache;

import java.util.HashMap;
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
     * 根据最近使用的情况排序的队列
     */
    protected Set<K> recentlyUsedQueue;

    public LRUMCache(int estimateSize, int capacity, boolean enableTimeout, int state,
                     CacheEntryFactory cacheEntryFactory) {
        super(capacity, enableTimeout, state, cacheEntryFactory);
        cache = new HashMap<>(estimateSize);
        recentlyUsedQueue = new LinkedHashSet<>();
    }

    public LRUMCache(int capacity, int state, CacheEntryFactory cacheEntryFactory) {
        this(DEFAULT_ESTIMATE_SIZE, capacity, DEFAULT_ENABLE_TIMEOUT, state, cacheEntryFactory);
    }

    public LRUMCache(int capacity, CacheEntryFactory cacheEntryFactory) {
        this(capacity, STATE_ALL, cacheEntryFactory);
    }

    public LRUMCache(int estimateSize, int capacity) {
        this(estimateSize, capacity, DEFAULT_ENABLE_TIMEOUT, STATE_ALL, DEFAULT_CACHE_ENTRY_FACTORY);
    }

    public LRUMCache(int capacity, boolean enableTimeout) {
        this(DEFAULT_ESTIMATE_SIZE, capacity, enableTimeout, STATE_ALL, DEFAULT_CACHE_ENTRY_FACTORY);
    }

    public LRUMCache(int capacity) {
        this(DEFAULT_ESTIMATE_SIZE, capacity);
    }

    @Override
    protected V getAfterGetEntry(K key, CacheEntry<K, V> entry) {
        if (recentlyUsedQueue.contains(key)) {
            recentlyUsedQueue.remove(key);
        }
        recentlyUsedQueue.add(key);
        return entry.value();
    }

    @Override
    protected boolean putAfterGetEntry(K key, CacheEntry<K, V> entry) {
        recentlyUsedQueue.add(key);
        if (size() > capacity) {
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
    protected CacheEntry<K, V> evictAfterGetEntry(K key, CacheEntry<K, V> entry) {
        recentlyUsedQueue.remove(key);
        return entry;
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
