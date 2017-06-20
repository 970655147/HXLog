package com.hx.log.cache.mem;

import com.hx.common.interf.cache.CacheEntry;
import com.hx.common.interf.cache.CacheEntryFactory;
import com.hx.log.cache.mem.interf.MCache;

import java.util.HashMap;

/**
 * 容器不主动 Eliminate 数据的 Cache
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 6/14/2017 8:06 PM
 */
public class UniverseCache<K, V> extends MCache<K, V> {

    public UniverseCache(boolean enableTimeout, int state, CacheEntryFactory cacheEntryFactory) {
        super(Integer.MAX_VALUE, enableTimeout, state, cacheEntryFactory);
        cache = new HashMap<>();
    }

    public UniverseCache(int state, CacheEntryFactory cacheEntryFactory) {
        this(DEFAULT_ENABLE_TIMEOUT, state, cacheEntryFactory);
    }

    public UniverseCache(boolean enableTimeout) {
        this(enableTimeout, STATE_ALL, DEFAULT_CACHE_ENTRY_FACTORY);
    }

    public UniverseCache() {
        this(DEFAULT_ENABLE_TIMEOUT);
    }

    @Override
    protected V getAfterGetEntry(K key, CacheEntry<K, V> entry) {
        return entry.value();
    }

    @Override
    protected boolean putAfterGetEntry(K key, CacheEntry<K, V> entry) {
        return true;
    }

    @Override
    protected boolean updateAfterGetEntry(K key, CacheEntry<K, V> entry) {
        return true;
    }

    @Override
    protected CacheEntry<K, V> evictAfterGetEntry(K key, CacheEntry<K, V> entry) {
        return entry;
    }

    @Override
    protected boolean afterStateUpdated(int state, boolean succ) {
        return true;
    }

    @Override
    protected int afterClear(int cleaned) {
        return cleaned;
    }

    @Override
    protected boolean afterDestroyed(boolean succ) {
        return succ;
    }
}
