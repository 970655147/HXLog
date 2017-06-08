package com.hx.log.cache;

import com.hx.common.interf.cache.*;

/**
 * ����SimpleCacheEntry��cacheEntryFactory
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 12:15 PM
 */
public class SimpleCacheEntryFactory implements CacheEntryFactory {

    @Override
    public <K, V> CacheEntry<K, V> create(K key, V value, long ttl) {
        return new SimpleCacheEntry<K, V>(key, value, ttl);
    }

    @Override
    public <K, V> CacheEntryFacade<K, V> createFacade(CacheEntry<K, V> entry) {
        return new SimpleCacheEntryFacade<>(entry);
    }

    @Override
    public <K, V> CacheContext<K, V> createContext(Cache<K, V> cache, CacheEntry<K, V> entry) {
        return new SimpleCacheContext<>(cache, (entry == null) ? null : createFacade(entry));
    }
}
