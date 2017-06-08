package com.hx.log.cache;

import com.hx.common.interf.cache.Cache;
import com.hx.common.interf.cache.CacheContext;
import com.hx.common.interf.cache.CacheEntryFacade;
import com.hx.log.util.Tools;

/**
 * 一个简易实现的 CacheContext
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 6/8/2017 8:04 PM
 */
public class SimpleCacheContext<K, V> implements CacheContext<K, V> {

    private Cache<K, V> cache;
    private CacheEntryFacade<K, V> entryFacade;

    public SimpleCacheContext(Cache<K, V> cache, CacheEntryFacade<K, V> entryFacade) {
        Tools.assert0(cache != null, "'cache' can't be null !");
        this.cache = cache;
        this.entryFacade = entryFacade;
    }

    @Override
    public Cache<K, V> getCache() {
        return cache;
    }

    @Override
    public CacheEntryFacade<K, V> cacheEntry() {
        return entryFacade;
    }
}
