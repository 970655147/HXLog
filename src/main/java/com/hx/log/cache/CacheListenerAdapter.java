package com.hx.log.cache;

import com.hx.common.interf.cache.CacheContext;
import com.hx.common.interf.cache.CacheListener;

/**
 * CacheListenerAdapter
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 6/8/2017 8:05 PM
 */
public class CacheListenerAdapter<K, V> implements CacheListener<K, V> {

    @Override
    public void beforeGet(CacheContext<K, V> context) {

    }

    @Override
    public void afterHitted(CacheContext<K, V> context) {

    }

    @Override
    public void beforeAdd(CacheContext<K, V> context) {

    }

    @Override
    public void afterAdd(CacheContext<K, V> context) {

    }

    @Override
    public void beforeUpdate(CacheContext<K, V> context) {

    }

    @Override
    public void afterUpdate(CacheContext<K, V> context) {

    }

    @Override
    public void beforeEvict(CacheContext<K, V> context) {

    }

    @Override
    public void afterEvict(CacheContext<K, V> context) {

    }

    @Override
    public void beforeClear(CacheContext<K, V> context) {

    }

    @Override
    public void afterClear(CacheContext<K, V> context) {

    }

    @Override
    public void beforeDestroy(CacheContext<K, V> context) {

    }

    @Override
    public void afterDestroy(CacheContext<K, V> context) {

    }
}
