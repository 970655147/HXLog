package com.hx.log.cache.mem.interf;

import com.hx.common.interf.cache.*;
import com.hx.json.JSONArray;
import com.hx.log.cache.SimpleCacheEntryFactory;
import com.hx.log.date.DateUtils;
import com.hx.log.util.Tools;

import java.util.*;
import java.util.concurrent.atomic.AtomicLong;

import static com.hx.log.util.Log.info;
import static com.hx.log.util.Tools.assert0;

/**
 * 存放于内存的cache
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 11:21 AM
 */
public abstract class MCache<K, V> implements Cache<K, V> {

    /**
     * 默认的cacheEntryFactory
     */
    protected static final CacheEntryFactory DEFAULT_CACHE_ENTRY_FACTORY = new SimpleCacheEntryFactory();
    /**
     * 只读, 只写, 可读可写, 不可读不可写
     */
    public static final int STATE_READ = 0b01;
    public static final int STATE_WRITE = 0b10;
    public static final int STATE_ALL = STATE_WRITE | STATE_READ;
    public static final int STATE_NONE = 0;
    /**
     * 触发listener的类型
     */
    public static final int LISTENER_BEFORE_GET = 0;
    public static final int LISTENER_AFTER_HITTED = LISTENER_BEFORE_GET + 1;
    public static final int LISTENER_BEFORE_ADD = LISTENER_AFTER_HITTED + 1;
    public static final int LISTENER_AFTER_ADD = LISTENER_BEFORE_ADD + 1;
    public static final int LISTENER_BEFORE_UPDATE = LISTENER_AFTER_ADD + 1;
    public static final int LISTENER_AFTER_UPDATE = LISTENER_BEFORE_UPDATE + 1;
    public static final int LISTENER_BEFORE_EVICT = LISTENER_AFTER_UPDATE + 1;
    public static final int LISTENER_AFTER_EVICT = LISTENER_BEFORE_EVICT + 1;
    public static final int LISTENER_BEFORE_CLEAR = LISTENER_AFTER_EVICT + 1;
    public static final int LISTENER_AFTER_CLEAR = LISTENER_BEFORE_CLEAR + 1;
    public static final int LISTENER_BEFORE_DESTROY = LISTENER_AFTER_CLEAR + 1;
    public static final int LISTENER_AFTER_DESTROY = LISTENER_BEFORE_DESTROY + 1;
    /**
     * 默认的size, 默认的loadFactor
     */
    protected static final int DEFAULT_ESTIMATE_SIZE = 16;
    protected static final float DEFAULT_LOADER_FACTOR = 0.75f;
    /**
     * 是否启动超时检测
     */
    protected static final boolean DEFAULT_ENABLE_TIMEOUT = true;
    /**
     * 超时检测的 timer
     */
    protected static Timer TIMEOUT_CHECK_TIMER = new Timer();

    /**
     * 缓存KV的Map
     */
    protected Map<K, CacheEntry<K, V>> cache;
    /**
     * 当前缓存最大可以容纳的元素的数量
     */
    protected int capacity;
    /**
     * 缓存命中的次数, 缓存访问的次数
     */
    protected AtomicLong hitted;
    protected AtomicLong visited;
    /**
     * 构建cacheEntry的工厂
     */
    protected CacheEntryFactory cacheEntryFactory;
    /**
     * 标记当前Cache的状态
     */
    protected int state;
    /**
     * ttl是固定的ttl[相对于createAt], 还是相对于上一次access的ttl
     */
    protected boolean fixedTtl;
    /**
     * 标记当前Cache是否被销毁
     */
    protected boolean destroyed;
    /**
     * 是否启动超时检测
     */
    protected boolean enableTimeout;

    /**
     * 当前 Cache 关联的 所有的 cacheListener
     */
    private List<CacheListener<K, V>> cacheListeners;

    /**
     * 维护cache同步的lock
     */
    protected final Object cacheLock = new Object();

    public MCache(int capacity, boolean enableTimeout, int state, CacheEntryFactory cacheEntryFactory) {
        assert0(capacity > 0, "'capacity' must gte 0 !");
        assert0(cacheEntryFactory != null, "'cacheEntryFactory' can't be null !");

        this.capacity = capacity;
        this.enableTimeout = enableTimeout;
        this.hitted = new AtomicLong(0);
        this.visited = new AtomicLong(0);

        this.cacheEntryFactory = cacheEntryFactory;
        // update state, first authority all, the scale the state
        this.state = STATE_ALL;
        state(state);
        cacheListeners = new ArrayList<>();
    }

    public MCache(int capacity, CacheEntryFactory cacheEntryFactory) {
        this(capacity, DEFAULT_ENABLE_TIMEOUT, STATE_ALL, cacheEntryFactory);
    }

    public MCache(int capacity, boolean enableTimeout) {
        this(capacity, DEFAULT_CACHE_ENTRY_FACTORY);
    }

    public MCache(int capacity) {
        this(capacity, DEFAULT_CACHE_ENTRY_FACTORY);
    }

    @Override
    public void addCacheListener(CacheListener<K, V> cacheListener) {
        Tools.assert0(cacheListener != null, "'cacheListener' can't be null !");

        cacheListeners.add(cacheListener);
    }

    @Override
    public boolean removeCacheListener(CacheListener<K, V> cacheListener) {
        if (cacheListener == null) {
            return false;
        }

        boolean hasRemoved = false;
        Iterator<CacheListener<K, V>> ite = cacheListeners.iterator();
        while (ite.hasNext()) {
            CacheListener<K, V> listener = ite.next();
            if (cacheListener.equals(listener)) {
                ite.remove();
                hasRemoved = true;
            }
        }
        return hasRemoved;
    }

    @Override
    public V get(K key) {
        if (!readable()) {
            throw new RuntimeException("currentStartIdx cache is not readable !");
        }

        fireListeners(LISTENER_BEFORE_GET, cacheEntryFactory.createContext(this, null));
        visited.incrementAndGet();
        CacheEntry<K, V> entry = getEntry0(key);
        if (entry == null) {
            return null;
        }

        hitted.incrementAndGet();
        V result = getEntry0(entry);
        fireListeners(LISTENER_AFTER_HITTED, cacheEntryFactory.createContext(this, entry));
        return result;
    }

    @Override
    public CacheEntryFacade<K, V> getEntry(K key) {
        if (!readable()) {
            throw new RuntimeException("currentStartIdx cache is not readable !");
        }

        fireListeners(LISTENER_BEFORE_GET, cacheEntryFactory.createContext(this, null));
        CacheEntry<K, V> entry = getEntry0(key);
        if (entry == null) {
            return null;
        }

        getEntry0(entry);
        fireListeners(LISTENER_AFTER_HITTED, cacheEntryFactory.createContext(this, entry));
        return cacheEntryFactory.createFacade(entry);
    }

    @Override
    public List<K> keys() {
        if (!readable()) {
            throw new RuntimeException("currentStartIdx cache is not readable !");
        }

        List<K> keys = new ArrayList<>(size());
        synchronized (cacheLock) {
            keys.addAll(cache.keySet());
        }

        return keys;
    }

    @Override
    public int size() {
        if (!readable()) {
            throw new RuntimeException("currentStartIdx cache is not readable !");
        }

        return cache.size();
    }

    @Override
    public int capacity() {
        if (!readable()) {
            throw new RuntimeException("currentStartIdx cache is not readable !");
        }

        return capacity;
    }

    @Override
    public long hitCount() {
        return hitted.get();
    }

    @Override
    public long visitCount() {
        return visited.get();
    }

    @Override
    public boolean put(K key, V value) {
        return put(key, value, CacheEntry.LONG_LIVE);
    }

    @Override
    public boolean put(K key, V value, long expire) {
        assert0(expire >= CacheEntry.LONG_LIVE, "'expire' must gte " + CacheEntry.LONG_LIVE);
        if (!writeable()) {
            throw new RuntimeException("currentStartIdx cache is not writeable !");
        }

        CacheEntry<K, V> entry = getEntry0(key);
        if (entry != null) {
            fireListeners(LISTENER_BEFORE_UPDATE, cacheEntryFactory.createContext(this, null));
            boolean result = updateEntry0(entry, value, expire);
            fireListeners(LISTENER_AFTER_UPDATE, cacheEntryFactory.createContext(this, entry));
            return result;
        }

        fireListeners(LISTENER_BEFORE_ADD, cacheEntryFactory.createContext(this, null));
        entry = cacheEntryFactory.create(key, value, expire);
        boolean result = false;
        synchronized (cacheLock) {
            cache.put(key, entry);
            result = putAfterGetEntry(key, entry);
        }
        fireListeners(LISTENER_AFTER_ADD, cacheEntryFactory.createContext(this, entry));
        putTimeoutCheckTask(expire);
        return result;
    }

    @Override
    public boolean update(K key, V value) {
        return update(key, value, CacheEntry.NOT_UPDATE_TTL);
    }

    @Override
    public boolean update(K key, V value, long expire) {
        assert0(expire >= CacheEntry.NOT_UPDATE_TTL, "'expire' must gte " + CacheEntry.LONG_LIVE);
        if (!writeable()) {
            throw new RuntimeException("currentStartIdx cache is not writeable !");
        }

        fireListeners(LISTENER_BEFORE_UPDATE, cacheEntryFactory.createContext(this, null));
        CacheEntry<K, V> entry = getEntry0(key);
        if (entry == null) {
            return false;
        }
        boolean result = updateEntry0(entry, value, expire);
        fireListeners(LISTENER_AFTER_UPDATE, cacheEntryFactory.createContext(this, entry));
        putTimeoutCheckTask(expire);
        return result;
    }

    @Override
    public V evict(K key) {
        if (!writeable()) {
            throw new RuntimeException("currentStartIdx cache is not writeable !");
        }

        fireListeners(LISTENER_BEFORE_EVICT, cacheEntryFactory.createContext(this, null));
        CacheEntry<K, V> entry = getEntry0(key);
        if (entry == null) {
            return null;
        }

        entry.evictedAt(new Date());
        CacheEntry<K, V> removed = null;
        synchronized (cacheLock) {
            cache.remove(key);
            removed = evictAfterGetEntry(key, entry);
        }
        fireListeners(LISTENER_AFTER_EVICT, cacheEntryFactory.createContext(this, entry));
        return (removed == null) ? null : removed.value();
    }

    @Override
    public List<V> evict(Collection<K> keys) {
        if (!writeable()) {
            throw new RuntimeException("currentStartIdx cache is not writeable !");
        }

        Set<K> distincted = new HashSet<>(Tools.estimateMapSize(keys.size()));
        distincted.addAll(keys);
        List<V> result = new ArrayList<>(keys.size());
        for (K key : distincted) {
            V removed = evict(key);
            if (removed != null) {
                result.add(removed);
            }
        }

        return result;
    }

    @Override
    public boolean state(int state) {
        assert0(state <= STATE_ALL, "not a valid state !");
        if (destroyed) {
            throw new RuntimeException("currentStartIdx cache is not destroyed !");
        }
        if (!writeable()) {
            throw new RuntimeException("currentStartIdx cache is not writeable !");
        }

        this.state = state;
        return afterStateUpdated(state, true);
    }

    /**
     * 判断当前Cache是否可读
     *
     * @return boolean true if currentStartIdx cache readable
     * @author Jerry.X.He
     * @date 4/13/2017 3:14 PM
     * @since 1.0
     */
    @Override
    public boolean readable() {
        return ((this.state & STATE_READ) > 0);
    }

    /**
     * 判断当前Cache是否可写
     *
     * @return boolean true if currentStartIdx cache writeable
     * @author Jerry.X.He
     * @date 4/13/2017 3:14 PM
     * @since 1.0
     */
    @Override
    public boolean writeable() {
        return ((this.state & STATE_WRITE) > 0);
    }

    @Override
    public void fixedTtl(boolean isFixed) {
        this.fixedTtl = isFixed;
    }

    @Override
    public boolean fixedTtl() {
        return fixedTtl;
    }

    /**
     * 更新checkInterval
     *
     * @return void
     * @author Jerry.X.He
     * @date 4/13/2017 5:56 PM
     * @since 1.0
     */
    public void putTimeoutCheckTask(long ttl) {
        if ((enableTimeout) && (ttl != CacheEntry.LONG_LIVE)) {
            Date toCheckDate = DateUtils.add(new Date(), Calendar.MILLISECOND, (int) ttl);
            TIMEOUT_CHECK_TIMER.schedule(new ExpireTimerTask(), toCheckDate);
        }
    }

    @Override
    public int clear() {
        int sz = 0;
        fireListeners(LISTENER_BEFORE_CLEAR, cacheEntryFactory.createContext(this, null));
        int cleared = 0;

        synchronized (cacheLock) {
            sz = cache.size();
            cache.clear();
            cleared = afterClear(sz);
        }
        fireListeners(LISTENER_AFTER_CLEAR, cacheEntryFactory.createContext(this, null));
        return cleared;
    }

    @Override
    public boolean destroy() {
        fireListeners(LISTENER_BEFORE_DESTROY, cacheEntryFactory.createContext(this, null));

        destroyed = true;
        this.state = STATE_NONE;
        boolean result = afterDestroyed(true);
        fireListeners(LISTENER_AFTER_DESTROY, cacheEntryFactory.createContext(this, null));
        return result;
    }

    /**
     * 留给子类重写的获取entry.value 以及其他的业务操作
     *
     * @param key   当前的key
     * @param entry 当前的key对应的entry
     * @return value of key corresponding
     * @author Jerry.X.He
     * @date 4/13/2017 12:09 PM
     * @since 1.0
     */
    protected abstract V getAfterGetEntry(K key, CacheEntry<K, V> entry);

    /**
     * 留给子类重写的put kv, 以及其他的业务操作
     * sync(cacheLock)
     *
     * @param key   当前的key
     * @param entry 当前的key对应的entry
     * @return value of key corresponding
     * @author Jerry.X.He
     * @date 4/13/2017 12:09 PM
     * @since 1.0
     */
    protected abstract boolean putAfterGetEntry(K key, CacheEntry<K, V> entry);

    /**
     * 留给子类重写的update kv, 以及其他的业务操作
     *
     * @param key   当前的key
     * @param entry 当前的key对应的entry
     * @return value of key corresponding
     * @author Jerry.X.He
     * @date 4/13/2017 12:09 PM
     * @since 1.0
     */
    protected abstract boolean updateAfterGetEntry(K key, CacheEntry<K, V> entry);

    /**
     * 留给子类重写的evict kv, 以及其他的业务操作
     * sync(cacheLock)
     *
     * @param key   当前的key
     * @param entry 当前的key对应的entry
     * @return value of key corresponding
     * @author Jerry.X.He
     * @date 4/13/2017 12:09 PM
     * @since 1.0
     */
    protected abstract CacheEntry<K, V> evictAfterGetEntry(K key, CacheEntry<K, V> entry);

    /**
     * 留给子类重写的配置状态之后之后需要处理的其他业务
     *
     * @param succ is config state success from now on
     * @return value of key corresponding
     * @author Jerry.X.He
     * @date 4/13/2017 12:09 PM
     * @since 1.0
     */
    protected abstract boolean afterStateUpdated(int state, boolean succ);

    /**
     * 留给子类重写的清理缓存之后需要处理的其他业务
     * sync(cacheLock)
     *
     * @return value of key corresponding
     * @author Jerry.X.He
     * @date 4/13/2017 12:09 PM
     * @since 1.0
     */
    protected abstract int afterClear(int cleaned);

    /**
     * 留给子类重写的销毁缓存之后需要处理的其他业务
     *
     * @return value of key corresponding
     * @author Jerry.X.He
     * @date 4/13/2017 12:09 PM
     * @since 1.0
     */
    protected abstract boolean afterDestroyed(boolean succ);

    // ---------------------------- 辅助方法 ----------------------------

    /**
     * 根据key获取key对应的cacheEntry
     *
     * @param key 给定的key
     * @return com.hx.common.interf.cache.CacheEntry<K,V>
     * @author Jerry.X.He
     * @date 4/13/2017 3:34 PM
     * @since 1.0
     */
    protected CacheEntry<K, V> getEntry0(K key) {
        CacheEntry<K, V> entry = cache.get(key);
        if (entry == null) {
            return null;
        }

        return entry;
    }

    /**
     * 更新当前entry的信息
     *
     * @param entry  当前需要更新的entry
     * @param value  更新entry的value
     * @param expire 更新entry的ttl
     * @return boolean
     * @author Jerry.X.He
     * @date 4/13/2017 3:59 PM
     * @since 1.0
     */
    private boolean updateEntry0(CacheEntry<K, V> entry, V value, long expire) {
        entry.value(value);
        entry.lastUpdated(new Date());
        if (expire != CacheEntry.NOT_UPDATE_TTL) {
            entry.ttl(expire);
        }
        return updateAfterGetEntry(entry.key(), entry);
    }

    /**
     * 处理获取entry的相关业务
     *
     * @param entry 给定的entry
     * @return V
     * @author Jerry.X.He
     * @date 4/13/2017 4:56 PM
     * @since 1.0
     */
    private V getEntry0(CacheEntry<K, V> entry) {
        entry.incAccessCount(1);
        entry.lastAccessed(new Date());
        return getAfterGetEntry(entry.key(), entry);
    }

    /**
     * 获取当前cache的所有的entry
     *
     * @return java.util.List<com.hx.common.interf.cache.CacheEntry<K,V>>
     * @author Jerry.X.He
     * @date 4/13/2017 5:41 PM
     * @since 1.0
     */
    private List<CacheEntry<K, V>> getAllEntries() {
        List<CacheEntry<K, V>> entries = new ArrayList<>(size());
        synchronized (cacheLock) {
            entries.addAll(cache.values());
        }
        return entries;
    }

    /**
     * 根据类型, 触发listener
     *
     * @param type    type
     * @param context context
     * @return void
     * @author Jerry.X.He
     * @date 6/8/2017 8:12 PM
     * @since 1.0
     */
    private void fireListeners(int type, CacheContext<K, V> context) {
        if (LISTENER_BEFORE_GET == type) {
            for (CacheListener<K, V> listener : cacheListeners) {
                listener.beforeGet(context);
            }
        } else if (LISTENER_AFTER_HITTED == type) {
            for (CacheListener<K, V> listener : cacheListeners) {
                listener.afterHitted(context);
            }
        } else if (LISTENER_BEFORE_ADD == type) {
            for (CacheListener<K, V> listener : cacheListeners) {
                listener.beforeAdd(context);
            }
        } else if (LISTENER_AFTER_ADD == type) {
            for (CacheListener<K, V> listener : cacheListeners) {
                listener.afterAdd(context);
            }
        } else if (LISTENER_BEFORE_UPDATE == type) {
            for (CacheListener<K, V> listener : cacheListeners) {
                listener.beforeUpdate(context);
            }
        } else if (LISTENER_AFTER_UPDATE == type) {
            for (CacheListener<K, V> listener : cacheListeners) {
                listener.afterUpdate(context);
            }
        } else if (LISTENER_BEFORE_EVICT == type) {
            for (CacheListener<K, V> listener : cacheListeners) {
                listener.beforeEvict(context);
            }
        } else if (LISTENER_AFTER_EVICT == type) {
            for (CacheListener<K, V> listener : cacheListeners) {
                listener.afterEvict(context);
            }
        } else if (LISTENER_BEFORE_CLEAR == type) {
            for (CacheListener<K, V> listener : cacheListeners) {
                listener.beforeClear(context);
            }
        } else if (LISTENER_AFTER_CLEAR == type) {
            for (CacheListener<K, V> listener : cacheListeners) {
                listener.afterClear(context);
            }
        } else if (LISTENER_BEFORE_DESTROY == type) {
            for (CacheListener<K, V> listener : cacheListeners) {
                listener.beforeDestroy(context);
            }
        } else if (LISTENER_AFTER_DESTROY == type) {
            for (CacheListener<K, V> listener : cacheListeners) {
                listener.afterDestroy(context);
            }
        } else {
            Tools.assert0("unknown type !");
        }
    }

    /**
     * 周期检查kv是否过期的task
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 4/13/2017 5:37 PM
     */
    private class ExpireTimerTask extends TimerTask {
        @Override
        public void run() {
            List<CacheEntry<K, V>> entries = getAllEntries();
            List<K> needToEvict = new ArrayList<>();
            for (CacheEntry<K, V> entry : entries) {
                if (entry.ttl() != CacheEntry.LONG_LIVE) {
                    Date start = entry.createdAt();
                    if (!fixedTtl) {
                        if ((entry.lastAccessed() != null) && (entry.lastAccessed().compareTo(start) > 0)) {
                            start = entry.lastAccessed();
                        }
                        if ((entry.lastUpdated() != null) && (entry.lastUpdated().compareTo(start) > 0)) {
                            start = entry.lastUpdated();
                        }
                    }

                    if (System.currentTimeMillis() >= (start.getTime() + entry.ttl())) {
                        needToEvict.add(entry.key());
                    }
                }
            }

            if (!needToEvict.isEmpty()) {
                evict(needToEvict);
            }
        }
    }

}
