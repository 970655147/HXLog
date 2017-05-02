package com.hx.log.test;

import com.hx.log.cache.interf.CacheEntryFacade;
import com.hx.log.cache.mem.FIFOMCache;
import com.hx.log.cache.mem.LFUMCache;
import com.hx.log.cache.mem.LRUMCache;
import com.hx.log.cache.interf.Cache;
import com.hx.log.util.Tools;
import org.junit.Test;

import java.util.LinkedHashSet;
import java.util.Set;

import static com.hx.log.util.Log.info;

/**
 * Test14CacheTest
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 3:23 PM
 */
public class Test14CacheTest {

    @Test
    public void test01FIFOCache() {

        int cnt = 3;
        Cache<String, Object> cache = new FIFOMCache<>(2, 2);
        CacheEntryFacade<String, Object> entry00 = null;

        for(int i=0; i<cnt; i++) {
            String iStr = String.valueOf(i);
            cache.put(iStr, iStr);
            if(i == 0) {
                entry00 = cache.getEntry("0");
            }
        }

        info(cache.size());
        info(cache.get("1"));
        cache.put("1", "11");
        CacheEntryFacade<String, Object> entry = cache.getEntry("1");
        info(cache.keys());

        cache.destroy();
    }

    @Test
    public void test02LFUCache() {

        int cnt = 5;
        Cache<String, Object> cache = new LFUMCache<>(3, 3);
        CacheEntryFacade<String, Object> entry00 = null;

        for(int i=0; i<cnt; i++) {
            String iStr = String.valueOf(i);
            cache.put(iStr, iStr);
            if(i == 2) {
                entry00 = cache.getEntry("0");
                entry00 = cache.getEntry("2");
            }
            if(i == 4) {
                info("sdf");
            }
        }

        info(cache.size());
        info(cache.get("1"));
        cache.put("1", "11");
        CacheEntryFacade<String, Object> entry = cache.getEntry("1");
        info(cache.keys());

        cache.destroy();
    }


    @Test
    public void test03LinkedSet() {

        Set<String> set = new LinkedHashSet<>();

        set.add("1");
        set.add("2");
        set.add("3");
        set.add("1");

        info(set.iterator() );

    }


    @Test
    public void test04LRUCache() {

        int cnt = 5;
        Cache<String, Object> cache = new LRUMCache<>(3, 3);
        CacheEntryFacade<String, Object> entry00 = null;

        for(int i=0; i<cnt; i++) {
            String iStr = String.valueOf(i);
            cache.put(iStr, iStr);
            if(i == 2) {
                entry00 = cache.getEntry("0");
                entry00 = cache.getEntry("2");
            }
            if(i == 4) {
                info("sdf");
            }
        }

        info(cache.size());
        info(cache.get("1"));
        cache.put("1", "11");
        CacheEntryFacade<String, Object> entry = cache.getEntry("1");
        info(cache.keys());

        cache.destroy();
    }

    @Test
    public void test05CacheExpireTest() {
        Cache<String, Object> cache = new FIFOMCache<>(2, 2);
        cache.put("1", "1", 3000);

        info(cache.get("1"));
        Tools.sleep(5000);
        info(cache.get("1"));

        info(cache.hitCount());
        info(cache.visitCount());

        cache.destroy();
    }

}
