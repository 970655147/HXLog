/**
 * file name : IdxManager.java
 * created at : 5:25:17 PM Jun 18, 2016
 * created by 970655147
 */

package com.hx.log.idx;

import java.util.HashMap;
import java.util.Map;

// IdxManager
public class IdxManager<IdxType> {

    /**
     * doLoad 的Map
     */
    private Map<String, IdxType> doLoad = new HashMap<>();
    /**
     * doFilter 的Map
     */
    private Map<String, IdxType> doFilter = new HashMap<>();

    /**
     * 初始化
     *
     * @param other other
     * @since 1.0
     */
    public IdxManager(IdxManager other) {
        if (other == null) {
            return;
        }

        if (other.doLoad != null) {
            doLoad.putAll(other.doLoad);
        }
        if (other.doFilter != null) {
            doFilter.putAll(other.doFilter);
        }
    }

    public IdxManager() {
    }


    /**
     * setter & getter
     */
    public void putDoLoad(String key, IdxType val) {
        doLoad.put(key, val);
    }

    public void putDoFilter(String key, IdxType val) {
        doFilter.put(key, val);
    }

    public IdxType getDoLoad(String key) {
        return doLoad.get(key);
    }

    public IdxType getDoFilter(String key) {
        return doFilter.get(key);
    }

    public Map<String, IdxType> getDoLoad() {
        return doLoad;
    }

    public Map<String, IdxType> getDoFilter() {
        return doFilter;
    }

}
