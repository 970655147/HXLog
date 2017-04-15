package com.hx.log.json;

import com.hx.log.json.interf.JSON;
import com.hx.log.json.interf.JSONConfig;
import com.hx.log.json.interf.JSONType;
import com.hx.log.str.WordsSeprator;
import com.hx.log.util.Tools;

import java.util.*;

import static com.hx.log.util.Log.info;

/**
 * JSONObject
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/15/2017 11:50 AM
 */
public class JSONObject implements JSON, Map<String, Object> {

    /**
     * 一个表示空的JSONObject的实例
     */
    public static final JSONObject NULL_JSON_OBJECT = new JSONObject();

    /**
     * 添加元素的时候, 如果元素存在是否覆盖
     */
    public static final boolean PUT_FORCE = true;

    /**
     * 存放各个元素
     */
    Map<String, JSON> eles;

    public JSONObject() {
        eles = new LinkedHashMap<>();
    }

    /**
     * 将给定的Object解析为一个JSONObject
     *
     * @param obj    给定的Object
     * @param config 解析JSONObject的时候的配置
     * @return com.hx.log.json.JSONObject
     * @author Jerry.X.He
     * @date 4/15/2017 12:03 PM
     * @since 1.0
     */
    public static JSONObject fromObject(Object obj, JSONConfig config) {
        if (obj == null) {
            return NULL_JSON_OBJECT;
        }

        JSONObject result = new JSONObject();
        if (obj instanceof String) {
            return fromString((String) obj);
        }

        return result;
    }

    public static JSONObject fromObject(Object obj) {
        return fromObject(obj, new SimpleJSONConfig());
    }

    @Override
    public JSONType type() {
        return JSONType.OBJECT;
    }

    @Override
    public Object value() {
        return this;
    }

    @Override
    public boolean isEmpty() {
        return false;
    }

    @Override
    public int size() {
        return 0;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        JSONParseUtils.toString(this, sb);
        return sb.toString();
    }

    @Override
    public String toString(int indentFactor) {
        StringBuilder sb = new StringBuilder();
        JSONParseUtils.toString(this, indentFactor, 1, sb);
        return sb.toString();
    }

    /**
     * 想当前JSONObject中刚添加一个kv pair
     * 如果元素已经存在, 并且force为false, 则不添加该元素
     *
     * @param key   给定的key
     * @param val   给定的value
     * @param force 是否覆盖已有的元素
     * @return com.hx.log.json.JSONObject
     * @author Jerry.X.He
     * @date 4/15/2017 6:29 PM
     * @since 1.0
     */
    public JSONObject element(String key, Object val, boolean force) {
        if (!(eles.containsKey(key) && !force)) {
            eles.put(key, JSONObj.fromObject(val));
        }

        return this;
    }

    public JSONObject element(String key, Object val) {
        return element(key, val, PUT_FORCE);
    }

    public JSONObject element(String key, JSONObject val, boolean force) {
        if (!(eles.containsKey(key) && !force)) {
            eles.put(key, val);
        }

        return this;
    }

    public JSONObject element(String key, JSONObject val) {
        return element(key, val, PUT_FORCE);
    }

    public JSONObject element(String key, JSONArray val, boolean force) {
        if (!(eles.containsKey(key) && !force)) {
            eles.put(key, val);
        }

        return this;
    }

    public JSONObject element(String key, JSONArray val) {
        return element(key, val, PUT_FORCE);
    }

    public JSONObject element(String key, String val, boolean force) {
        if (!(eles.containsKey(key) && !force)) {
            eles.put(key, JSONStr.fromObject(val));
        }

        return this;
    }

    public JSONObject element(String key, String val) {
        return element(key, val, PUT_FORCE);
    }

    public JSONObject element(String key, boolean val, boolean force) {
        if (!(eles.containsKey(key) && !force)) {
            eles.put(key, JSONBool.fromObject(val));
        }

        return this;
    }

    public JSONObject element(String key, boolean val) {
        return element(key, val, PUT_FORCE);
    }

    public JSONObject element(String key, int val, boolean force) {
        if (!(eles.containsKey(key) && !force)) {
            eles.put(key, JSONInt.fromObject(val));
        }

        return this;
    }

    public JSONObject element(String key, int val) {
        return element(key, val, PUT_FORCE);
    }

    public JSONObject element(String key, long val, boolean force) {
        if (!(eles.containsKey(key) && !force)) {
            eles.put(key, JSONLong.fromObject(val));
        }

        return this;
    }

    public JSONObject element(String key, long val) {
        return element(key, val, PUT_FORCE);
    }


    public JSONObject element(String key, float val, boolean force) {
        if (!(eles.containsKey(key) && !force)) {
            eles.put(key, JSONFloat.fromObject(val));
        }

        return this;
    }

    public JSONObject element(String key, float val) {
        return element(key, val, PUT_FORCE);
    }

    public JSONObject element(String key, double val, boolean force) {
        if (!(eles.containsKey(key) && !force)) {
            eles.put(key, JSONDouble.fromObject(val));
        }

        return this;
    }

    public JSONObject element(String key, double val) {
        return element(key, val, PUT_FORCE);
    }

    public Object put(String key, Object val, boolean force) {
        Object result = null;
        if (!(eles.containsKey(key) && !force)) {
            result = eles.put(key, JSONObj.fromObject(val));
        }

        return result;
    }

    public Object put(String key, Object val) {
        return put(key, val, PUT_FORCE);
    }

    public Object put(String key, JSONObject val, boolean force) {
        Object result = null;
        if (!(eles.containsKey(key) && !force)) {
            result = eles.put(key, val);
        }

        return result;
    }

    public Object put(String key, JSONObject val) {
        return element(key, val, PUT_FORCE);
    }

    public Object put(String key, JSONArray val, boolean force) {
        Object result = null;
        if (!(eles.containsKey(key) && !force)) {
            result = eles.put(key, val);
        }

        return result;
    }

    public Object put(String key, JSONArray val) {
        return element(key, val, PUT_FORCE);
    }

    public Object put(String key, String val, boolean force) {
        Object result = null;
        if (!(eles.containsKey(key) && !force)) {
            result = eles.put(key, JSONStr.fromObject(val));
        }

        return result;
    }

    public Object put(String key, String val) {
        return put(key, val, PUT_FORCE);
    }

    public Object put(String key, boolean val, boolean force) {
        Object result = null;
        if (!(eles.containsKey(key) && !force)) {
            result = eles.put(key, JSONBool.fromObject(val));
        }

        return result;
    }

    public Object put(String key, boolean val) {
        return put(key, val, PUT_FORCE);
    }

    public Object put(String key, int val, boolean force) {
        Object result = null;
        if (!(eles.containsKey(key) && !force)) {
            result = eles.put(key, JSONInt.fromObject(val));
        }

        return result;
    }

    public Object put(String key, int val) {
        return put(key, val, PUT_FORCE);
    }

    public Object put(String key, long val, boolean force) {
        Object result = null;
        if (!(eles.containsKey(key) && !force)) {
            result = eles.put(key, JSONLong.fromObject(val));
        }

        return result;
    }

    public Object put(String key, long val) {
        return put(key, val, PUT_FORCE);
    }


    public Object put(String key, float val, boolean force) {
        Object result = null;
        if (!(eles.containsKey(key) && !force)) {
            result = eles.put(key, JSONFloat.fromObject(val));
        }

        return result;
    }

    public Object put(String key, float val) {
        return put(key, val, PUT_FORCE);
    }

    public Object put(String key, double val, boolean force) {
        Object result = null;
        if (!(eles.containsKey(key) && !force)) {
            result = eles.put(key, JSONDouble.fromObject(val));
        }

        return result;
    }

    public Object put(String key, double val) {
        return put(key, val, PUT_FORCE);
    }

    /**
     * 获取key对应的Object
     * 如果不存在, 或者类型不匹配, 抛出异常
     *
     * @param key 给定的key
     * @return java.lang.Object
     * @author Jerry.X.He
     * @date 4/15/2017 7:06 PM
     * @since 1.0
     */
    public Object get(String key) {
        JSON val = eles.get(key);
        if(val == null) {
            Tools.assert0("the key : " + key + " do not exists !");
        }

        return val.value();
    }

    public JSONObject getJSONObject(String key) {
        JSON val = eles.get(key);
        if(val == null || (val.type() != JSONType.OBJECT)) {
            Tools.assert0("the key : " + key + " do not exists or it does not an JSONObject !");
        }

        return (JSONObject) val.value();
    }

    public JSONArray getJSONArray(String key) {
        JSON val = eles.get(key);
        if(val == null || (val.type() != JSONType.ARRAY)) {
            Tools.assert0("the key : " + key + " do not exists or it does not an JSONArray !");
        }

        return (JSONArray) val.value();
    }

    public String getString(String key) {
        JSON val = eles.get(key);
        if(val == null) {
            Tools.assert0("the key : " + key + " do not exists !");
        }

        return String.valueOf(val.value());
    }

    public boolean getBoolean(String key) {
        JSON val = eles.get(key);
        if(val == null || (val.type() != JSONType.BOOL)) {
            Tools.assert0("the key : " + key + " do not exists or it does not an boolean !");
        }

        return (Boolean) val.value();
    }

    public int getInt(String key) {
        JSON val = eles.get(key);
        if(val == null || (val.type() != JSONType.INT)) {
            Tools.assert0("the key : " + key + " do not exists or it does not an int !");
        }

        return (Integer) val.value();
    }

    public long getLong(String key) {
        JSON val = eles.get(key);
        if(val == null || (val.type() != JSONType.LONG)) {
            Tools.assert0("the key : " + key + " do not exists or it does not an long !");
        }

        return (Long) val.value();
    }

    public float getFloat(String key) {
        JSON val = eles.get(key);
        if(val == null || (val.type() != JSONType.FLOAT)) {
            Tools.assert0("the key : " + key + " do not exists or it does not an float !");
        }

        return (Float) val.value();
    }

    public double getDouble(String key) {
        JSON val = eles.get(key);
        if(val == null || (val.type() != JSONType.DOUBLE)) {
            Tools.assert0("the key : " + key + " do not exists or it does not an double !");
        }

        return (Double) val.value();
    }

    /**
     * 获取key对应的Object
     * 如果不存在, 或者类型不匹配, 返回默认结果
     *
     * @param key 给定的key
     * @return java.lang.Object
     * @author Jerry.X.He
     * @date 4/15/2017 7:06 PM
     * @since 1.0
     */
    public Object opt(String key) {
        JSON val = eles.get(key);
        if(val == null) {
            return null;
        }

        return val.value();
    }

    public JSONObject optJSONObject(String key) {
        JSON val = eles.get(key);
        if(val == null || (val.type() != JSONType.OBJECT)) {
            return null;
        }

        return (JSONObject) val.value();
    }

    public JSONArray optJSONArray(String key) {
        JSON val = eles.get(key);
        if(val == null || (val.type() != JSONType.ARRAY)) {
            return null;
        }

        return (JSONArray) val.value();
    }

    public String optString(String key) {
        JSON val = eles.get(key);
        if(val == null) {
            return null;
        }

        return String.valueOf(val.value());
    }

    public boolean optBoolean(String key) {
        JSON val = eles.get(key);
        if(val == null || (val.type() != JSONType.BOOL)) {
            return false;
        }

        return (Boolean) val.value();
    }

    public int optInt(String key) {
        JSON val = eles.get(key);
        if(val == null || (val.type() != JSONType.INT)) {
            return 0;
        }

        return (Integer) val.value();
    }

    public long optLong(String key) {
        JSON val = eles.get(key);
        if(val == null || (val.type() != JSONType.LONG)) {
            return 0L;
        }

        return (Long) val.value();
    }

    public float optFloat(String key) {
        JSON val = eles.get(key);
        if(val == null || (val.type() != JSONType.FLOAT)) {
            return 0F;
        }

        return (Float) val.value();
    }

    public double optDouble(String key) {
        JSON val = eles.get(key);
        if(val == null || (val.type() != JSONType.DOUBLE)) {
            return 0D;
        }

        return (Double) val.value();
    }

    /**
     * 获取当前JSONObject的所有的key的集合的iterator
     *
     * @return com.hx.log.json.interf.JSON
     * @author Jerry.X.He
     * @date 4/15/2017 6:44 PM
     * @since 1.0
     */
    public Iterator<String> keys() {
        return keySet().iterator();
    }

    /**
     * 获取当前JSONObject的所有的key的集合
     *
     * @return com.hx.log.json.interf.JSON
     * @author Jerry.X.He
     * @date 4/15/2017 6:44 PM
     * @since 1.0
     */
    public Set<String> keySet() {
        return eles.keySet();
    }

    @Override
    public boolean containsKey(Object key) {
        return eles.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        for(Entry<String, JSON> entry : eles.entrySet()) {
            if(Objects.equals(entry.getValue(), value)) {
                return true;
            }
        }

        return false;
    }

    @Override
    public Object get(Object key) {
        return eles.get(key).value();
    }

    @Override
    public Object remove(Object key) {
        return eles.remove(key);
    }

    @Override
    public void putAll(Map<? extends String, ?> m) {
        for(Entry<? extends String, ?> entry : m.entrySet()) {
            put(entry.getKey(), entry.getValue());
        }
    }

    @Override
    public Collection<Object> values() {
        Collection<Object> result = new ArrayList<>(size());
        for(Entry<String, JSON> entry : eles.entrySet()) {
            result.add(entry.getValue().value());
        }
        return result;
    }

    @Override
    public Set<Entry<String, Object>> entrySet() {
        Set<Entry<String, Object>> result = new HashSet<>(size());
        for(Entry<String, JSON> entry : eles.entrySet()) {
            result.add(new MapEentry<>(entry.getKey(), entry.getValue().value()) );
        }
        return result;
    }

    /**
     * 移除当前JSONObject中key对应的条目
     *
     * @param key 给定的key
     * @return com.hx.log.json.interf.JSON
     * @author Jerry.X.He
     * @date 4/15/2017 6:44 PM
     * @since 1.0
     */
    public Object remove(String key) {
        return eles.remove(key).value();
    }

    /**
     * 移除当前JSONObject中所有的条目
     *
     * @return com.hx.log.json.interf.JSON
     * @author Jerry.X.He
     * @date 4/15/2017 6:44 PM
     * @since 1.0
     */
    public void clear() {
        eles.clear();
    }

    // ----------------- 辅助数据结构 -----------------------
    private static class MapEentry<K, V> implements Map.Entry<K, V> {
        /**
         * key & value
         */
        private K key;
        private V value;

        public MapEentry(K key, V value) {
            this.key = key;
            this.value = value;
        }

        @Override
        public K getKey() {
            return key;
        }

        @Override
        public V getValue() {
            return value;
        }

        @Override
        public V setValue(V value) {
            throw new RuntimeException("Unsupported Operation Exception !");
        }
    }

    // ----------------- 辅助方法 -----------------------

    /**
     * 解析给定的seprator的剩余的部分, 将其解析为一个JSONObject
     *
     * @param sep 给定的seprator
     * @return com.hx.log.json.JSONObject
     * @author Jerry.X.He
     * @date 4/15/2017 5:32 PM
     * @since 1.0
     */
    static JSONObject fromString(WordsSeprator sep, boolean checkEnd) {
        Tools.assert0(JSONConstants.OBJ_START.equals(sep.next()), "expect a : " + JSONConstants.OBJ_START + " ! around : " + sep.currentAndRest());
        JSONObject result = new JSONObject();

        while (sep.hasNext()) {
            String nextKey = sep.next().trim();
            Tools.assert0(
                    (nextKey.startsWith(JSONConstants.STR_SEP01) && nextKey.endsWith(JSONConstants.STR_SEP01))
                            ||
                            (nextKey.startsWith(JSONConstants.STR_SEP02) && nextKey.endsWith(JSONConstants.STR_SEP02)),
                    "bad key format around : " + sep.currentAndRest()
            );
            Tools.assert0(JSONConstants.KV_SEP.equals(sep.next()), "expect a : " + JSONConstants.KV_SEP + " ! around : " + sep.currentAndRest());
            nextKey = JSONParseUtils.trimForSurroundSep(nextKey, JSONConstants.KEY_SEPS);
            JSON nextValue = JSONParseUtils.getNextValue(sep, nextKey);
            result.eles.put(nextKey, nextValue);

            if (JSONConstants.OBJ_END.equals(sep.seek())) {
                break;
            }
            Tools.assert0(JSONConstants.ELE_SEP.equals(sep.next()), "expect a : " + JSONConstants.ELE_SEP + " ! around : " + sep.currentAndRest());
        }
        // skip '}'
        sep.next();
        if (checkEnd) {
            Tools.assert0(Tools.isEmpty(sep.next()), "expect nothing after '}' !");
        }
        return result;
    }

    static JSONObject fromString(String str) {
        WordsSeprator sep = new WordsSeprator(str, JSONConstants.JSON_SEPS, JSONConstants.NEED_TO_ESCAPE, true, false);
        return fromString(sep, true);
    }


}
