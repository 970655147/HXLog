package com.hx.log.json;

import com.hx.log.json.interf.JSON;
import com.hx.log.json.interf.JSONConfig;
import com.hx.log.json.interf.JSONType;
import com.hx.log.str.WordsSeprator;
import com.hx.log.util.Tools;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
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
     * һ����ʾ�յ�JSONObject��ʵ��
     */
    public static final JSONObject NULL_JSON_OBJECT = NullJSONObject.getInstance();

    /**
     * ���Ԫ�ص�ʱ��, ���Ԫ�ش����Ƿ񸲸�
     */
    public static final boolean PUT_FORCE = true;

    /**
     * ��Ÿ���Ԫ��
     */
    Map<String, JSON> eles;

    public JSONObject() {
        eles = new LinkedHashMap<>();
    }

    /**
     * ��������Object����Ϊһ��JSONObject
     *
     * @param obj    ������Object
     * @param config ����JSONObject��ʱ�������
     * @return com.hx.log.json.JSONObject
     * @author Jerry.X.He
     * @date 4/15/2017 12:03 PM
     * @since 1.0
     */
    public static JSONObject fromObject(Object obj, JSONConfig config) {
        if (obj == null) {
            return NULL_JSON_OBJECT;
        }

        if (obj instanceof String) {
            return fromString((String) obj, config);
        } else if (obj instanceof Map) {
            JSONObject result = new JSONObject();
            Map map = (Map) obj;
            for (Object key : map.keySet()) {
                result.put(String.valueOf(key), map.get(key));
            }
            return result;
        } else {
            return fromBean(obj, config);
        }
    }

    /**
     * ��������JSONObjectת��Ϊһ��ʵ��
     *
     * @param obj    ������JSONObject
     * @param config ת���������JSONConfig
     * @param clazz  ������ʵ���Class
     * @return T
     * @author Jerry.X.He
     * @date 4/16/2017 12:10 PM
     * @since 1.0
     */
    public static <T> T toBean(JSONObject obj, JSONConfig config, Class<T> clazz) {
        return toBean0(obj, config, clazz);
    }

    public static <T> T toBean(JSONObject obj, Class<T> clazz) {
        return toBean(obj, new SimpleJSONConfig(), clazz);
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
    public boolean isNull() {
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
     * �뵱ǰJSONObject�и����һ��kv pair
     * ���Ԫ���Ѿ�����, ����forceΪfalse, ����Ӹ�Ԫ��
     *
     * @param key   ������key
     * @param val   ������value
     * @param force �Ƿ񸲸����е�Ԫ��
     * @return com.hx.log.json.JSONObject
     * @author Jerry.X.He
     * @date 4/15/2017 6:29 PM
     * @since 1.0
     */
    public JSONObject element(String key, Object val, boolean force) {
        put(key, val, force);
        return this;
    }

    public JSONObject element(String key, Object val) {
        return element(key, val, PUT_FORCE);
    }

    public JSONObject element(String key, JSONObject val, boolean force) {
        put(key, val, force);
        return this;
    }

    public JSONObject element(String key, JSONObject val) {
        return element(key, val, PUT_FORCE);
    }

    public JSONObject element(String key, JSONArray val, boolean force) {
        put(key, val, force);
        return this;
    }

    public JSONObject element(String key, JSONArray val) {
        return element(key, val, PUT_FORCE);
    }

    public JSONObject element(String key, String val, boolean force) {
        put(key, val, force);
        return this;
    }

    public JSONObject element(String key, String val) {
        return element(key, val, PUT_FORCE);
    }

    public JSONObject element(String key, boolean val, boolean force) {
        put(key, val, force);
        return this;
    }

    public JSONObject element(String key, boolean val) {
        return element(key, val, PUT_FORCE);
    }

    public JSONObject element(String key, int val, boolean force) {
        put(key, val, force);
        return this;
    }

    public JSONObject element(String key, int val) {
        return element(key, val, PUT_FORCE);
    }

    public JSONObject element(String key, long val, boolean force) {
        put(key, val, force);
        return this;
    }

    public JSONObject element(String key, long val) {
        return element(key, val, PUT_FORCE);
    }


    public JSONObject element(String key, float val, boolean force) {
        put(key, val, force);
        return this;
    }

    public JSONObject element(String key, float val) {
        return element(key, val, PUT_FORCE);
    }

    public JSONObject element(String key, double val, boolean force) {
        put(key, val, force);
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
     * ��ȡkey��Ӧ��Object
     * ���������, �������Ͳ�ƥ��, �׳��쳣
     *
     * @param key ������key
     * @return java.lang.Object
     * @author Jerry.X.He
     * @date 4/15/2017 7:06 PM
     * @since 1.0
     */
    public Object get(String key) {
        JSON val = eles.get(key);
        if (val == null) {
            Tools.assert0("the key : " + key + " do not exists !");
        }

        return val.value();
    }

    public JSONObject getJSONObject(String key) {
        JSON val = eles.get(key);
        if (val == null || (val.type() != JSONType.OBJECT)) {
            Tools.assert0("the key : " + key + " do not exists or it does not an JSONObject !");
        }

        return (JSONObject) val.value();
    }

    public JSONArray getJSONArray(String key) {
        JSON val = eles.get(key);
        if (val == null || (val.type() != JSONType.ARRAY)) {
            Tools.assert0("the key : " + key + " do not exists or it does not an JSONArray !");
        }

        return (JSONArray) val.value();
    }

    public String getString(String key) {
        JSON val = eles.get(key);
        if (val == null) {
            Tools.assert0("the key : " + key + " do not exists !");
        }

        return String.valueOf(val.value());
    }

    public boolean getBoolean(String key) {
        JSON val = eles.get(key);
        if (val == null || (val.type() != JSONType.BOOL)) {
            Tools.assert0("the key : " + key + " do not exists or it does not an boolean !");
        }

        return (Boolean) val.value();
    }

    public int getInt(String key) {
        JSON val = eles.get(key);
        if (val == null || (val.type() != JSONType.INT)) {
            Tools.assert0("the key : " + key + " do not exists or it does not an int !");
        }

        return (Integer) val.value();
    }

    public long getLong(String key) {
        JSON val = eles.get(key);
        if (val == null || (val.type() != JSONType.LONG)) {
            Tools.assert0("the key : " + key + " do not exists or it does not an long !");
        }

        return (Long) val.value();
    }

    public float getFloat(String key) {
        JSON val = eles.get(key);
        if (val == null || (val.type() != JSONType.FLOAT)) {
            Tools.assert0("the key : " + key + " do not exists or it does not an float !");
        }

        return (Float) val.value();
    }

    public double getDouble(String key) {
        JSON val = eles.get(key);
        if (val == null || (val.type() != JSONType.DOUBLE)) {
            Tools.assert0("the key : " + key + " do not exists or it does not an double !");
        }

        return (Double) val.value();
    }

    /**
     * ��ȡkey��Ӧ��Object
     * ���������, �������Ͳ�ƥ��, ����Ĭ�Ͻ��
     *
     * @param key ������key
     * @return java.lang.Object
     * @author Jerry.X.He
     * @date 4/15/2017 7:06 PM
     * @since 1.0
     */
    public Object opt(String key) {
        JSON val = eles.get(key);
        if (val == null) {
            return null;
        }

        return val.value();
    }

    public JSONObject optJSONObject(String key) {
        JSON val = eles.get(key);
        if (val == null || (val.type() != JSONType.OBJECT)) {
            return null;
        }

        return (JSONObject) val.value();
    }

    public JSONArray optJSONArray(String key) {
        JSON val = eles.get(key);
        if (val == null || (val.type() != JSONType.ARRAY)) {
            return null;
        }

        return (JSONArray) val.value();
    }

    public String optString(String key) {
        JSON val = eles.get(key);
        if (val == null) {
            return null;
        }

        return String.valueOf(val.value());
    }

    public boolean optBoolean(String key) {
        JSON val = eles.get(key);
        if (val == null || (val.type() != JSONType.BOOL)) {
            return false;
        }

        return (Boolean) val.value();
    }

    public int optInt(String key) {
        JSON val = eles.get(key);
        if (val == null || (val.type() != JSONType.INT)) {
            return 0;
        }

        return (Integer) val.value();
    }

    public long optLong(String key) {
        JSON val = eles.get(key);
        if (val == null || (val.type() != JSONType.LONG)) {
            return 0L;
        }

        return (Long) val.value();
    }

    public float optFloat(String key) {
        JSON val = eles.get(key);
        if (val == null || (val.type() != JSONType.FLOAT)) {
            return 0F;
        }

        return (Float) val.value();
    }

    public double optDouble(String key) {
        JSON val = eles.get(key);
        if (val == null || (val.type() != JSONType.DOUBLE)) {
            return 0D;
        }

        return (Double) val.value();
    }

    /**
     * ��ȡ��ǰJSONObject�����е�key�ļ��ϵ�iterator
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
     * ��ȡ��ǰJSONObject�����е�key�ļ���
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
        for (Entry<String, JSON> entry : eles.entrySet()) {
            if (Objects.equals(entry.getValue().value(), value)) {
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
        for (Entry<? extends String, ?> entry : m.entrySet()) {
            put(entry.getKey(), entry.getValue());
        }
    }

    @Override
    public Collection<Object> values() {
        Collection<Object> result = new ArrayList<>(size());
        for (Entry<String, JSON> entry : eles.entrySet()) {
            result.add(entry.getValue().value());
        }
        return result;
    }

    @Override
    public Set<Entry<String, Object>> entrySet() {
        Set<Entry<String, Object>> result = new LinkedHashSet<>(size());
        for (Entry<String, JSON> entry : eles.entrySet()) {
            result.add(new MapEentry<>(entry.getKey(), entry.getValue().value()));
        }
        return result;
    }

    /**
     * �Ƴ���ǰJSONObject��key��Ӧ����Ŀ
     *
     * @param key ������key
     * @return com.hx.log.json.interf.JSON
     * @author Jerry.X.He
     * @date 4/15/2017 6:44 PM
     * @since 1.0
     */
    public Object remove(String key) {
        return eles.remove(key).value();
    }

    /**
     * �Ƴ���ǰJSONObject�����е���Ŀ
     *
     * @return com.hx.log.json.interf.JSON
     * @author Jerry.X.He
     * @date 4/15/2017 6:44 PM
     * @since 1.0
     */
    public void clear() {
        eles.clear();
    }

    // ----------------- �������ݽṹ -----------------------
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

    // ----------------- �������� -----------------------

    /**
     * ����������seprator��ʣ��Ĳ���, �������Ϊһ��JSONObject
     *
     * @param sep ������seprator
     * @return com.hx.log.json.JSONObject
     * @author Jerry.X.He
     * @date 4/15/2017 5:32 PM
     * @since 1.0
     */
    static JSONObject fromString(WordsSeprator sep, JSONConfig config, boolean checkEnd) {
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
            JSON nextValue = JSONParseUtils.getNextValue(sep, nextKey, config);
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

    static JSONObject fromString(String str, JSONConfig config) {
        WordsSeprator sep = new WordsSeprator(str, JSONConstants.JSON_SEPS, JSONConstants.NEED_TO_ESCAPE, true, false);
        return fromString(sep, config, true);
    }

    /**
     * �Ӹ�����Object�н���JSONObject
     *
     * @param obj    ������object
     * @param config ����object������
     * @return com.hx.log.json.JSONObject
     * @author Jerry.X.He
     * @date 4/16/2017 11:49 AM
     * @since 1.0
     */
    static JSONObject fromBean(Object obj, JSONConfig config) {
        JSONObject result = new JSONObject();
        Class clazz = obj.getClass();
        int clazzModifier = clazz.getModifiers();
        if ((!Modifier.isPublic(clazzModifier)) || (Modifier.isAbstract(clazzModifier))) {
            return NULL_JSON_OBJECT;
        }

        Method[] methods = clazz.getDeclaredMethods();
        try {
            for (Method method : methods) {
                String methodName = method.getName();
                int modifier = method.getModifiers();

                if (JSONParseUtils.startsWith(methodName, JSONConstants.BEAN_GETTER_PREFIXES)
                        && (Modifier.isPublic(modifier) && (!Modifier.isStatic(modifier) && (!Modifier.isAbstract(modifier))))
                        && (method.getParameterTypes().length == 0)) {
                    Object invokeResult = method.invoke(obj);
                    String key = Tools.lowerCaseFirstChar(JSONParseUtils.trimIfStartsWith(methodName, JSONConstants.BEAN_GETTER_PREFIXES));

                    Class resultClazz = invokeResult.getClass();
                    if ((boolean.class == resultClazz) || (Boolean.class == resultClazz)) {
                        result.put(key, Boolean.valueOf(invokeResult.toString()).booleanValue());
                    } else if (((int.class == resultClazz) || (Integer.class == resultClazz))
                            || ((byte.class == resultClazz) || (Byte.class == resultClazz))
                            || ((short.class == resultClazz) || (Short.class == resultClazz))
                            ) {
                        result.put(key, Integer.valueOf(invokeResult.toString()).intValue());
                    } else if ((long.class == resultClazz) || (Long.class == resultClazz)) {
                        result.put(key, Long.valueOf(invokeResult.toString()).longValue());
                    } else if ((float.class == resultClazz) || (Float.class == resultClazz)) {
                        result.put(key, Float.valueOf(invokeResult.toString()).floatValue());
                    } else if ((double.class == resultClazz) || (Double.class == resultClazz)) {
                        result.put(key, Double.valueOf(invokeResult.toString()).doubleValue());
                    } else {
                        result.put(key, invokeResult);
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            return NULL_JSON_OBJECT;
        }

        return result;
    }

    /**
     * ��������JSONObjectת��Ϊһ��ʵ��
     *
     * @param obj    ������JSONObject
     * @param config ת���������JSONConfig
     * @param clazz  ������ʵ���Class
     * @return T
     * @author Jerry.X.He
     * @date 4/16/2017 12:10 PM
     * @since 1.0
     */
    private static <T> T toBean0(JSONObject obj, JSONConfig config, Class<T> clazz) {
        int clazzModifier = clazz.getModifiers();
        if ((!Modifier.isPublic(clazzModifier)) || (Modifier.isAbstract(clazzModifier))) {
            return null;
        }

        Method[] methods = clazz.getDeclaredMethods();
        try {
            T result = clazz.newInstance();
            for (Method method : methods) {
                String methodName = method.getName();
                int modifier = method.getModifiers();

                if (JSONParseUtils.startsWith(methodName, JSONConstants.BEAN_SETTER_PREFIXES)
                        && (Modifier.isPublic(modifier) && (!Modifier.isStatic(modifier) && (!Modifier.isAbstract(modifier))))
                        && (method.getParameterTypes().length == 1)) {
                    String key = Tools.lowerCaseFirstChar(JSONParseUtils.trimIfStartsWith(methodName, JSONConstants.BEAN_SETTER_PREFIXES));
                    Class argClazz = method.getParameterTypes()[0];
                    if ((boolean.class == argClazz) || (Boolean.class == argClazz)) {
                        method.invoke(result, obj.optBoolean(key));
                    } else if (((int.class == argClazz) || (Integer.class == argClazz))
                            || ((byte.class == argClazz) || (Byte.class == argClazz))
                            || ((short.class == argClazz) || (Short.class == argClazz))
                            ) {
                        method.invoke(result, obj.optInt(key));
                    } else if ((long.class == argClazz) || (Long.class == argClazz)) {
                        method.invoke(result, obj.optLong(key));
                    } else if ((float.class == argClazz) || (Float.class == argClazz)) {
                        method.invoke(result, obj.optFloat(key));
                    } else if ((double.class == argClazz) || (Double.class == argClazz)) {
                        method.invoke(result, obj.optDouble(key));
                    } else {
                        method.invoke(result, obj.opt(key));
                    }
                }
            }

            return result;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

}
