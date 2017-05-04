/**
 * file name : MapUtils.java
 * created at : 21:33:27 2016-12-30
 * created by 970655147
 */

package com.hx.log.collection;

import java.util.List;
import java.util.Map;

import com.hx.log.util.Tools;
import com.hx.json.JSONArray;
import com.hx.json.JSONObject;

import static com.hx.log.util.Tools.assert0;

public final class MapUtils {

    // disable constructor
    private MapUtils() {
        assert0("can't instantiate !");
    }

    // add at 2016.05.07
    /**
     * 默认的索引
     */
    public static int GET_INFO_FROM_JSON_DEFAULT_IDX = 0;
    /**
     * 默认的boolean的值
     */
    public static boolean DEFAULT_BOOLEAN_VALUE = false;
    /**
     * 默认的int的值
     */
    public static int DEFAULT_INT_VALUE = 0;
    /**
     * 默认的long的值
     */
    public static long DEFAULT_LONG_VALUE = 0L;
    /**
     * 默认的double的值
     */
    public static float DEFAULT_FLOAT_VALUE = 0.0F;
    /**
     * 默认的double的值
     */
    public static double DEFAULT_DOUBLE_VALUE = 0.0D;
    /**
     * 默认的字符串的值
     */
    public static String DEFAULT_STR_VALUE = "";
    /**
     * 默认的JSONObject的值
     */
    public static JSONObject DEFAULT_OBJ_VALUE = null;
    /**
     * 默认的JSONArray的值
     */
    public static JSONArray DEFAULT_ARR_VALUE = null;
    /**
     * 默认的Object的值
     */
    public static Object DEFAULT_OBJECT_VALUE = null;


    // 获取给定的JSONObject的给定的索引的数据
    // with 'defaultValue'
    /**
     * 从list中获取idxes[idx]对应的Object, 如果没有 抛出异常
     *
     * @param map          给定的map
     * @param idx          给定的key
     * @return java.lang.Object
     * @author Jerry.X.He
     * @date 5/4/2017 11:35 PM
     * @since 1.0
     */
    public static Object get(Map<String, Object> map, int idx, String[] idxes) {
        return get(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static Object opt(Map<String, Object> map, int idx, String[] idxes) {
        return opt(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static Object opt(Map<String, Object> map, int idx, String[] idxes, Object defaultValue) {
        return opt(map, idxes[Tools.getIdx(idx, idxes)], defaultValue);
    }

    public static boolean getBoolean(Map<String, Object> map, int idx, String[] idxes) {
        return getBoolean(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static boolean optBoolean(Map<String, Object> map, int idx, String[] idxes) {
        return optBoolean(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static boolean optBoolean(Map<String, Object> map, int idx, String[] idxes, boolean defaultValue) {
        return optBoolean(map, idxes[Tools.getIdx(idx, idxes)], defaultValue);
    }

    public static int getInt(Map<String, Object> map, int idx, String[] idxes) {
        return getInt(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static int optInt(Map<String, Object> map, int idx, String[] idxes) {
        return optInt(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static int optInt(Map<String, Object> map, int idx, String[] idxes, int defaultValue) {
        return optInt(map, idxes[Tools.getIdx(idx, idxes)], defaultValue);
    }

    public static long getLong(Map<String, Object> map, int idx, String[] idxes) {
        return getLong(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static long optLong(Map<String, Object> map, int idx, String[] idxes) {
        return optLong(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static long optLong(Map<String, Object> map, int idx, String[] idxes, long defaultValue) {
        return optLong(map, idxes[Tools.getIdx(idx, idxes)], defaultValue);
    }

    public static float getFloat(Map<String, Object> map, int idx, String[] idxes) {
        return getFloat(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static float optFloat(Map<String, Object> map, int idx, String[] idxes) {
        return optFloat(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static float optFloat(Map<String, Object> map, int idx, String[] idxes, float defaultValue) {
        return optFloat(map, idxes[Tools.getIdx(idx, idxes)], defaultValue);
    }

    public static double getDouble(Map<String, Object> map, int idx, String[] idxes) {
        return getDouble(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static double optDouble(Map<String, Object> map, int idx, String[] idxes) {
        return optDouble(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static double optDouble(Map<String, Object> map, int idx, String[] idxes, double defaultValue) {
        return optDouble(map, idxes[Tools.getIdx(idx, idxes)], defaultValue);
    }

    public static String getString(Map<String, Object> map, int idx, String[] idxes) {
        return getString(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static String optString(Map<String, Object> map, int idx, String[] idxes) {
        return optString(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static String optString(Map<String, Object> map, int idx, String[] idxes, String defaultValue) {
        return optString(map, idxes[Tools.getIdx(idx, idxes)], defaultValue);
    }

    public static JSONObject getJSONObject(Map<String, Object> map, int idx, String[] idxes) {
        return getJSONObject(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static JSONObject optJSONObject(Map<String, Object> map, int idx, String[] idxes) {
        return optJSONObject(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static JSONArray getJSONArray(Map<String, Object> map, int idx, String[] idxes) {
        return getJSONArray(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static JSONArray optJSONArray(Map<String, Object> map, int idx, String[] idxes) {
        return optJSONArray(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    // with 'defaultIdx'
    /**
     * 从map中获取idxes[idx]对应的Object, 如果没有 抛出异常
     *
     * @param map          给定的map
     * @param idx          给定的key
     * @return java.lang.Object
     * @author Jerry.X.He
     * @date 5/4/2017 11:35 PM
     * @since 1.0
     */
    public static Object get(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return get(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static Object opt(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return opt(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static Object opt(Map<String, Object> map, int idx, int defaultIdx, String[] idxes, Object defaultValue) {
        return opt(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)], defaultValue);
    }

    public static boolean getBoolean(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return getBoolean(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static boolean optBoolean(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return optBoolean(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static boolean optBoolean(Map<String, Object> map, int idx, int defaultIdx, String[] idxes, boolean defaultValue) {
        return optBoolean(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)], defaultValue);
    }

    public static int getInt(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return getInt(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static int optInt(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return optInt(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static int optInt(Map<String, Object> map, int idx, int defaultIdx, String[] idxes, int defaultValue) {
        return optInt(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)], defaultValue);
    }

    public static long getLong(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return getLong(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static long optLong(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return optLong(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static long optLong(Map<String, Object> map, int idx, int defaultIdx, String[] idxes, long defaultValue) {
        return optLong(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)], defaultValue);
    }

    public static float getFloat(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return getFloat(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static float optFloat(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return optFloat(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static float optFloat(Map<String, Object> map, int idx, int defaultIdx, String[] idxes, float defaultValue) {
        return optFloat(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)], defaultValue);
    }

    public static double getDouble(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return getDouble(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static double optDouble(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return optDouble(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static double optDouble(Map<String, Object> map, int idx, int defaultIdx, String[] idxes, double defaultValue) {
        return optDouble(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)], defaultValue);
    }

    public static String getString(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return getString(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static String optString(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return optString(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static String optString(Map<String, Object> map, int idx, int defaultIdx, String[] idxes, String defaultValue) {
        return optString(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)], defaultValue);
    }

    public static JSONObject getJSONObject(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return getJSONObject(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static JSONObject optJSONObject(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return optJSONObject(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static JSONArray getJSONOArray(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return getJSONArray(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static JSONArray optJSONArray(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return optJSONArray(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    // add getString / Int / ...(List, int) 		at 2016.06.02
    /**
     * 从list中获取idx对应的Object, 如果没有 抛出异常
     *
     * @param arr          给定的map
     * @param idx          给定的key
     * @return java.lang.Object
     * @author Jerry.X.He
     * @date 5/4/2017 11:35 PM
     * @since 1.0
     */
    public static Object get(List arr, int idx) {
        Object res = arr.get(idx);
        if (res == null) {
            assert0("got 'Nothing' with idx : " + idx);
        }
        return res;
    }

    public static Object opt(List arr, int idx, Object defaultValue) {
        Object res = arr.get(idx);
        if (res == null) {
            return defaultValue;
        }
        return res;
    }

    public static Object opt(List arr, int idx) {
        return opt(arr, idx, DEFAULT_OBJECT_VALUE);
    }

    public static boolean getBoolean(List arr, int idx) {
        Object res = arr.get(idx);
        if (res == null) {
            assert0("got 'Nothing' with idx : " + idx);
        }
        return Boolean.valueOf(res.toString());
    }

    public static boolean optBoolean(List arr, int idx, boolean defaultValue) {
        Object res = arr.get(idx);
        if (res == null) {
            return defaultValue;
        }
        return Boolean.valueOf(res.toString());
    }

    public static boolean optBoolean(List arr, int idx) {
        return optBoolean(arr, idx, DEFAULT_BOOLEAN_VALUE);
    }

    public static int getInt(List arr, int idx) {
        Object res = arr.get(idx);
        if (res == null) {
            assert0("got 'Nothing' with idx : " + idx);
        }
        return Integer.valueOf(res.toString());
    }

    public static int optInt(List arr, int idx, int defaultValue) {
        Object res = arr.get(idx);
        if (res == null) {
            return defaultValue;
        }
        return Integer.valueOf(res.toString());
    }

    public static int optInt(List arr, int idx) {
        return optInt(arr, idx, DEFAULT_INT_VALUE);
    }

    public static long getLong(List arr, int idx) {
        Object res = arr.get(idx);
        if (res == null) {
            assert0("got 'Nothing' with idx : " + idx);
        }
        return Long.valueOf(res.toString());
    }

    public static long optLong(List arr, int idx, long defaultValue) {
        Object res = arr.get(idx);
        if (res == null) {
            return defaultValue;
        }
        return Long.valueOf(res.toString());
    }

    public static long optLong(List arr, int idx) {
        return optLong(arr, idx, DEFAULT_LONG_VALUE);
    }

    public static float getFloat(List arr, int idx) {
        Object res = arr.get(idx);
        if (res == null) {
            assert0("got 'Nothing' with idx : " + idx);
        }
        return Float.valueOf(res.toString());
    }

    public static float optFloat(List arr, int idx, float defaultValue) {
        Object res = arr.get(idx);
        if (res == null) {
            return defaultValue;
        }
        return Float.valueOf(res.toString());
    }

    public static float optFloat(List arr, int idx) {
        return optFloat(arr, idx, DEFAULT_FLOAT_VALUE);
    }

    public static double getDouble(List arr, int idx) {
        Object res = arr.get(idx);
        if (res == null) {
            assert0("got 'Nothing' with idx : " + idx);
        }
        return Double.valueOf(res.toString());
    }

    public static double optDouble(List arr, int idx, double defaultValue) {
        Object res = arr.get(idx);
        if (res == null) {
            return defaultValue;
        }
        return Double.valueOf(res.toString());
    }

    public static double optDouble(List arr, int idx) {
        return optDouble(arr, idx, DEFAULT_DOUBLE_VALUE);
    }

    public static String getString(List arr, int idx) {
        Object res = arr.get(idx);
        if (res == null) {
            assert0("got 'Nothing' with idx : " + idx);
        }
        return res.toString();
    }

    public static String optString(List arr, int idx, String defaultValue) {
        Object res = arr.get(idx);
        if (res == null) {
            return defaultValue;
        }
        return res.toString();
    }

    public static String optString(List arr, int idx) {
        return optString(arr, idx, DEFAULT_STR_VALUE);
    }

    public static JSONObject getJSONObject(List arr, int idx) {
        Object res = arr.get(idx);
        if (res == null) {
            assert0("got 'Nothing' with idx : " + idx);
        }
        return JSONObject.fromObject(res);
    }

    public static JSONObject optJSONObject(List arr, int idx, JSONObject defaultValue) {
        Object res = arr.get(idx);
        if (res == null) {
            return defaultValue;
        }
        return JSONObject.fromObject(res.toString());
    }

    public static JSONObject optJSONObject(List arr, int idx) {
        return optJSONObject(arr, idx, DEFAULT_OBJ_VALUE);
    }

    public static JSONArray getJSONArray(List arr, int idx) {
        Object res = arr.get(idx);
        if (res == null) {
            assert0("got 'Nothing' with idx : " + idx);
        }
        return JSONArray.fromObject(res);
    }

    public static JSONArray optJSONArray(List arr, int idx, JSONArray defaultValue) {
        Object res = arr.get(idx);
        if (res == null) {
            return defaultValue;
        }
        return JSONArray.fromObject(res.toString());
    }

    public static JSONArray optJSONArray(List arr, int idx) {
        return optJSONArray(arr, idx, DEFAULT_ARR_VALUE);
    }

    // get / optString (map, key, defaultValue)
    /**
     * 从map中获取key对应的额Object, 如果没有 抛出异常
     *
     * @param map          给定的map
     * @param key          给定的key
     * @return java.lang.Object
     * @author Jerry.X.He
     * @date 5/4/2017 11:35 PM
     * @since 1.0
     */
    public static Object get(Map<String, Object> map, String key) {
        Object res = map.get(key);
        if (res == null) {
            assert0("got 'Nothing' with key : " + key);
        }
        return res;
    }

    public static Object opt(Map<String, Object> map, String key, Object defaultValue) {
        Object res = map.get(key);
        if (res == null) {
            return defaultValue;
        }
        return res;
    }

    public static Object opt(Map<String, Object> map, String key) {
        return opt(map, key, DEFAULT_OBJECT_VALUE);
    }

    public static boolean getBoolean(Map<String, Object> map, String key) {
        Object res = map.get(key);
        if (res == null) {
            assert0("got 'Nothing' with key : " + key);
        }
        return Boolean.valueOf(res.toString());
    }

    public static boolean optBoolean(Map<String, Object> map, String key, boolean defaultValue) {
        Object res = map.get(key);
        if (res == null) {
            return defaultValue;
        }
        return Boolean.valueOf(res.toString());
    }

    public static boolean optBoolean(Map<String, Object> map, String key) {
        return optBoolean(map, key, DEFAULT_BOOLEAN_VALUE);
    }

    public static int getInt(Map<String, Object> map, String key) {
        Object res = map.get(key);
        if (res == null) {
            assert0("got 'Nothing' with key : " + key);
        }
        return Integer.valueOf(res.toString());
    }

    public static int optInt(Map<String, Object> map, String key, int defaultValue) {
        Object res = map.get(key);
        if (res == null) {
            return defaultValue;
        }
        return Integer.valueOf(res.toString());
    }

    public static int optInt(Map<String, Object> map, String key) {
        return optInt(map, key, DEFAULT_INT_VALUE);
    }

    public static long getLong(Map<String, Object> map, String key) {
        Object res = map.get(key);
        if (res == null) {
            assert0("got 'Nothing' with key : " + key);
        }
        return Long.valueOf(String.valueOf(res));
    }

    public static long optLong(Map<String, Object> map, String key, long defaultValue) {
        Object res = map.get(key);
        if (res == null) {
            return defaultValue;
        }
        return Long.valueOf(res.toString());
    }

    public static long optLong(Map<String, Object> map, String key) {
        return optLong(map, key, DEFAULT_LONG_VALUE);
    }

    public static float getFloat(Map<String, Object> map, String key) {
        Object res = map.get(key);
        if (res == null) {
            assert0("got 'Nothing' with key : " + key);
        }
        return Float.valueOf(res.toString());
    }

    public static float optFloat(Map<String, Object> map, String key, float defaultValue) {
        Object res = map.get(key);
        if (res == null) {
            return defaultValue;
        }
        return Float.valueOf(res.toString());
    }

    public static float optFloat(Map<String, Object> map, String key) {
        return optFloat(map, key, DEFAULT_FLOAT_VALUE);
    }

    public static double getDouble(Map<String, Object> map, String key) {
        Object res = map.get(key);
        if (res == null) {
            assert0("got 'Nothing' with key : " + key);
        }
        return Double.valueOf(res.toString());
    }

    public static double optDouble(Map<String, Object> map, String key, double defaultValue) {
        Object res = map.get(key);
        if (res == null) {
            return defaultValue;
        }
        return Double.valueOf(res.toString());
    }

    public static double optDouble(Map<String, Object> map, String key) {
        return optDouble(map, key, DEFAULT_DOUBLE_VALUE);
    }

    public static String getString(Map<String, Object> map, String key) {
        Object res = map.get(key);
        if (res == null) {
            assert0("got 'Nothing' with key : " + key);
        }
        return res.toString();
    }

    public static String optString(Map<String, Object> map, String key, String defaultValue) {
        Object res = map.get(key);
        if (res == null) {
            return defaultValue;
        }
        return res.toString();
    }

    public static String optString(Map<String, Object> map, String key) {
        return optString(map, key, DEFAULT_STR_VALUE);
    }

    public static JSONObject getJSONObject(Map<String, Object> map, String key) {
        Object res = map.get(key);
        if (res == null) {
            assert0("got 'Nothing' with key : " + key);
        }
        return JSONObject.fromObject(res);
    }

    public static JSONObject optJSONObject(Map<String, Object> map, String key, JSONObject defaultValue) {
        Object res = map.get(key);
        if (res == null) {
            return defaultValue;
        }
        return JSONObject.fromObject(res.toString());
    }

    public static JSONObject optJSONObject(Map<String, Object> map, String key) {
        return optJSONObject(map, key, DEFAULT_OBJ_VALUE);
    }

    public static JSONArray getJSONArray(Map<String, Object> map, String key) {
        Object res = map.get(key);
        if (res == null) {
            assert0("got 'Nothing' with key : " + key);
        }
        return JSONArray.fromObject(res);
    }

    public static JSONArray optJSONArray(Map<String, Object> map, String key, JSONArray defaultValue) {
        Object res = map.get(key);
        if (res == null) {
            return defaultValue;
        }
        return JSONArray.fromObject(res.toString());
    }

    public static JSONArray optJSONArray(Map<String, Object> map, String key) {
        return optJSONArray(map, key, DEFAULT_ARR_VALUE);
    }


    /**
     * @param size 需要存放的元素的个数
     * @return
     * @Name: estimateMapSize
     * @Description: 计算封装给定size个元素, HashMap需要的容量
     * @Create at 2016-12-14 20:08:33 by '970655147'
     */
    public static int estimateMapSize(int size) {
        return size + (size >> 1);
    }


}
