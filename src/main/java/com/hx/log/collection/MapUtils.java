/**
 * file name : MapUtils.java
 * created at : 21:33:27 2016-12-30
 * created by 970655147
 */

package com.hx.log.collection;

import java.util.List;
import java.util.Map;

import com.hx.log.util.Tools;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

public final class MapUtils {

    // disable constructor
    private MapUtils() {
        Tools.assert0("can't instantiate !");
    }

    // add at 2016.05.07
    // 默认的索引, 默认的返回值
    public static int GET_INFO_FROM_JSON_DEFAULT_IDX = 0;
    public static String DEFAULT_STR_VALUE = "";
    public static int DEFAULT_INT_VALUE = 0;
    public static boolean DEFAULT_BOOLEAN_VALUE = false;
    public static long DEFAULT_LONG_VALUE = 0L;
    public static double DEFAULT_DOUBLE_VALUE = 0.0d;
    public static JSONObject DEFAULT_OBJ_VALUE = null;
    public static JSONArray DEFAULT_ARR_VALUE = null;

    // 获取给定的JSONObject的给定的索引的数据
    // with 'defaultValue'
    public static String getString(Map<String, Object> map, int idx, String[] idxes) {
        return getString(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static String optString(Map<String, Object> map, int idx, String[] idxes) {
        return optString(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static String optString(Map<String, Object> map, int idx, String[] idxes, String defaultValue) {
        return optString(map, idxes[Tools.getIdx(idx, idxes)], defaultValue);
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

    public static boolean getBoolean(Map<String, Object> map, int idx, String[] idxes) {
        return getBoolean(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static boolean optBoolean(Map<String, Object> map, int idx, String[] idxes) {
        return optBoolean(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static boolean optBoolean(Map<String, Object> map, int idx, String[] idxes, boolean defaultValue) {
        return optBoolean(map, idxes[Tools.getIdx(idx, idxes)], defaultValue);
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

    public static double getDouble(Map<String, Object> map, int idx, String[] idxes) {
        return getDouble(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static double optDouble(Map<String, Object> map, int idx, String[] idxes) {
        return optDouble(map, idxes[Tools.getIdx(idx, idxes)]);
    }

    public static double optDouble(Map<String, Object> map, int idx, String[] idxes, double defaultValue) {
        return optDouble(map, idxes[Tools.getIdx(idx, idxes)], defaultValue);
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
    public static String getString(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return getString(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static String optString(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return optString(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static String optString(Map<String, Object> map, int idx, int defaultIdx, String[] idxes, String defaultValue) {
        return optString(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)], defaultValue);
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

    public static double getDouble(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return getDouble(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static double optDouble(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
        return optDouble(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)]);
    }

    public static double optDouble(Map<String, Object> map, int idx, int defaultIdx, String[] idxes, double defaultValue) {
        return optDouble(map, idxes[Tools.getIdx(idx, idxes, defaultIdx)], defaultValue);
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
    public static String getString(List arr, int idx) {
        Object res = arr.get(idx);
        if (res == null) {
            Tools.assert0("got 'Nothing' with idx : " + idx);
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

    public static int getInt(List arr, int idx) {
        Object res = arr.get(idx);
        if (res == null) {
            Tools.assert0("got 'Nothing' with idx : " + idx);
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

    public static boolean getBoolean(List arr, int idx) {
        Object res = arr.get(idx);
        if (res == null) {
            Tools.assert0("got 'Nothing' with idx : " + idx);
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

    public static long getLong(List arr, int idx) {
        Object res = arr.get(idx);
        if (res == null) {
            Tools.assert0("got 'Nothing' with idx : " + idx);
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

    public static double getDouble(List arr, int idx) {
        Object res = arr.get(idx);
        if (res == null) {
            Tools.assert0("got 'Nothing' with idx : " + idx);
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

    public static JSONObject getJSONObject(List arr, int idx) {
        Object res = arr.get(idx);
        if (res == null) {
            Tools.assert0("got 'Nothing' with idx : " + idx);
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
            Tools.assert0("got 'Nothing' with idx : " + idx);
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
    public static String getString(Map<String, Object> map, String key) {
        Object res = map.get(key);
        if (res == null) {
            Tools.assert0("got 'Nothing' with key : " + key);
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

    public static int getInt(Map<String, Object> map, String key) {
        Object res = map.get(key);
        if (res == null) {
            Tools.assert0("got 'Nothing' with key : " + key);
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

    public static boolean getBoolean(Map<String, Object> map, String key) {
        Object res = map.get(key);
        if (res == null) {
            Tools.assert0("got 'Nothing' with key : " + key);
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

    public static long getLong(Map<String, Object> map, String key) {
        Object res = map.get(key);
        if (res == null) {
            Tools.assert0("got 'Nothing' with key : " + key);
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

    public static double getDouble(Map<String, Object> map, String key) {
        Object res = map.get(key);
        if (res == null) {
            Tools.assert0("got 'Nothing' with key : " + key);
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

    public static JSONObject getJSONObject(Map<String, Object> map, String key) {
        Object res = map.get(key);
        if (res == null) {
            Tools.assert0("got 'Nothing' with key : " + key);
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
            Tools.assert0("got 'Nothing' with key : " + key);
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
