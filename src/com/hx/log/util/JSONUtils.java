/**
 * file name : JSONUtils.java
 * created at : 22:38:27 2016-12-30
 * created by 970655147
 */

package com.hx.log.util;

import com.hx.log.util.interf.IdxIterator;
import java.util.Iterator;
import java.util.Set;

import net.sf.json.JSON;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

public final class JSONUtils {

    // disable constructor
    private JSONUtils() {
        Tools.assert0("can't instantiate !");
    }


    // add at 2016.06.21
    public static <K, V> boolean isJSONEmpty(JSONObject obj) {
        return (obj == null) || (obj.isNullObject()) || (obj.size() == 0) || (obj.isEmpty());
    }


    // 去掉掉obj中所有的字符串类的值的相邻的多个空格
    // 思路 : 如果obj是空对象  则直接返回
    // 否则 遍历各个kv数据, 如果值为String  则去掉其多余的空格, 然后在更新obj中对应key的值
    // 否则如果 值为JSONObject, 递归
    // 否则如果 值为JSONArray, trimSpaces(JSONArray )
    public static void trimSpaces(JSONObject obj) {
        if (obj.isNullObject() || Tools.isEmpty(obj)) {
            return;
        }

        JSONArray names = obj.names();
        Iterator<?> it = names.iterator();
        while (it.hasNext()) {
            String key = String.valueOf(it.next());
            Object val = obj.get(key);
            if (val instanceof String) {
                obj.put(key, Tools.trimSpacesAsOne((String) val));
            } else if (val instanceof JSONObject) {
                trimSpaces((JSONObject) val);
            } else if (val instanceof JSONArray) {
                trimSpaces((JSONArray) val);
            }
        }
    }

    // 去掉掉arr中所有的字符串类的值的相邻的多个空格
    // 思路 : 如果arr是空对象  则直接返回
    // 否则 遍历各个数据, 如果值为String  则去掉其多余的空格, 然后在更新obj中对应key的值
    // 否则如果 值为JSONObject, trimSpaces(JSONObject )
    // 否则如果 值为JSONArray, 递归
    public static void trimSpaces(JSONArray arr) {
        if (Tools.isEmpty(arr)) {
            return;
        }

        for (int i = 0; i < arr.size(); i++) {
            Object val = arr.get(i);
            if (val instanceof String) {
                arr.set(i, Tools.trimSpacesAsOne((String) val));
            } else if (val instanceof JSONObject) {
                trimSpaces((JSONObject) val);
            } else if (val instanceof JSONArray) {
                trimSpaces((JSONArray) val);
            }
        }
    }

    // 确保arr中的每一个JSONObject都存在指定的key, 否则  则删除该条目
    // val.toString可以确保值为null的情形
    public static void removeIfNull(JSONArray arr, String key) {
        if (Tools.isEmpty(arr)) {
            return;
        }

        Iterator<?> it = arr.iterator();
        while (it.hasNext()) {
            Object obj = it.next();
            if (obj instanceof JSONObject) {
                if (!((JSONObject) obj).containsKey(key)) {
                    it.remove();
                } else {
                    Object val = ((JSONObject) obj).get(key);
                    // 1. "{'key' : '' }"
                    // 2. "{'key' : null }"
                    if (Tools.isEmpty(val.toString())) {
                        it.remove();
                    }
                }
            }

        }
    }

    // 去掉掉obj中所有的字符串类的值的相邻的多个空格
    // 思路 : 如果obj是空对象  则直接返回
    // 否则 遍历各个kv数据, 如果值为String  如果只为空  则去掉当前kv对
    // 否则如果 值为JSONObject, 递归,  如果该方法之后, val为空, 则移除val对应的条目
    // 否则如果 值为JSONArray, removeIfNull(JSONArray ),  如果该方法之后, val为空, 则移除val对应的条目
    public static void removeIfNull(JSONObject obj) {
        if (obj.isNullObject() || Tools.isEmpty(obj)) {
            return;
        }

        Iterator<?> it = obj.names().iterator();
        while (it.hasNext()) {
            String key = String.valueOf(it.next());
            Object val = obj.get(key);
            if (val instanceof String) {
                if (Tools.isEmpty((String) val)) {
                    obj.remove(key);
                }
            } else if (val instanceof JSONObject) {
                // 防止 "{'price' : null } "的情形
                if (Tools.isEmpty(val.toString())) {
                    obj.remove(key);
                } else {
                    removeIfNull((JSONObject) val);
                    if (((JSONObject) val).isEmpty()) {
                        obj.remove(key);
                    }
                }
            } else if (val instanceof JSONArray) {
                removeIfNull((JSONArray) val);
                if (((JSONArray) val).isEmpty()) {
                    obj.remove(key);
                }
            }
        }
    }

    // 去掉掉arr中所有的字符串类的值的相邻的多个空格
    // 思路 : 如果arr是空对象  则直接返回
    // 否则 遍历各个数据, 如果值为String  则去掉其多余的空格, 然后在更新obj中对应key的值
    // 否则如果 值为JSONObject, removeIfNull(JSONObject ), 如果该方法之后, val为空, 则移除val对应的条目
    // 注意 : 因为这里对JSONArray进行了删除操作, 所以这里使用了Iterator
    public static void removeIfNull(JSONArray arr) {
        if (Tools.isEmpty(arr)) {
            return;
        }

        Iterator<?> it = arr.iterator();
        while (it.hasNext()) {
            Object val = it.next();
            if (val instanceof String) {
                if (Tools.isEmpty((String) val)) {
                    it.remove();
                }
            } else if (val instanceof JSONObject) {
                // 防止 "{'price' : null } "的情形
                if (Tools.isEmpty(val.toString())) {
                    it.remove();
                } else {
                    removeIfNull((JSONObject) val);
                    if (((JSONObject) val).isEmpty()) {
                        it.remove();
                    }
                }
            }
        }
    }

    /**
     * @param obj            JSON对象
     * @param needBeFiltered 需要过滤掉的key
     * @return
     * @Description: 从obj中移除needBeFiltered包含的所有的key
     * @Create at 2016-12-30 22:42:43 by '970655147'
     */
    public static JSONObject filter(JSONObject obj, Set<String> needBeFiltered) {
        if (Tools.isEmpty(obj) || Tools.isEmpty(needBeFiltered)) {
            return obj;
        }

        for (String filter : needBeFiltered) {
            obj.remove(filter);
        }
        return obj;
    }


    /**
     * @param pattern 给定的pattern
     * @param len     需要提取的JSONArray的长度
     * @return
     * @Description: 根据给定的pattern生成IdxIterator
     * @Create at 2016-12-31 14:04:43 by '970655147'
     */
    public static IdxIterator getIdxIteratorByPattern(String pattern, int len) {
        return JSONExtractor.getIdxIteratorByPattern(pattern, len);
    }

    /**
     * @param json    给定的json对象
     * @param pattern 需要提取数据的pattern
     * @return
     * @Description: 根据给定的pattern从给定的json中提取数据
     * @Create at 2016-12-31 14:05:51 by '970655147'
     */
    public static JSONArray extractInfoFromJSON(JSON json, String pattern) {
        return JSONExtractor.extractInfoFromJSON(json, pattern);
    }


}
