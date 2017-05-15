/**
 * file name : JSONUtils.java
 * created at : 22:38:27 2016-12-30
 * created by 970655147
 */

package com.hx.log.json;

import com.hx.json.JSONArray;
import com.hx.json.JSONObject;
import com.hx.json.interf.JSON;
import com.hx.common.interf.idx.IdxIterator;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.hx.log.json.interf.CheckRequiredAttributeFunc;
import com.hx.log.util.Tools;

import static com.hx.log.util.Tools.assert0;

/**
 * JSON相关的工具类
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 4:26 PM
 */
public final class JSONUtils {

    // disable constructor
    private JSONUtils() {
        assert0("can't instantiate !");
    }


    // add at 2016.06.21

    /**
     * 判断给定的JSONObject是否为空
     *
     * @param obj 给定的JSONObject
     * @return boolean
     * @author Jerry.X.He
     * @date 5/5/2017 4:18 PM
     * @since 1.0
     */
    public static <K, V> boolean isJSONEmpty(JSONObject obj) {
        return (obj == null) || (obj.isNull()) || (obj.size() == 0) || (obj.isEmpty());
    }

    /**
     * 去掉掉obj中所有的字符串类的值的相邻的多个空格
     * 思路 : 如果obj是空对象  则直接返回
     * 否则 遍历各个kv数据, 如果值为String  则去掉其多余的空格, 然后在更新obj中对应key的值
     * 否则如果 值为JSONObject, 递归
     * 否则如果 值为JSONArray, trimSpaces(JSONArray )
     *
     * @param obj 给定的JSONObject
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:18 PM
     * @since 1.0
     */
    public static void trimSpaces(JSONObject obj) {
        if (obj.isNull() || Tools.isEmpty(obj)) {
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

    /**
     * 去掉掉arr中所有的字符串类的值的相邻的多个空格
     * 思路 : 如果arr是空对象  则直接返回
     * 否则 遍历各个数据, 如果值为String  则去掉其多余的空格, 然后在更新obj中对应key的值
     * 否则如果 值为JSONObject, trimSpaces(JSONObject )
     * 否则如果 值为JSONArray, 递归
     *
     * @param arr 给定的JSONArray
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:19 PM
     * @since 1.0
     */
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

    /**
     * 确保arr中的每一个JSONObject都存在指定的key, 否则  则删除该条目
     * val.toString可以确保值为null的情形
     *
     * @param arr 给定的JSONArray
     * @param key 需要确保的key
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:20 PM
     * @since 1.0
     */
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

    /**
     * 去掉掉obj中所有的null属性
     * 思路 : 如果obj是空对象  则直接返回
     * 否则 遍历各个kv数据, 如果值为String  如果只为空  则去掉当前kv对
     * 否则如果 值为JSONObject, 递归,  如果该方法之后, val为空, 则移除val对应的条目
     * 否则如果 值为JSONArray, removeIfNull(JSONArray ),  如果该方法之后, val为空, 则移除val对应的条目
     *
     * @param obj 给定的JSONObject
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:21 PM
     * @since 1.0
     */
    public static void removeIfNull(JSONObject obj) {
        if (obj.isNull() || Tools.isEmpty(obj)) {
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

    /**
     * 去掉掉arr中所有的null属性
     * 思路 : 如果arr是空对象  则直接返回
     * 否则 遍历各个数据, 如果值为String  则去掉其多余的空格, 然后在更新obj中对应key的值
     * 否则如果 值为JSONObject, removeIfNull(JSONObject ), 如果该方法之后, val为空, 则移除val对应的条目
     * 注意 : 因为这里对JSONArray进行了删除操作, 所以这里使用了Iterator
     *
     * @param arr 给定的JSONArray
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:21 PM
     * @since 1.0
     */
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
     * 从obj中移除needBeFiltered包含的所有的key
     *
     * @param obj            JSON对象
     * @param needBeFiltered 需要过滤掉的key的集合
     * @return com.hx.json.JSONObject
     * @author Jerry.X.He
     * @date 5/5/2017 4:22 PM
     * @since 1.0
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
     * 根据给定的pattern生成IdxIterator
     *
     * @param pattern 给定的pattern
     * @param len     需要提取的JSONArray的长度
     * @return com.hx.common.interf.idx.IdxIterator
     * @author Jerry.X.He
     * @date 5/5/2017 4:23 PM
     * @since 1.0
     */
    public static IdxIterator getIdxIteratorByPattern(String pattern, int len) {
        return JSONExtractor.getIdxIteratorByPattern(pattern, len);
    }

    /**
     * 根据给定的pattern从给定的json中提取数据
     *
     * @param json    给定的json对象
     * @param pattern 需要提取数据的pattern
     * @return com.hx.json.JSONArray
     * @author Jerry.X.He
     * @date 5/5/2017 4:23 PM
     * @since 1.0
     */
    public static JSONArray extractInfoFromJSON(JSON json, String pattern) {
        return JSONExtractor.extractInfoFromJSON(json, pattern);
    }

    /**
     * 确保obj中给定的必需属性存在, 并且合法
     *
     * @param obj           给定的JSONObject
     * @param requiredAttrs 必须要存在的属性
     * @param checkFunc     校验必需属性的函数
     * @return boolean return true if all of 'requiredAttrs' in 'obj' does exists and valid, or else
     * @author 970655147 created at 2017-03-19 18:43
     */
    public static boolean checkJSONObj(JSONObject obj, Collection<String> requiredAttrs, CheckRequiredAttributeFunc checkFunc) {
        assert0(requiredAttrs != null, "'requiredAttrs' can't be null !");
        if (Tools.isEmpty(obj)) {
            return false;
        }

        for (String required : requiredAttrs) {
            if (!obj.containsKey(required)) {
                return false;
            }
            if (checkFunc != null) {
                boolean valid = checkFunc.check(obj, required);
                if (!valid) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * 确保arr中每一个元素的给定的必需属性存在, 并且合法
     *
     * @param arr           给定的JSONArray
     * @param requiredAttrs 必须要存在的属性
     * @param checkFunc     校验必需属性的函数
     * @return boolean return true if all of 'requiredAttrs' in 'obj' does exists and valid, or else
     * @author 970655147 created at 2017-03-19 18:43
     */
    public static boolean checkJSONArr(JSONArray arr, Collection<String> requiredAttrs, CheckRequiredAttributeFunc checkFunc) {
        assert0(requiredAttrs != null, "'requiredAttrs' can't be null !");
        if (Tools.isEmpty(arr)) {
            return false;
        }

        for (int i = 0, len = arr.size(); i < len; i++) {
            JSONObject obj = arr.getJSONObject(i);
            if (obj == null) {
                return false;
            }
            boolean valid = checkJSONObj(obj, requiredAttrs, checkFunc);
            if (!valid) {
                return false;
            }
        }

        return true;
    }

    /**
     * 从spec中获取需要的数据
     * 注意 : 必需确保spec中每一个对象为JSONObject, name为spec的数据中需要检测的值, value为spec的数据中需要获取的值,
     * getInSpec存放获取数据的键的(key[src源对象] -> key[dst目标对象])映射
     * <p>
     * demo 01 .
     * <p>
     * arr : [{"name":"hx","age":21},{"name":"zhangsan","age":33}]
     * result : { }
     * nameKeyInArr : 'name', valueKeyInArr : 'age'
     * key2NewKey : {"hx":"hxAlias","zhangsan'sName":"zhangsan'sName"}
     * ----------------------------extractValueAsKv-------------------------
     * result : {"hxAlias":21, "zhangsan":33}
     * </p>
     * <p>
     * demo 02 .
     * [
     * ...												{
     * {												...
     * "value":" #F3F07AAR#ABA",		=>	    		"model":"#F3F07AAR#ABA"
     * "name":"Model"								    ...
     * }                                                }
     * ...
     * ]
     *
     * @param arr           给定的数组, 每一个元素为JSONObject
     * @param result        接收数据的JSONObject
     * @param nameKeyInArr  从spec的子数组中提取name作为key[getInSpec]
     * @param valueKeyInArr 从spec的子数组中提取value作为value
     * @param key2NewKey    需要提取的 key -> newKey的映射
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 10:34 PM
     * @since 1.0
     */
    public static void extractValueAsKv(JSONArray arr, JSONObject result, String nameKeyInArr, String valueKeyInArr,
                                        Map<String, String> key2NewKey) {
        if ((Tools.isEmpty(arr)) || (Tools.isEmpty(result)) || Tools.isEmpty(key2NewKey)) {
            return;
        }

        Iterator<?> it = arr.iterator();
        while (it.hasNext()) {
            JSONObject val = (JSONObject) it.next();
            if (val != null) {
                String key = val.getString(nameKeyInArr);
                if (key2NewKey.containsKey(key)) {
                    result.put(key2NewKey.get(key), val.get(valueKeyInArr));
                }
            }
        }
    }

    public static void extractValueAsKv(JSONArray arr, JSONObject result, String nameKeyInArr, String valueKeyInArr) {
        if ((Tools.isEmpty(arr)) || (Tools.isEmpty(result))) {
            return;
        }

        Iterator<?> it = arr.iterator();
        while (it.hasNext()) {
            JSONObject val = (JSONObject) it.next();
            if (val != null) {
                String key = val.getString(nameKeyInArr);
                result.put(key, val.get(valueKeyInArr));
            }
        }
    }

}
